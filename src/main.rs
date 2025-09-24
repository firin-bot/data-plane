mod graph;

use anyhow::Context as _;
use anyhow::Result;
use futures_util::StreamExt as _;
use rand::Rng as _;
use tokio::time::Duration;
use tokio::time::Instant;
use tokio_tungstenite::tungstenite::client::IntoClientRequest as _;
use twitch_api::eventsub::channel::ChannelChatMessageV1Payload;
use twitch_api::eventsub::Event;
use twitch_api::eventsub::EventsubWebsocketData;
use twitch_api::eventsub::Message;
use twitch_api::TwitchClient;
use twitch_api::twitch_oauth2::AppAccessToken;
use twitch_api::types::UserId;

use petgraph::dot::Dot;

struct DataState<'a> {
    client: TwitchClient<'a, reqwest::Client>,
    control_host: String,
    control_port: u16,
    control_token: String,
    app_token: Option<AppAccessToken>,
    my_id: Option<UserId>
}

fn jitter(d: Duration, percent: f64) -> Duration {
    let factor = rand::rng().random_range(0.0..percent);
    d.mul_f64(1.0 + factor)
}

#[tokio::main]
async fn main() -> Result<()> {
    env_logger::init();
    dotenvy::dotenv().ok();



    // graph stuff

    let mut ctx = graph::Context::default();

    let constant = graph::Op::Constant(graph::Value::Character('H'));
    let identity = graph::Op::Identity;

    let mut g = graph::Graph::default();
    let n0 = g.add(constant.instantiate(&mut ctx));
    let n1 = g.add(identity.instantiate(&mut ctx));
    let n2 = g.add(identity.instantiate(&mut ctx));
    g.connect(n0, 0, n1, 0);
    g.connect(n1, 0, n2, 0);

    let i0 = g.add(identity.instantiate(&mut ctx));
    let i1 = g.add(identity.instantiate(&mut ctx));
    //let i2 = g.add(identity.instantiate(&mut ctx));
    g.connect(i0, 0, i1, 0);
    //g.connect(i1, 0, i2, 0);

    log::info!("{:?}", Dot::new(&g.0));
    g.type_check()?;
    log::info!("{:?}", Dot::new(&g.0));

    return Ok(());



    let control_host  = std::env::var("CONTROL_HOST" ).context("missing CONTROL_HOST")?;
    let control_port  = std::env::var("CONTROL_PORT" ).context("missing CONTROL_PORT")?.parse::<u16>()?;
    let control_token = std::env::var("CONTROL_TOKEN").context("missing CONTROL_TOKEN")?;

    const MIN_BACKOFF: Duration = Duration::from_secs(1);
    const MAX_BACKOFF: Duration = Duration::from_secs(30);
    const GOOD_SESSION: Duration = Duration::from_secs(30);

    let request = "wss://eventsub.wss.twitch.tv/ws".into_client_request()?;
    let mut backoff = MIN_BACKOFF;

    let mut data_state = DataState {
        client: TwitchClient::default(),
        control_host,
        control_port,
        control_token,
        app_token: None,
        my_id: None
    };

    loop {
        let started = Instant::now();

        let mut stream = match tokio_tungstenite::connect_async(request.clone()).await {
            Ok((stream, _)) => stream,
            Err(e) => {
                let b = jitter(backoff, 0.5);
                log::error!("connect_async failed: {e}; retrying in {} seconds", b.as_secs());
                tokio::time::sleep(b).await;
                backoff = (backoff * 2).min(MAX_BACKOFF);
                continue
            }
        };

        while let Some(Ok(msg)) = stream.next().await {
            if let Err(e) = handle_message(&mut data_state, msg).await {
                log::error!("{e}");
                break
            }
        }

        let uptime = started.elapsed();
        if uptime >= GOOD_SESSION {
            backoff = MIN_BACKOFF;
        } else {
            backoff = (backoff * 2).min(MAX_BACKOFF);
        }

        let b = jitter(backoff, 0.5);
        log::error!("disconnected; reconnecting in {} seconds", b.as_secs());
        tokio::time::sleep(b).await;
    }
}

async fn handle_message(data_state: &mut DataState<'_>, msg: tungstenite::Message) -> Result<()> {
    match msg {
        tungstenite::Message::Text(text) => {
            match Event::parse_websocket(&text)? {
                EventsubWebsocketData::Welcome {
                    payload,
                    ..
                } => {
                    log::info!("websocket session ID = {:#?}", payload.session.id);
                    let client = reqwest::Client::new();
                    let url = reqwest::Url::parse(&format!(
                        "http://{}:{}/session/assign",
                        data_state.control_host,
                        data_state.control_port
                    ))?;
                    let (client_id, client_secret, my_id) = client
                        .post(url)
                        .bearer_auth(data_state.control_token.clone())
                        .body(payload.session.id.as_ref().to_owned())
                        .send().await?
                        .error_for_status()?
                        .json().await?;
                    data_state.app_token = Some(AppAccessToken::get_app_access_token(
                        &data_state.client,
                        client_id,
                        client_secret,
                        vec![]
                    ).await?);
                    data_state.my_id = my_id;
                    log::info!("{:?}", data_state.app_token);
                },
                EventsubWebsocketData::Notification {
                    payload,
                    ..
                } => match payload {
                    Event::ChannelChatMessageV1(payload) => {
                        if let Message::Notification(payload) = payload.message {
                            if let Some(my_id) = &data_state.my_id && payload.chatter_user_id != my_id {
                                log::info!(
                                    "channel chat message in #{} from {}: {:?}",
                                    payload.broadcaster_user_login,
                                    payload.chatter_user_login,
                                    payload.message
                                );
                                if let Some(cmd) = parse_command(&payload) {
                                    handle_command(data_state, cmd).await?;
                                }
                            }
                        }
                    },
                    _ => {
                        log::info!("unhandled notification payload: {payload:#?}");
                    }
                }
                _ => {}
            }
            Ok(())
        },
        tungstenite::Message::Close(_) => {
            anyhow::bail!("connection closed by server")
        },
        _ => Ok(())
    }
}

#[derive(Debug)]
struct Command<'a> {
    broadcaster_id: UserId,
    name: &'a str,
    rest: &'a str
}

fn parse_command(payload: &ChannelChatMessageV1Payload) -> Option<Command<'_>> {
    if !payload.message.text.starts_with('!') {
        None
    } else {
        #[allow(clippy::string_slice)]
        let trimmed = &payload.message.text[1..];
        let mut parts = trimmed.splitn(2, ' ');
        let name = parts.next()?;
        let rest = parts.next().unwrap_or("");
        Some(Command {
            broadcaster_id: payload.broadcaster_user_id.clone(),
            name,
            rest
        })
    }
}

async fn handle_command(data_state: &DataState<'_>, cmd: Command<'_>) -> Result<()> {
    log::info!("{cmd:?}");
    data_state.client.helix.send_chat_message(
        cmd.broadcaster_id,
        data_state.my_id.clone().expect("my_id should be set"),
        "hai",
        &data_state.app_token.clone().expect("access_token should be set")
    ).await?;
    Ok(())
}
