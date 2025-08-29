use anyhow::{Context, Result};
use futures_util::StreamExt as _;
use rand::Rng as _;
use tokio::time::{Duration, Instant};
use tokio_tungstenite::tungstenite::client::IntoClientRequest as _;
use twitch_api::eventsub::{
    Event,
    EventsubWebsocketData
};

struct DataState {
    control_host: String,
    control_port: u16,
    control_token: String
}

fn jitter(d: Duration, percent: f64) -> Duration {
    let factor = rand::rng().random_range(0.0..percent);
    d.mul_f64(1.0 + factor)
}

#[tokio::main]
async fn main() -> Result<()> {
    env_logger::init();
    dotenvy::dotenv().ok();

    let control_host  = std::env::var("CONTROL_HOST" ).context("missing CONTROL_HOST")?;
    let control_port  = std::env::var("CONTROL_PORT" ).context("missing CONTROL_PORT")?.parse::<u16>()?;
    let control_token = std::env::var("CONTROL_TOKEN").context("missing CONTROL_TOKEN")?;

    const MIN_BACKOFF: Duration = Duration::from_secs(1);
    const MAX_BACKOFF: Duration = Duration::from_secs(30);
    const GOOD_SESSION: Duration = Duration::from_secs(30);

    let request = "wss://eventsub.wss.twitch.tv/ws".into_client_request()?;
    let mut backoff = MIN_BACKOFF;

    let data_state = DataState {
        control_host,
        control_port,
        control_token
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
            if let Err(e) = handle_message(&data_state, msg).await {
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

async fn handle_message(data_state: &DataState, msg: tungstenite::Message) -> Result<()> {
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
                    let res = client
                        .post(url)
                        .bearer_auth(data_state.control_token.clone())
                        .body(payload.session.id.as_ref().to_owned())
                        .send().await?;
                    log::info!("{res:?}");
                },
                EventsubWebsocketData::Notification {
                    payload,
                    ..
                } => {
                    log::info!("{payload:#?}");
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
