use anyhow::Result;
use futures_util::StreamExt as _;
use rand::Rng as _;
use tokio::time::{Duration, Instant};
use tokio_tungstenite::tungstenite::client::IntoClientRequest as _;

fn jitter(d: Duration, percent: f64) -> Duration {
    let factor = rand::rng().random_range(0.0..percent);
    d.mul_f64(1.0 + factor)
}

#[tokio::main]
async fn main() -> Result<()> {
    env_logger::init();
    dotenvy::dotenv().ok();

    const MIN_BACKOFF: Duration = Duration::from_secs(1);
    const MAX_BACKOFF: Duration = Duration::from_secs(30);
    const GOOD_SESSION: Duration = Duration::from_secs(30);

    let request = "wss://eventsub.wss.twitch.tv/ws".into_client_request()?;
    let mut backoff = MIN_BACKOFF;

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

        while let Some(item) = stream.next().await {
            log::info!("{item:#?}");
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
