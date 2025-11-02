use arc_swap::ArcSwap;
use std::sync::Arc;
use tokio::sync::Mutex as TokioMutex;
use tracing_subscriber::reload::Handle;
use tracing_subscriber::EnvFilter;

#[derive(Clone)]
pub struct Config {
    pub log_level: Arc<str>,
    pub lang: Arc<str>,
    pub append_suffix: bool,
}

impl Config {
    pub fn new(log_level: impl Into<Arc<str>>, lang: impl Into<Arc<str>>, append_suffix: bool) -> Self {
        Self {
            log_level: log_level.into(),
            lang: lang.into(),
            append_suffix,
        }
    }
}

pub type LogReloadHandle = Handle<EnvFilter, tracing_subscriber::Registry>;

pub struct ConfigManager {
    inner: ArcSwap<Config>,
    log_reload_handle: TokioMutex<Option<LogReloadHandle>>,
}

impl ConfigManager {
    pub fn new(config: Config) -> Self {
        Self {
            inner: ArcSwap::from_pointee(config),
            log_reload_handle: TokioMutex::new(None),
        }
    }

    /// Set the log reload handle (should be called once during initialization)
    pub async fn set_log_reload_handle(&self, handle: LogReloadHandle) {
        *self.log_reload_handle.lock().await = Some(handle);
    }

    /// Get current configuration (lock-free, always succeeds)
    ///
    /// This method never blocks, making it safe to call from hot paths
    /// like completion or hover handlers.
    pub fn get(&self) -> Arc<Config> {
        self.inner.load_full()
    }

    /// Update configuration atomically
    ///
    /// All readers will see either the old config or the new config,
    /// never a partially updated state.
    pub fn update(&self, config: Config) {
        self.inner.store(Arc::new(config));
    }

    /// Update configuration using a transformation function
    ///
    /// This is useful when you need to modify config based on its current values.
    pub fn update_with<F>(&self, f: F)
    where
        F: FnOnce(&Config) -> Config,
    {
        let old = self.get();
        let new = f(&old);
        self.update(new);
    }

    /// Update configuration and apply log level changes dynamically
    ///
    /// This method updates the configuration and also reloads the tracing subscriber
    /// with the new log level if it has changed.
    pub async fn update_with_log_reload<F>(&self, f: F)
    where
        F: FnOnce(&Config) -> Config,
    {
        let old = self.get();
        let new = f(&old);

        // Check if log level changed
        if old.log_level != new.log_level {
            // Update the tracing subscriber with new log level
            if let Some(handle) = self.log_reload_handle.lock().await.as_ref() {
                // Only set log level for jsonui_lsp crate, keep other crates at warn level
                let new_filter = EnvFilter::new("error")
                    .add_directive(format!("jsonui_lsp={}", new.log_level.as_ref()).parse().unwrap());
                if let Err(e) = handle.reload(new_filter) {
                    eprintln!("Failed to reload log level: {}", e);
                }
            }
        }

        self.update(new);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_manager_get() {
        let config = Config::new("info", "en", false);
        let manager = ConfigManager::new(config);

        let retrieved = manager.get();
        assert_eq!(retrieved.log_level.as_ref(), "info");
        assert_eq!(retrieved.lang.as_ref(), "en");
        assert!(!retrieved.append_suffix);
    }

    #[test]
    fn test_config_manager_update() {
        let initial = Config::new("info", "en", false);
        let manager = ConfigManager::new(initial);

        // Update config
        let new_config = Config::new("debug", "zh", true);
        manager.update(new_config);

        let retrieved = manager.get();
        assert_eq!(retrieved.log_level.as_ref(), "debug");
        assert_eq!(retrieved.lang.as_ref(), "zh");
        assert!(retrieved.append_suffix);
    }

    #[test]
    fn test_config_manager_update_with() {
        let initial = Config::new("info", "en", false);
        let manager = ConfigManager::new(initial);

        // Update using transformation function
        manager.update_with(|old| {
            let mut new_config = old.clone();
            new_config.log_level = "trace".into();
            new_config.lang = "zh".into();
            new_config.append_suffix = !old.append_suffix;
            new_config
        });

        let retrieved = manager.get();
        assert_eq!(retrieved.log_level.as_ref(), "trace");
        assert_eq!(retrieved.lang.as_ref(), "zh");
        assert!(retrieved.append_suffix);
    }

    #[test]
    fn test_config_manager_concurrent_access() {
        use std::thread;

        let config = Config::new("info", "en", false);
        let manager = Arc::new(ConfigManager::new(config));

        // Spawn multiple reader threads
        let handles: Vec<_> = (0..10)
            .map(|_| {
                let m = manager.clone();
                thread::spawn(move || {
                    for _ in 0..100 {
                        let _ = m.get();
                    }
                })
            })
            .collect();

        // All readers should complete without blocking
        for handle in handles {
            handle.join().unwrap();
        }
    }
}
