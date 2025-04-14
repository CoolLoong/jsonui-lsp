use std::sync::{Arc, OnceLock};

use lasso::{Spur, ThreadedRodeo};

pub(crate) struct StringPool {
    inner: ThreadedRodeo,
}
static INSTANCE: OnceLock<StringPool> = OnceLock::new();

impl StringPool {
    pub(crate) fn global() -> &'static StringPool {
        INSTANCE.get_or_init(|| StringPool {
            inner: ThreadedRodeo::default(),
        })
    }

    pub(crate) fn get_or_intern(&self, s: &str) -> Spur {
        self.inner.get_or_intern(s)
    }

    pub(crate) fn resolve(&self, spur: &Spur) -> &str {
        self.inner.resolve(spur)
    }

    pub(crate) fn resolve_to_arc(&self, spur: &Spur) -> Arc<str> {
        Arc::from(self.inner.resolve(spur))
    }
}
