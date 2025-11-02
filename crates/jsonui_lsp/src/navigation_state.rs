/// Navigation state management for goto definition operations
///
/// When the user invokes goto_definition to a symbol in another file,
/// VSCode automatically:
/// 1. Opens the target file (did_open)
/// 2. Closes the source file (did_close)
///
/// We need to ignore these automated lifecycle events since they're part
/// of the navigation flow, not user-initiated file management.

use std::sync::atomic::{AtomicU8, Ordering};

/// States during a goto definition navigation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum State {
    /// Normal operation - no navigation in progress
    Idle = 0,
    /// Navigation initiated - expecting automated target file open
    AwaitingTargetOpen = 1,
    /// Target opened - expecting automated source file close
    AwaitingSourceClose = 2,
}

impl State {
    fn from_u8(value: u8) -> Self {
        match value {
            1 => Self::AwaitingTargetOpen,
            2 => Self::AwaitingSourceClose,
            _ => Self::Idle,
        }
    }
}

/// Manages navigation state to filter automated file lifecycle events
pub struct NavigationStateManager {
    state: AtomicU8,
}

impl NavigationStateManager {
    /// Create a new navigation state manager in idle state
    pub fn new() -> Self {
        Self {
            state: AtomicU8::new(State::Idle as u8),
        }
    }

    /// Start a navigation sequence (called by goto_definition)
    ///
    /// Transitions: Idle -> AwaitingTargetOpen
    pub fn start_navigation(&self) {
        self.state.store(State::AwaitingTargetOpen as u8, Ordering::SeqCst);
        tracing::trace!("Navigation started - awaiting target file open");
    }

    /// Check if file open event should be ignored
    ///
    /// Returns true if this is an automated open from navigation.
    /// Transitions: AwaitingTargetOpen -> AwaitingSourceClose
    pub fn should_ignore_open(&self) -> bool {
        let current = State::from_u8(self.state.load(Ordering::SeqCst));
        if current == State::AwaitingTargetOpen {
            self.state.store(State::AwaitingSourceClose as u8, Ordering::SeqCst);
            tracing::trace!("Ignored automated file open during navigation");
            true
        } else {
            false
        }
    }

    /// Check if file close event should be ignored
    ///
    /// Returns true if this is an automated close from navigation.
    /// Transitions: AwaitingSourceClose -> Idle
    pub fn should_ignore_close(&self) -> bool {
        let current = State::from_u8(self.state.load(Ordering::SeqCst));
        if current == State::AwaitingSourceClose {
            self.state.store(State::Idle as u8, Ordering::SeqCst);
            tracing::trace!("Ignored automated file close during navigation - navigation complete");
            true
        } else {
            false
        }
    }

    /// Reset to idle state (for error recovery)
    pub fn reset(&self) {
        self.state.store(State::Idle as u8, Ordering::SeqCst);
        tracing::trace!("Navigation state reset to idle");
    }

    #[cfg(test)]
    fn get_state(&self) -> State {
        State::from_u8(self.state.load(Ordering::SeqCst))
    }
}

impl Default for NavigationStateManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_navigation_flow() {
        let manager = NavigationStateManager::new();
        assert_eq!(manager.get_state(), State::Idle);

        // Start navigation
        manager.start_navigation();
        assert_eq!(manager.get_state(), State::AwaitingTargetOpen);

        // Normal open should not be ignored
        let other_manager = NavigationStateManager::new();
        assert!(!other_manager.should_ignore_open());

        // Automated target open should be ignored
        assert!(manager.should_ignore_open());
        assert_eq!(manager.get_state(), State::AwaitingSourceClose);

        // Normal close should not be ignored
        assert!(!other_manager.should_ignore_close());

        // Automated source close should be ignored
        assert!(manager.should_ignore_close());
        assert_eq!(manager.get_state(), State::Idle);
    }

    #[test]
    fn test_reset() {
        let manager = NavigationStateManager::new();
        manager.start_navigation();
        assert_eq!(manager.get_state(), State::AwaitingTargetOpen);

        manager.reset();
        assert_eq!(manager.get_state(), State::Idle);
    }

    #[test]
    fn test_ignore_only_during_navigation() {
        let manager = NavigationStateManager::new();

        // In idle state, nothing should be ignored
        assert!(!manager.should_ignore_open());
        assert!(!manager.should_ignore_close());
        assert_eq!(manager.get_state(), State::Idle);
    }
}
