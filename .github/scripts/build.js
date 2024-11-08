const { execSync } = require('child_process');
const arch = process.env.MATRIX_ARCH;
const os = process.env.MATRIX_OS;
const rustTarget = process.env.MATRIX_RUST_TARGET;

if (arch === 'arm64' && os === 'ubuntu-latest') {
  console.log('Using cross to build...');
  execSync('cross build -p jsonui_lsp --verbose --release --target=' + rustTarget, { stdio: 'inherit' });
} else {
  console.log('Using cargo to build...');
  execSync('rustup set auto-self-update disable', { stdio: 'inherit' });
  execSync('rustup target add ' + rustTarget, { stdio: 'inherit' });
  execSync('cargo build -p jsonui_lsp --verbose --release --target=' + rustTarget, { stdio: 'inherit' });
}