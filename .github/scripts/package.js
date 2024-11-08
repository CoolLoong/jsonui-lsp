const commitMessage = process.env.GITHUB_EVENT_HEAD_COMMIT_MESSAGE;
const target = process.env.TARGET;

const execSync = require('child_process').execSync;

if (commitMessage.includes('-pre')) {
  execSync('npx vsce package --pre-release --target ' + target, { stdio: 'inherit' });
} else {
  execSync('npx vsce package --target ' + target, { stdio: 'inherit' });
}
