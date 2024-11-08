const commitMessage = process.env.GITHUB_EVENT_HEAD_COMMIT_MESSAGE;
const target = process.env.TARGET;

const execSync = require('child_process').execSync;
if (commitMessage.includes('-pre')) {
    console.log("PreRelease jsonui_lsp " + target)
    execSync('npx vsce package --pre-release --target ' + target + ' --no-dependencies', {stdio: 'inherit'});
} else {
    console.log("Release jsonui_lsp " + target)
    execSync('npx vsce package --target ' + target + ' --no-dependencies', {stdio: 'inherit'});
}
