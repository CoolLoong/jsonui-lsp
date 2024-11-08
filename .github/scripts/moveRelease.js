const fs = require('fs');
const path = require('path');

const targetDir = process.env.TARGET;
const sourcePaths = [
  'target/' + targetDir + '/release/jsonui_lsp',
  'target/' + targetDir + '/release/jsonui_lsp.exe'
];
const destDir = 'dist';

sourcePaths.forEach(sourcePath => {
  const destPath = path.join(destDir, path.basename(sourcePath));

  if (fs.existsSync(sourcePath)) {
    fs.renameSync(sourcePath, destPath);
    console.log('File', path.basename(sourcePath), 'moved successfully to', destPath);
  } else {
    console.log('File', path.basename(sourcePath), 'not found at', sourcePath);
  }
});
