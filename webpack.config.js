"use strict";
const path = require("path");

/** @type {import('webpack').Configuration} */
const config = {
  target: "node",
  entry: "./client/src/extension.ts",
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "extension.js",
    libraryTarget: "commonjs2",
    devtoolModuleFilenameTemplate: "../[resource-path]",
  },
  devtool: "source-map",
  externals: {
    vscode: "commonjs vscode",
    'vscode-languageclient': 'commonjs vscode-languageclient',
    'vscode-languageserver': 'commonjs vscode-languageserver',
    'vscode-languageserver-protocol': 'commonjs vscode-languageserver-protocol',
    'vscode-languageserver-types': 'commonjs vscode-languageserver-types'
  },
  resolve: {
    extensions: [".ts", ".js"],
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        exclude: /node_modules/,
        use: [
          {
            loader: "ts-loader",
          },
        ],
      },
    ],
  },
  cache: {
    type: 'filesystem',
  },
};
module.exports = config;