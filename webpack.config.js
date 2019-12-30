const HtmlWebpackPlugin = require('html-webpack-plugin');
const path = require("path");

module.exports = {
  mode: "production",
  entry: {
    dice: "./src/app/dice/index.tsx",
  },
  plugins: [
    new HtmlWebpackPlugin({
      title: "dice",
      filename: "app/dice/index.html",
      chunks: ["dice"],
      template: "./src/assets/index.html",
    }),
  ],
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "[name].bundle.js",
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: "ts-loader",
        exclude: /node_modules/,
      },
    ],
  },
  externals: {
    react: "React",
    "react-dom": "ReactDOM",
  },
  resolve: {
    extensions: [ ".tsx", ".ts", ".jsx", ".js" ],
  },
};
