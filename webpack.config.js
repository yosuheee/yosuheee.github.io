const HtmlWebpackPlugin = require('html-webpack-plugin');
const path = require("path");
const webpack = require("webpack");

module.exports = {
  mode: "production",
  entry: {
    // dice: "./src/app/dice/index.tsx",
    // "invert-matrix4x4": "./src/app/invert-matrix4x4/index.tsx",
    test: "./src/app/test/index.ts",
  },
  plugins: [
    new webpack.ProvidePlugin({
      // glMatrix: "gl-matrix",
      Vue: "vue",
    }),
    // new HtmlWebpackPlugin({
    //   title: "dice",
    //   filename: "app/dice/index.html",
    //   chunks: ["dice"],
    //   template: "./src/assets/index.html",
    // }),
    // new HtmlWebpackPlugin({
    //   title: "逆行列を求めるプログラム",
    //   filename: "app/invert-matrix4x4/index.html",
    //   chunks: ["invert-matrix4x4"],
    //   template: "./src/assets/index.html",
    // }),
    new HtmlWebpackPlugin({
      title: "test",
      filename: "app/test/index.html",
      chunks: ["test"],
      template: "./src/assets/test.html",
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
    "vue": "Vue",
  },
  resolve: {
    extensions: [ ".tsx", ".ts", ".jsx", ".js" ],
  },
};
