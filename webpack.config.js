let CopyPlugin = require("copy-webpack-plugin");
let path = require("path");

module.exports = {
  entry: {
    main: "./js/main.js"
  },
  output: {
    filename: "[name].js",
    path: __dirname + "/public"
  },
  devServer: {
    contentBase: path.join(__dirname, "public"),
    compress: true,
    port: 8080,
    host: "0.0.0.0"
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader"
        }
      },
      {
        test: /\.scss$/,
        use: ["style-loader", "css-loader", "sass-loader"]
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: "elm-webpack-loader",
          options: { debug: true }
        }
      }
    ]
  },
  plugins: [new CopyPlugin([{ from: "static" }])]
};
