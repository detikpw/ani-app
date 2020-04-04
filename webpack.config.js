var path = require('path');

module.exports = {
  module: {
    rules: [{
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file-loader?name=[name].[ext]'
      },
      {
        test: /\.css$/,
        exclude: /node_modules/,
        loader: 'file-loader?name=[name].[ext]'
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: "elm-webpack-loader",
        options: {
          debug: true
        }
      },
      {
        test: /\.css$/,
        use: [ 'postcss-loader' ]
      }
    ]
  },
  plugins: [
    require('tailwindcss'),
    require('autoprefixer'),
  ],
  devServer: {
    inline: true,
    stats: 'errors-only'
  }
};