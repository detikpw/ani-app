var path = require('path');
const HtmlWebPackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const TerserPlugin = require('terser-webpack-plugin');

module.exports = env => {
  const mode = env.production ? 'production' : 'development';
  return {
    mode,
    module: {
      rules: [
        {
          test: /\.(sa|sc|c)ss$/,
          use: [
            MiniCssExtractPlugin.loader,
            'css-loader',
            {
              loader: 'postcss-loader',
              options: { 
                config: { 
                  ctx: { mode } 
                } 
              }
            }
          ]
        },
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader: "elm-webpack-loader",
          options: {
            debug: !env.production,
            pathToElm: 'node_modules/.bin/elm'
          }
        },
      ]
    },
    plugins: [
      new MiniCssExtractPlugin({
        filename: "main.css",
        chunkFilename: "main.css"
      }),
      new HtmlWebPackPlugin({
        template: './src/index.html'
      }),
    ],
    optimization: {
      minimizer: [new TerserPlugin(), new OptimizeCSSAssetsPlugin()],
    },
    devServer: {
      inline: true,
      stats: 'errors-only',
      disableHostCheck: true, 
    }
  }
};