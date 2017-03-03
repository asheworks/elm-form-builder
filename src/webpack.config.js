/* global process */

var AutoPrefixer = require('autoprefixer');
var CopyWebpackPlugin = require('copy-webpack-plugin');
var ExtractTextPlugin = require('extract-text-webpack-plugin');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var Webpack = require('webpack');
var WebpackMerge = require('webpack-merge');

var common = {
    entry: {
        app: './app.js'
    },

    resolve: {
        modulesDirectories: ['node_modules'],
        extensions: ['', '.js', '.elm']
    },

    module: {
        loaders: [
          {
            test: /\.json$/,
            loader: 'json'
          }, {
            test: /\.(jpg|eot|svg|ttf|woff|woff2)(\?v=\d+\.\d+\.\d+)?/,
            loader: 'file-loader'
          }, {
            test: /\.(scss)$/,
            loaders: [
              'style-loader',
              'css-loader',
              'postcss-loader',
              'sass-loader'
            ]
          }
        ],

        noParse: /^(?!.*Stylesheets).*\.elm$/
    },

    plugins: [
        new HtmlWebpackPlugin({
            template: 'index.tpl.html'
        }),
        new Webpack.optimize.CommonsChunkPlugin({
            name: "init",
            minChunks: Infinity
        }),
        new Webpack.optimize.OccurenceOrderPlugin()
    ],

    postcss: [AutoPrefixer({
        browsers: ['last 2 versions']
    })],

    target: 'web'
};

var extractCssVendor = null;

console.log('running development');
extractCssVendor = new ExtractTextPlugin('vendor.css');

var devOnly = {
    output: {
        filename: '[name].js'
    },

    module: {
        loaders: [
            {
                test: /Stylesheets.elm$/,
                loaders: [
                    'style-loader',
                    'css-loader',
                    'postcss-loader',
                    'elm-css-webpack-loader'
                ]
            },

            {
                test: /\.elm$/,
                exclude: [
                    /elm-stuff/,
                    /node_modules/,
                    /Stylesheets.elm$/
                ],
                loaders: [
                    'elm-hot-loader',
                    'elm-webpack-loader'
                ]
            },

            {
                test: /\.css$/,
                loader: extractCssVendor.extract('style-loader', 'css-loader')
            }
        ]
    },

    plugins: [
        extractCssVendor
    ],

    devServer: {
        contentBase: '',
        devtool: 'eval',
        stats: 'errors-only',
        hot: true,
        inline: true,
        progress: true,
        historyApiFallback: true,
        port: 8000,
        host: 'localhost',
        proxy: {

        }
    }
};

module.exports = WebpackMerge(common, devOnly);