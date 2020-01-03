const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');

const config = {
    entry : './app/index.js',
    output : {
        filename : 'bundle.js',
        path: './dist'
    },
    module : {
        loaders : [
            { test: /\.js$/, exclude: /node_modules/, loader: "babel-loader" }
            ]
    },
    plugins : [
        new webpack.optimize.UglifyJsPlugin(),
        new HtmlWebpackPlugin({template: './index.html'})
    ]
}


module.exports = config;
