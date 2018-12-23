var fs = require('fs');
var express = require('express');
var app = express();
var path = require('path');
app.use('/', express.static(path.join(__dirname, '/')));
app.get('/', function (req, res) {
    console.log('Index page')
    res.sendFile(path.join(public, 'index.html'));
});
app.use('/', require('./service'));
module.exports.app = app;
app.listen(3000);
console.log('Server is up!');



