var Parse = require('./libs/Parse');

var po = {
    "start" : "this ${is} a $1{test} $2{test} $2{#pop} $2{test} $2{test}",
    "is" : 'was',
    "test" : "${#push}${#=test:blaaaah}something"
};

console.log(Parse([po],'start'));
