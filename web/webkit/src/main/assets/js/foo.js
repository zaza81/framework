// http://ifandelse.com/its-not-hard-making-your-library-support-amd-and-commonjs/
(function (root, factory) {
  if(typeof define === "function" && define.amd) {
    define([], function() {
      return (root.foo = factory());
    });
  } else if(typeof module === "object" && module.exports) {
    module.exports = factory();
  } else {
    root.foo = factory();
  }
}(this, function() {
  var x = 2;
  if (x === 1) {
    console.log(x);
  }

  var foo = {
    hello: function(msg) {
      return "hello " + msg;
    }
  };
  return foo;
}));
