var expect = require('chai').expect;
//var jsdom = require('jsdom');

describe("liftJQuery", function() {
  var liftJQuery = require("./liftJQuery");

  it("init is a function", function() {
    expect(typeof liftJQuery.init).to.equal("function");
  });
});

