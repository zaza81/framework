var expect = require('chai').expect;

describe("foo", function() {
  it("say hello", function() {
    var foo = require("./foo");
    expect(foo.hello("world")).to.equal("hello world");
  });
});
