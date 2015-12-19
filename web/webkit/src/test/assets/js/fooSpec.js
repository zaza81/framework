var assert = require("assert");
describe("foo", function() {
  it("say hello", function() {
    var foo = require("./foo");
    assert.equal(foo.hello("world"), "hello world");
  });
});
