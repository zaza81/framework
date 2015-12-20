var expect = require('chai').expect;

describe("liftVanilla", function() {
  var liftVanilla = require("./liftVanilla");

  it("init is a function", function() {
    expect(typeof liftVanilla.init).to.equal("function");
  });

  // var xhr, requests;
  // var sinon = require("sinon");

  // before(function () {
  //   xhr = sinon.useFakeXMLHttpRequest();
  //   requests = [];
  //   xhr.onCreate = function (req) { requests.push(req); };
  // });

  // after(function () {
  //   // Like before we must clean up when tampering with globals.
  //   xhr.restore();
  // });

  // it("makes a GET request for todo items", function () {
  //   getTodos(42, sinon.spy());

  //   assert.equals(requests.length, 1);
  //   assert.match(requests[0].url, "/todo/42/items");
  // });

  // it("load it", function() {
  //   // var lift = require("lift");
  //   var lv = require("./liftVanilla");
  //   assert.equal(typeof lv.init, "function");
  //   assert.equal(typeof lv.defaultSettings.ajaxGet, "function");
  //   var jq = require("../lib/jquery/dist/jquery");
  // });
});

