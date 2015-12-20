(function (root, factory) {
  if(typeof define === "function" && define.amd) {
    define(["./lift"], function(lift) {
      return (root.liftVanilla = factory(lift));
    });
  } else if(typeof module === "object" && module.exports) {
    module.exports = (root.liftVanilla = factory(require("./lift")));
  } else {
    root.liftVanilla = factory(root.lift);
  }
}(this, function(lift) {

  // The default settings used with vanilla JavaScript
  var defaultSettings = {
    // This and onDocumentReady adapted from https://github.com/dperini/ContentLoaded/blob/master/src/contentloaded.js,
    // as also used (with modifications) in jQuery.
    onEvent: function(elementOrId, eventName, fn) {
      var win = window,
          doc = win.document,
          add = doc.addEventListener ? 'addEventListener' : 'attachEvent',
          pre = doc.addEventListener ? '' : 'on';

      var element = elementOrId;
      if (typeof elementOrId === 'string') {
        element = document.getElementById(elementOrId);
      }

      element[add](pre + eventName, fn, false);
    },
    onDocumentReady: function(fn) {
      var done = false, top = true,
      win = window, doc = win.document, root = doc.documentElement,
      pre = doc.addEventListener ? '' : 'on',
      rem = doc.addEventListener ? 'removeEventListener' : 'detachEvent',

      init = function(e) {
        if (e.type === 'readystatechange' && doc.readyState !== 'complete') {
          return;
        }
        (e.type === 'load' ? win : doc)[rem](pre + e.type, init, false);
        if (!done && (done = true)) {
          fn.call(win, e.type || e);
        }
      },

      poll = function() {
        try { root.doScroll('left'); } catch(e) { setTimeout(poll, 50); return; }
        init('poll');
      };

      if (doc.readyState === 'complete') {
        fn.call(win, 'lazy');
      } else {
        if (doc.createEventObject && root.doScroll) {
            try { top = !win.frameElement; } catch(e) { }
            if (top) { poll(); }
        }
        defaultSettings.onEvent(doc, 'DOMContentLoaded', init);
        defaultSettings.onEvent(doc, 'readystatechange', init);
        defaultSettings.onEvent(win, 'load', init);
      }
    },
    ajaxPost: function(url, data, dataType, onSuccess, onFailure, onUploadProgress) {
      var settings = this;

      var xhr = new XMLHttpRequest();

      if (onUploadProgress) {
        xhr.upload.addEventListener("progress", onUploadProgress, false);
      }

      xhr.onreadystatechange = function() {
        if (xhr.readyState === 4) { // Done
          if (xhr.status === 200) {
            if (dataType === "script") {
              try {
                eval(xhr.responseText); // jshint ignore:line
              }
              catch (e) {
                settings.logError('The server call succeeded, but the returned Javascript contains an error: '+e);
              }
              finally {
                onSuccess();
              }
            }
            else if (dataType === "json") {
              var obj = {};
              try {
                obj = JSON.parse(xhr.responseText);
              }
              catch(e) {
                settings.logError('The server call succeeded, but the returned JSON contains an error: '+e);
              }
              finally {
                onSuccess(obj);
              }
            }
            else {
              settings.logError("Unknown data type: "+dataType);
            }
          }
          else {
            onFailure();
          }
        }
      };

      xhr.open("POST", url, true);
      xhr.timeout = settings.ajaxPostTimeout;
      xhr.setRequestHeader("X-Requested-With", "XMLHttpRequest");

      // set content-type header if the form has been serialized into a string
      if (typeof data === "string") {
        xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
      }

      // These just mimic what jQuery produces
      if (dataType === "script") {
        xhr.setRequestHeader("Accept", "text/javascript, application/javascript, application/ecmascript, application/x-ecmascript, */*; q=0.01");
      }
      else if (dataType === "json") {
        xhr.setRequestHeader("Accept", "application/json, text/javascript, */*; q=0.01");
      }
      xhr.send(data);
    },
    ajaxGet: function(url, data, onSuccess, onFailure) {
      var settings = this;

      // create query string
      var qs = "";
      for (var key in data) {
        if (qs !== "") {
          qs += "&";
        }
        qs += key + "=" + data[key];
      }

      var xhr = new XMLHttpRequest();

      xhr.onreadystatechange = function() {
        if (xhr.readyState === 4) { // Done
          if (xhr.status === 200) {
            try {
              eval(xhr.responseText); // jshint ignore:line
            }
            catch (e) {
              settings.logError('The server call succeeded, but the returned Javascript contains an error: '+e);
            }
            finally {
              onSuccess();
            }
          // done + 0 status = aborted, or at least it's the most
          // straightforward proxy that we have; ready state = 0 is
          // supposed to be aborted, but that's all kinds of not
          // working, unfortunately. jQuery's approach is better,
          // but they have a lot more state tracking to achieve it.
          } else if (xhr.status !== 0) {
            onFailure();
          }
        }
      };

      if (qs !== "") {
        url = url+"?"+qs;
      }

      xhr.open("GET", url, true);
      xhr.timeout = settings.cometGetTimeout;
      xhr.setRequestHeader("Accept", "text/javascript, application/javascript, application/ecmascript, application/x-ecmascript, */*; q=0.01");
      xhr.setRequestHeader("X-Requested-With", "XMLHttpRequest");
      xhr.send();

      return xhr;
    }
  };

  var liftVanilla = {
    init: function(settings) {
      lift.init(settings, defaultSettings);
    },
    defaultSettings: defaultSettings
  };

  return liftVanilla;
}));
