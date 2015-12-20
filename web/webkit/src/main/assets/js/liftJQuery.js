(function (root, factory) {
  if(typeof define === "function" && define.amd) {
    define(["jquery"], function(jQuery) {
      return (root.liftJQuery = factory(root.document, jQuery));
    });
  } else if(typeof module === "object" && module.exports) {
    module.exports = factory(root.document, require("jquery"));
  } else {
    root.liftJQuery = factory(root.document, root.jQuery);
  }
}(this, function(document, jQuery) {

  var liftJQuery = {
    onEvent: function(elementOrId, eventName, fn) {
      if (typeof elementOrId === 'string') {
        elementOrId = '#' + elementOrId;
      }

      jQuery(elementOrId).on(eventName, fn);
    },
    onDocumentReady: jQuery(document).ready,
    ajaxPost: function(url, data, dataType, onSuccess, onFailure) {
      var processData = true,
          contentType = 'application/x-www-form-urlencoded; charset=UTF-8';

      if (typeof data === "object") { // FormData
        processData = false;  // tell jQuery not to process the data
        contentType = false; // tell jQuery not to set contentType
      }

      jQuery.ajax({
        url: url,
        data: data,
        type: "POST",
        dataType: dataType,
        timeout: this.ajaxPostTimeout,
        cache: false,
        success: onSuccess,
        error: onFailure,
        processData: processData,
        contentType: contentType
      });
    },
    ajaxGet: function(url, data, onSuccess, onFailure) {
      return jQuery.ajax({
        url: url,
        data: data,
        type: "GET",
        dataType: "script",
        timeout: this.cometGetTimeout,
        cache: false,
        success: onSuccess,
        error: function(_, status) {
          if (status !== 'abort') {
            return onFailure.apply(this, arguments);
          }
        }
      });
    }
  };

  return liftJQuery;
}));
