var Ajax = {
    options: {
        lockAjax: true,
        async: true,

        traditional: true,
        cache: false,

        showPreloader: true,
        showFailure: true
    }
};

(function () {
    Ajax.post = function (options) {
        return process(options, "POST");
    };

    Ajax.get = function (options) {
        return process(options, "GET");
    };

    var redirect = false,
        progress = false;

    function process(options, type) {
        options = $.extend({}, Ajax.options, options);

        if (options.lockAjax && progress) {
            return;
        }

        options.type = type;

        var success = options.success;
        options.success = function (response) {
            if (response && response.RedirectUrl) {
                location.href = response.RedirectUrl;
                redirect = true;
            }
            else if ($.isFunction(success)) {
                success(response);
            }
        };

        var error = options.error;
        options.error = function (response) {
            if ($.isFunction(error)) {
                error(response);
            }

            if (options.showFailure) {
                Message.failure();
            }
        };

        var complete = options.complete;
        options.complete = function (response) {
            if ($.isFunction(complete)) {
                complete(response);
            }

            if (!redirect) {
                if (options.lockAjax) {
                    progress = false;
                }
                if (options.showPreloader) {
                    if (options.preloaderSelector) {
                        $(options.preloaderSelector).hidePreloader();
                    }
                    else {
                        Preloader.hide();
                    }
                }
            }
        };

        if (options.showPreloader) {
            if (options.preloaderSelector) {
                $(options.preloaderSelector).showPreloader();
            }
            else {
                Preloader.show();
            }
        }

        if (options.lockAjax) {
            progress = true;
        }

        return $.ajax(options);
    };

    Ajax.LoadPartialView = function (url, data, successCallback, errorCallback, showPreloader, cache) {

        if (!cache) {
            cache = false;
        }

        if (!successCallback) {
            successCallback = null;
        }

        if (!errorCallback) {
            errorCallback = null;
        }

        showPreloader = (typeof showPreloader === "undefined") ? true : showPreloader;

        Ajax.get({
            url: url,
            data: data,
            lockAjax: false,
            showPreloader: showPreloader,
            preloaderSelector: "",
            showFailure: true,
            success: function (response) {
                if (response.HasError) {
                    Message.error(response.Message);
                }
                else if ($.isFunction(successCallback)) {
                    successCallback(response);
                }
            },
            error: function (response) {
                if ($.isFunction(errorCallback)) {
                    errorCallback(response);
                } else {
                    Message.error(response.Message);
                }
            }
        });
    };

    Ajax.LoadPartialViewInPopup = function (windowId, title, url, data, callback, showPreloader, cache, options) {
        if (data == null) {
            data = {};
        }

        showPreloader = (typeof showPreloader === "undefined") ? true : showPreloader;

        var success = function(response) {
            var html = response.Html || response;
            if (!options) {
                options = {};
            }

            options = $.extend({
                modal: true,
                draggable: true,
                resizable: false,
                destroyOnClose: true
            }, options);
            

            if ($("#" + windowId).length == 0) {
                var element = $("<div>").attr("id", windowId);
                element.kendoWindow(options).getKendoWindow().content(html);
            }

            var wnd = $("#" + windowId).data("kendoWindow");
            if (wnd) {
                wnd.openInCenter();
                if (title) {
                    wnd.title(title);
                }
            }

            if ($.isFunction(callback)) {
                callback();
            }
        };

        Ajax.LoadPartialView(url, data, success, null, showPreloader, cache);
    };
})();