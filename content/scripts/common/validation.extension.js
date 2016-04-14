(function ($) {
    $.fn.modifyRules = function (data, disable) {
        this.each(function () {
            var elem = $(this);
            if (typeof data === "boolean") {
                elem.data("Rules", {});

                if (!data) {
                    for (rule in elem.rules()) {
                        elem.modifyRules(rule, data);
                    }
                }
            }
            else if (typeof data === "object") {
                for (rule in data) {
                    elem.modifyRules(rule, data[rule]);
                }
            }
            else if (typeof data === "string") {
                var ruleNames = data.trim().split(' ');
                var rules = elem.data("Rules") || {};
                elem.data("Rules", rules);
                for (var i = 0; i < ruleNames.length; i++) {
                    var ruleName = ruleNames[i].trim();
                    if (ruleName) {
                        if (disable) {
                            delete rules[ruleName];
                        }
                        else {
                            rules[ruleName] = false;
                        }
                    }
                }
            }
        });

        return this;
    };

    $.validator.setDefaults({
        validClass: "",
        //onfocusout: false,
        onkeyup: false,
        //onclick: false,
        ignore: "",
        highlight: function (element, errorClass, validClass) {
            updateValidationClasses(this, element, errorClass, validClass);
        },
        unhighlight: function (element, errorClass, validClass) {
            updateValidationClasses(this, element, validClass, errorClass);
        }
    });
    $.validator.classRuleSettings = {};

    function updateValidationClasses(sender, element, addClass, removeClass) {
        var elem;
        if (element.type === 'radio') {
            elem = sender.findByName(element.name);
        }
        else {
            elem = $(element);
            if (elem.data("role") == "combobox") {
                elem = elem.prev();
            }
            else if (elem.data("role") == "datepicker") {
                elem = elem.parent();
            }
        }

        elem.removeClass(removeClass).addClass(addClass);
    };

    jQuery.validator.unobtrusive.parseArea = function (selector) {
        $(selector).find(":input").filter("[data-val=true]").each(function () {
            jQuery.validator.unobtrusive.parseElement(this, false);
        });
    };

    // Globanet Custom attribute methods
    jQuery.validator.unobtrusive.adapters.add("booleanrequired", function (options) {
        options.rules["required"] = true;
        if (options.message) {
            options.messages["required"] = options.message;
        }
    });

    //jQuery.validator.addMethod('requiredif', function (value, element, params) {
    //    var dependentProperty = params['dependentproperty'];
    //    var targetValue = params['targetvalue'];

    //    var dependentValue = this.findByName(dependentProperty).val();

    //    if (dependentValue.length > 0) {
    //        if (targetValue.length == 0 || dependentValue == targetValue) {
    //            return value.length > 0;
    //        }
    //    }

    //    return true;
    //});

    //RequiredIfNew
    jQuery.validator.addMethod('requiredif', function (value, element, params) {
        var that = this;

        var condition = params.condition;
        var conditionHasErrors = false;

        var isCond = checkCondition(condition);

        if (conditionHasErrors) {
            console.error("Invalid condition pattern.")
            return false;
        }
        return !isCond || value.length > 0;

        function checkCondition(cond) {
            if (conditionHasErrors) {
                return;
            }

            var isCondition = false;
            var start = 0;
            var end = cond.indexOf(')');

            if (end >= 0) {
                start = cond.lastIndexOf('(', end);
                if (start < 0) {
                    conditionHasErrors = true;
                    return;
                }

                var subCond = cond.slice(start + 1, end);
                cond = cond.replace("(" + subCond + ")", checkCondition(subCond));

                return (checkCondition(cond));
            }

            var matches = cond.match(/\|\||&&/g);
            var subConds = cond.split(/\|\||&&/g);
            isCondition = parseStringToBool(subConds[0]);

            if (matches) {
                for (var i = 0; i < matches.length; i++) {
                    if (subConds[i + 1] == "") {
                        conditionHasErrors = true;
                        return;
                    }

                    switch (matches[i]) {
                        case "||":
                            isCondition = isCondition || parseStringToBool(subConds[i + 1]);
                            break;
                        case "&&":
                            isCondition = isCondition && parseStringToBool(subConds[i + 1]);
                            break;
                        default:
                            break;
                    }
                }
            }

            return isCondition;
        }

        function parseStringToBool(str) {
            str = str.trim();

            switch (str) {
                case "True":
                case "true":
                    return true;
                case "False":
                case "false":
                    return false;
                default:
                    var match = str.match(/==|=|!=|>|>=|<|<=/g);
                    if (match) {
                        match = match[0];
                    }
                    else {
                        conditionHasErrors = true;
                        return;
                    }

                    var strArr = str.split(match);
                    var prop = strArr[0];
                    var val = strArr[1];

                    if (prop == "" || val == "") {
                        conditionHasErrors = true;
                        return;
                    }

                    return comparePropertyAndValue(prop, val, match);
            }
        }

        function comparePropertyAndValue(prop, val, op) {
            prop = prop.trim();

            var match = val.match(/'.*?'/g)
            val = match ? match[0] : val.trim();

            var property = that.findByName(prop);
            var propertyValue = property.val();

            if (that.checkable(property[0])) {
                switch (property[0].type) {
                    case 'radio':
                        propertyValue = property.filter(":checked").val();
                        break;
                    case 'checkbox':
                        propertyValue = property.is(':checked');
                        break;
                }
            }
            if (propertyValue == null) {
                conditionHasErrors = true;
                return;
            }

            var dataType;

                //Boolean
            if (val.toLowerCase() == "true" || val.toLowerCase() == "false") {
                val = (val.toLowerCase() === "true");
            }
                //String
            else if (val[0] == '\'' && val[val.length - 1] == '\'') {
                val = val.slice(1, val.length - 1);
            }
                //DateTime
            else if (val.slice(val.length - 3, val.length) == "/dt") {
                val = kendo.parseDate(val.Substring(0, val.length - 3));
                propertyValue = kendo.parseDate(propertyValue);
                dataType = "date";
            }
                //Null
            else if (val.toLowerCase() == "null" || val.toLowerCase() == "empty") {
                val = "";
            }
            else  //Numeric
            {
                val = kendo.parseFloat(val);
                propertyValue = kendo.parseFloat(propertyValue);
            }

            switch (op) {
                case "==":
                case "=":
                    return propertyValue == val;
                case "!=":
                    return propertyValue != val;
                case ">":
                    return propertyValue > val;
                case "<":
                    return propertyValue < val;
                case ">=":
                    return propertyValue >= val;
                case "<=":
                    return propertyValue <= val;
            }

            return true;
        }
    });
    //RequiredIfNew

    jQuery.validator.addMethod('comparevalues', function (value, element, params) {
        var dependentProperty = params['dependentproperty'];
        var criteria = params['criteria'];
        var datatype = params['datatype'];

        var dependentValue = this.findByName(dependentProperty).val();

        if (value && dependentValue) {
            switch (datatype) {
                case "numeric":
                    value = kendo.parseFloat(value);
                    dependentValue = kendo.parseFloat(dependentValue);
                    break;
                case "date":
                    var format = params['format'];
                    value = kendo.parseDate(value, format);
                    dependentValue = kendo.parseDate(dependentValue, format);

                    if (value == null || dependentValue == null) {
                        return false;
                    }
                    break;
            }

            switch (criteria) {
                case "=":
                    return value == dependentValue;
                case "!=":
                    return value != dependentValue;
                case ">":
                    return value > dependentValue;
                case "<":
                    return value < dependentValue;
                case ">=":
                    return value >= dependentValue;
                case "<=":
                    return value <= dependentValue;
            }
        }

        return true;
    });

    jQuery.validator.addMethod('dateformat', function (value, element, params) {
        if (value) {
            return isDate(value, params['format']);
        }

        return true;
    });

    jQuery.validator.addMethod('mindate', function (value, element, params) {
        if (value) {
            return kendo.parseDate(value, params.format) >= kendo.parseDate(params.today, params.format);
        }

        return true;
    });

    jQuery.validator.addMethod('agerange', function (value, element, params) {
        if (value) {
            var date = kendo.parseDate(value, params['format']);
            if (date == null) {
                return false;
            }

            var year = date.getFullYear();
            return kendo.parseInt(params['yearmin']) <= year && year <= kendo.parseInt(params['yearmax']);
        }

        return true;
    });

    jQuery.validator.addMethod("videourl",
        function (value, element) {
            var d = $(element).data("VideoUrl");
            if (d) {
                return !d.HasError;
            }

            return true;
        },
        function (params, element) {
            var d = $(element).data("VideoUrl");
            if (d && d.Message) {
                return d.Message;
            }

            return "Invalid Video Url";
        }
    );

    jQuery.validator.addMethod("validexpirationdate", function (value, element, param) {
        if (value == "") {
            return true;
        }
        var expYearSelector = "[name$='" + param.expirationyearfieldname + "']";
        var expirationYear = $(expYearSelector).val();

        if (expirationYear == "") {
            return true;
        }

        if ((new Date()).getFullYear() == parseInt(expirationYear)) {
            if ((new Date()).getMonth() >= parseInt(value)) {
                return false;
            }
        }
        return true;
    });

    // Globanet Custom attribute adapters
    //jQuery.validator.unobtrusive.adapters.add('requiredif', ['dependentproperty', 'targetvalue'], function (options) {
    //    options.rules["requiredif"] = {
    //        dependentproperty: options.params.dependentproperty,
    //        targetvalue: options.params.targetvalue
    //    };
    //    if (options.message) {
    //        options.messages["requiredif"] = options.message;
    //    }
    //});

    //RequiredIfAdvanced
    jQuery.validator.unobtrusive.adapters.add("requiredif", ["condition"], function (options) {
        options.rules["requiredif"] = {
            condition: options.params.condition
        };
        if (options.message) {
            options.messages["requiredif"] = options.message;
        }
    });
    //RequiredIfAdvanced

    jQuery.validator.unobtrusive.adapters.add("comparevalues", ["dependentproperty", "criteria", "datatype", "format"], function (options) {
        options.rules["comparevalues"] = {
            dependentproperty: options.params.dependentproperty,
            criteria: options.params.criteria,
            datatype: options.params.datatype,
            format: options.params.format
        };
        if (options.message) {
            options.messages["comparevalues"] = options.message;
        }
    });

    jQuery.validator.unobtrusive.adapters.add("dateformat", ["format"], function (options) {
        options.rules["dateformat"] = {
            format: options.params.format
        };
        if (options.message) {
            options.messages["dateformat"] = options.message;
        }
    });

    jQuery.validator.unobtrusive.adapters.add("mindate", ["format", "today"], function (options) {
        options.rules["mindate"] = {
            format: options.params.format,
            today: options.params.today
        };
        if (options.message) {
            options.messages["mindate"] = options.message;
        }
    });

    jQuery.validator.unobtrusive.adapters.add("agerange", ["format", "yearmin", "yearmax"], function (options) {
        options.rules["agerange"] = {
            format: options.params.format,
            yearmin: options.params.yearmin,
            yearmax: options.params.yearmax
        };
        if (options.message) {
            options.messages["agerange"] = options.message;
        }
    });

    jQuery.validator.unobtrusive.adapters.add("videourl", function (options) {
        options.rules["videourl"] = true;
        if (options.message) {
            options.messages["videourl"] = options.message;
        }
    });

    jQuery.validator.unobtrusive.adapters.add("validexpirationdate", ["expirationyearfieldname"], function (options) {
        options.rules["validexpirationdate"] = options.params;
        options.messages["validexpirationdate"] = options.message;
    });
})($);
