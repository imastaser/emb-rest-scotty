String.prototype.trim = function () {
    return this.replace(/^\s+|\s+$/g, "");
};

String.prototype.trimStart = function () {
    return this.replace(/^\s+/, "");
};

String.prototype.trimEnd = function () {
    return this.replace(/\s+$/, "");
};

String.replace = function (value, oldValue, newValue) {
    return (value + "").replace(new RegExp(oldValue, "gmi"), newValue);
};

//Formats string like in c# String.format
String.prototype.format = function () {
    var returnValue = this;
    for (var i = 0; i < arguments.length; i++) {
        //replace all occurrences of {i} in source string
        while (returnValue.indexOf("{" + i.toString() + "}") != -1)
            returnValue = returnValue.replace("{" + i.toString() + "}", arguments[i]);
    }
    return returnValue;
};

String.isNullOrEmpty = function (value) {
    return value === undefined || value === null || value == "";
};

String.addItem = function (text, item, delimiter, addIfNotEmpty) {
    if (addIfNotEmpty && item == "") {
        return text;
    }

    if (text != "") {
        text += delimiter;
    }
    text += item;

    return text;
};

String.prototype.addItem = function (item, delimiter, addIfNotEmpty) {
    return String.addItem(this, item, delimiter, addIfNotEmpty);
};

Array.prototype.removeAt = function (idx) {
    this.splice(idx, 1);
    return this;
};

Array.prototype.swap = function (idx1, idx2) {
    var temp = this.splice(idx1, 1);
    this.splice(idx2, 0, temp[0]);
};