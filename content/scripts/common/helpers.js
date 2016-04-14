var Form = {
    valid: function (form) {
        if (typeof form == String) {
            form = $("#" + form);
        }

        if (!form.data("unobtrusiveValidation")) {
            $.validator.unobtrusive.parse(form);
        }
        return form.valid();
    },
    toJson: function(form)
    {
        var o = {};
        var a = form.serializeArray();
        $.each(a, function() {
            if (o[this.name] !== undefined) {
                if (!o[this.name].push) {
                    o[this.name] = [o[this.name]];
                }
                o[this.name].push(this.value || '');
            } else {
                o[this.name] = this.value || '';
            }
        });
        return o;
    },
    serializeFormJSON: function (form) {

        var o = {};
        var a = form.serializeArray();
        $.each(a, function () {
            if (o[this.name]) {
                if (!o[this.name].push) {
                    o[this.name] = [o[this.name]];
                }
                o[this.name].push(this.value || '');
            } else {
                o[this.name] = this.value || '';
            }
        });
        return o;
    }


};
