var PersonEntity = {
    Initializing: true,
    OnSuccess: null,
    GetData: function (form) {
        
        var data = Form.toJson(form);
        data["firstname"] = $("#firstname").val();
        data["lastname"]  = $("#lastname").val();
        data["email"]     = $("#email").val();
        
        return JSON.stringify(data);
    },
    Validate: function (form) {
        var valid = Form.valid(form);

        if (!valid) {
            form.data("validator").focusInvalid();
            return false;
        }

        return true;
    },
    Add: function () {
        
        var form = $("#personForm");

        if (!PersonEntity.Validate(form)) {
            return;
        }

        var url = "/person/add";
        var data = PersonEntity.GetData(form);

        console.log(data);
                
        Ajax.post({
            url: url,
            async: true,
            data: data,
            lockAjax: false,
            showFailure: false,
            showPreloader: false,
            preloaderSelector: "",
            success: function (response) {
                
                console.log(response);

            },
            error: function (response) {
                //Preloader.hide();
                console.log(response.Message);
            }
        });
    }
}


$(function(){
  $("#addBtn").on("click", PersonEntity.Add);
});

