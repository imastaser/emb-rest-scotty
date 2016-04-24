(function() {

var PersonEntity = {
    initialize: function() {
        $("#addBtn").on("click", PersonEntity.Add);
        $('#firstname').focus();    
        PersonEntity.initDelete($('.rest-delete'));
    },
    Initializing: true,
    OnSuccess: null,
    GetData: function (form) {
        
        var data = Form.toJson(form);
        data["firstname"] = $("#firstname").val();
        data["lastname"]  = $("#lastname").val();
        data["email"]     = $("#email").val();
        data["phone"]     = $("#phone").val();
        data["phone2"]     = $("#phone2").val();
        data["note"]     = $("#note").val();
        
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
                //response.prependTo('#ps'); 
                var row = $(response)  
                row.hide();
                $('#ps').prepend(row);
                $("#ps tr").eq(1).fadeIn('slow')
                //$('#ps').children('tr:first').fadeIn('slow');
                PersonEntity.Reset()
                var delLink = $(row).find("a[data-method='delete']");
  
                PersonEntity.initDelete(delLink);
                             //console.log(response);
            },
            error: function (err) {
               console.log(err);
               alert(err);     
            }
        });
    },

    Reset: function (){
        $('#firstname').focus();
        $('#firstname').val('');
        $('#lastname').val('');
        $('#email').val('');
        $('#phone').val('');
        $('#phone2').val('');
        $('#note').val('');
    },

    initDelete: function(elem) {
        elem.on('click', function(e) {
            self=$(this);
            e.preventDefault();
            var id = self.data("tag");
            if(confirm('Are you sure?')) {

              $.ajax({
                url: self.attr('href'),
                method: 'DELETE',
                success: function(data) {
                    console.log(data);
                    var item = '#p_'+ id;
                    $(item).fadeOut('slow');
                    $(item).hide();
                },
                error: function(data) {
                    alert("Error while deleting.");
                    console.log(data);
                }
              });
            };
        })
    }

  
};

  PersonEntity.initialize();

})();

// $(function(){
// });PersonEntity

// jQuery(document).ready(
//     function () {

    

// });


