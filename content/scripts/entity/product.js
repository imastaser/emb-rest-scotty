(function() {

var ProductEntity = {
    initialize: function() {
        $("#addProductBtn").on("click", ProductEntity.Add);
        $("#saveBtn").on("click", ProductEntity.Save);
        $('#name').focus();    
        ProductEntity.initDelete($('.rest-delete'));
    },
    Initializing: true,
    OnSuccess: null,
    GetData: function (form) {
        
        var data = Form.toJson(form);
        data["person_id"] = parseInt(form.attr('tag'));
        data["workType_id"] = parseInt($("#workType_id").val());
        data["name"]  = $("#name").val();
        data["price"]     = parseInt($("#price").val());
        data["caxs"]     = parseInt($("#caxs").val());
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
        
        var form = $("#productForm");

        if (!ProductEntity.Validate(form)) {
            return;
        }

        var url = "/person/" + form.attr('tag') + "/product/add";
        console.log(url);
        var data = ProductEntity.GetData(form);

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
                ProductEntity.Reset()
                var delLink = $(row).find("a[data-method='delete']");
  
                ProductEntity.initDelete(delLink);
                             //console.log(response);
            },
            error: function (err) {
               console.log(err);
               alert(err);     
            }
        });
    },
    Save: function () {
        
        var form = $("#productForm");

        if (!ProductEntity.Validate(form)) {
            return;
        }
//debugger;
        var url = form.attr('action');
        var data = ProductEntity.GetData(form);
        
        var id = form.attr('tag');

       // data = JSON.stringify(actualObj);

        console.log("ajax  = " + data);
                
        Ajax.put({
            url: url,
            async: true,
            data: data,
            lockAjax: false,
            showFailure: false,
            showPreloader: false,
            preloaderSelector: "",
            success: function (response) {
                
                console.log(response);
                // similar behavior as an HTTP redirect
                //window.location.replace("http://stackoverflow.com");

                // similar behavior as clicking on a link
               window.location.href = window.location.origin+"/person/" + id + "/products/";
            },
            error: function (err) {
               console.log(err);
               //alert(err);     
            }
        });
    },

    Reset: function (){
        $('#person_id').focus();
        $('#workType_id').val('');
        $('#name').val('');
        $('#price').val('');
        $('#caxs').val('');
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
                    $('#ps').children('tr:first').fadeIn('slow');
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

  ProductEntity.initialize();

})();

// $(function(){
// });PersonEntity

// jQuery(document).ready(
//     function () {

    

// });


