(function() {

var OrderEntity = {
    initialize: function() {
        $("#addOrderBtn").on("click", OrderEntity.Add);
        $("#saveBtn").on("click", OrderEntity.Save);
        $('#name').focus();    
        OrderEntity.initDelete($('.rest-delete'));

        // $('#datetime').datepicker({
        //         closeText: "Select",
        //         changeMonth: true,
        //         changeYear: true,
        //          pickTime: false
        //     });


        Ajax.get({
            url: '/person',
            async: true,
            lockAjax: false,
            showFailure: false,
            showPreloader: false,
            preloaderSelector: "",
                success: function(data) {
                    debugger;
                    console.log(data);
                     $($.parseJSON(data)).map(function () {
                         return $('<option>').val(this.id).text(this.firstname);
                    }).appendTo('#personCmb');
                    
                },
                error: function(data) {
                    alert("Error while populating person combobox.");
                    console.log(data);
                }
              });

       


    },
    Initializing: true,
    OnSuccess: null,
    GetData: function (form) {
        
        var data = Form.toJson(form);
        data["person_id"] = parseInt(form.attr('tag'));
        data["product_id"] = parseInt($("#product_id").val());
        data["price"]     = parseInt($("#price").val());
        data["caxs"]     = parseInt($("#caxs").val());
        data["count"]     = parseInt($("#caxs").val());
        data["datetime"]     = $("#datetime").val();
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
        
        var form = $("#orderForm");

        if (!OrderEntity.Validate(form)) {
            return;
        }

        var url = "/person/" + form.attr('tag') + "/order/add";
        console.log(url);
        var data = OrderEntity.GetData(form);

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
                OrderEntity.Reset()
                var delLink = $(row).find("a[data-method='delete']");
  
                OrderEntity.initDelete(delLink);
                             //console.log(response);
            },
            error: function (err) {
               console.log(err);
               alert(err);     
            }
        });
    },
    Save: function () {
        
        var form = $("#orderForm");

        if (!OrderEntity.Validate(form)) {
            return;
        }
//debugger;
        var url = form.attr('action');
        var data = OrderEntity.GetData(form);
        
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
               window.location.href = window.location.origin+"/person/" + id + "/orders/";
            },
            error: function (err) {
               console.log(err);
               //alert(err);     
            }
        });
    },

    Reset: function (){
        $('#person_id').focus();
        $('#price').val('');
        $('#caxs').val('');
        $('#count').val('');
        $('#datetime').val('');
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

  OrderEntity.initialize();

})();

// $(function(){
// });PersonEntity

// jQuery(document).ready(
//     function () {

    

// });


