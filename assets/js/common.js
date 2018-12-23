

 $('.btnProcessFile').click(function(){
    var pathOption=$("#csvfile")[0].files[0].name;
    $.ajax({
        url: '/ReadFile',
        data:{path:pathOption},
        type: "GET", // if you want to send data via the "data" property change this to "POST". This can be omitted otherwise
        success: function(responseData) {
        },
        error: console.error
      });
});

