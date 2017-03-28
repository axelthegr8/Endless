/*

$(document).ready(function(){
    $("#hide").click(function(){
        $("#feed").hide();
    });
    $(".col-feed").click(function(){
         
        $("#feed").show();
    });
});
*/

$(document).ready(function(){
   $('a').click(function(){
       $("#pager_1").hide()
       $("#pager_2").hide()
       $("#pager_3").hide()
       $("#pager_4").hide()
      var id = "#pager_" + $(this).attr('id');
       $(id).show();
           
   });
});

/*<a href="#" id="pager_1" class="pagerlink" >link</a>

$('a.pagerlink').click(function() { 
    
    var id = $(this).attr('id');
    alert(id);
});*/