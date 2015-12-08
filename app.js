$(document).ready(function(){
    $('#myTab a').click(function (e) {
        e.preventDefault();
        $(this).tab('show');
    });

    // store the currently selected tab in the hash value
    $("ul.nav-tabs > li > a").on("shown.bs.tab", function (e) {
        var id = $(e.target).attr("href").substr(1);
        window.location.hash = id;
    });
    // on load of the page: switch to the currently selected tab
    var hash = window.location.hash;
    $('#myTab a[href="' + hash + '"]').tab('show');

    // import the bibliography
    $("#importBib").load("bib.html"); 
    
    // when a project is selected, center it and remove the others
    // add a back button for return
//    $(".projects .thumbnail").click(function(){
//        $(this).parent().siblings().children().toggle();
//        $(this).parent().toggleClass("col-xs-4").toggleClass("col-xs-12");
//    });

});

