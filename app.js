$(document).ready(function(){
    // random banner
//    $('#header').css({'background-image': 'url(images/' + images[Math.floor(Math.random() * images.length)] + ')'});
//    $('<img src="images/' + images[Math.floor(Math.random() * images.length)] + '">').appendTo('#banner');

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
    
    // import the project descriptions
    $("#importProj").load("projects.html"); 
    
    // when a project is selected, center it and remove the others
    /*$(".projects .thumbnail").click(function(){
        $(this).parent().siblings().children("h3").toggle();
        $(this).parent().siblings().children(".thumbnail").toggle();
        $(this).parent().toggleClass("col-xs-4").toggleClass("col-xs-12");
        $(this).next().toggle();
    });*/
});

// Google Analytics
(function(i,s,o,g,r,a,m){
    i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
        (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
        m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');
ga('create', 'UA-73376463-1', 'auto');
ga('send', 'pageview');
