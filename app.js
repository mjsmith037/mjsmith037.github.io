$(document).ready(function(){
    // random banner
//    $('#header').css({'background-image': 'url(images/' + images[Math.floor(Math.random() * images.length)] + ')'});
//    $('<img src="images/' + images[Math.floor(Math.random() * images.length)] + '">').appendTo('#banner');


/////////////////////////////////// NAV TABS ///////////////////////////////////
    // show a tab when clicked and save in history
    $('#myTabs a').click(function (e) {
        $(this).tab('show');
        history.pushState(null, null, $(e.target).attr("href"));
    });
    // store the currently selected tab in the hash value
    $('#myTabs a').on("shown.bs.tab", function (e) {
        location.hash = $(e.target).attr("href").substr(1);
    });
    // show active tab on reload
    if (location.hash !== '') $('a[href="' + location.hash + '"]').tab('show');
    // navigate to a tab when the history changes
    window.addEventListener("popstate", function(e) {
        var activeTab = $('[href=' + location.hash + ']');
        if (activeTab.length) {
            activeTab.tab('show');
        } else { /*This part still not working*/
            $('#myTabs a:first').tab('show')
        }
    });


    // import the bibliography
    $("#importBib").load("bib.html");

    // import the project descriptions
    $("#importProj").load("projects.html");

    // import the project descriptions
    $("#importBlog").load("blog/index.html");

    // when a project is selected, center it and remove the others
    /*$(".projects .thumbnail").click(function(){
        $(this).parent().siblings().children("h3").toggle();
        $(this).parent().siblings().children(".thumbnail").toggle();
        $(this).parent().toggleClass("col-xs-4").toggleClass("col-xs-12");
        $(this).next().toggle();
    });*/
});

// // Google Analytics
// (function(i,s,o,g,r,a,m){
//     i['GoogleAnalyticsObject']=r;
//     i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},
//     i[r].l=1*new Date();
//     a=s.createElement(o), m=s.getElementsByTagName(o)[0];
//     a.async=1;
//     a.src=g;
//     m.parentNode.insertBefore(a,m)
// })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
// ga('create', 'UA-73376463-1', 'auto');
// ga('send', 'pageview');
