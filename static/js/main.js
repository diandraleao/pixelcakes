jQuery(document).ready(function(a) {
    /*a(function() {
        var b = window.location.hash.substr(1);
        a('nav[role="tablist"] a').removeClass("active"), a('nav[role="tablist"] a[href="#' + b + '"]').tab("show").addClass("active"), a('nav[role="tablist"] a').click(function(b) {
            b.preventDefault(), a('nav[role="tablist"] a').removeClass("active"), a(this).tab("show").addClass("active")
        })
    });*/
    var b = a("#navbar-main").offset().top,
        c = a(window);
    c.scroll(function() {
        c.scrollTop() >= b ? a("#navbar-main").removeClass("navbar-fixed-top").addClass("navbar-fixed-top") : a("#navbar-main").removeClass("navbar-fixed-top")
    })
});