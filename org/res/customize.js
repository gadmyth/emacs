function toggle_toc() {
    var width = document.documentElement.clientWidth;
    var displayVal = "inline";
    if (width <= 757) {
        displayVal = "none";
    }
    console.log("" + width);
    document.getElementById("table-of-contents").style.display = displayVal;
    document.getElementById("postamble").style.display = displayVal;
}

window.onresize = function() {
    toggle_toc();
}

$(document).ready(function() {
    toggle_toc();
});