$(function() {
    $('.note').before("<p class='admonition-title note'>Note</p>");
    $('.seealso').before("<p class='admonition-title seealso'>See also</p>");
    $('.warning').before("<p class='admonition-title warning'>Warning</p>");
    $('.caution').before("<p class='admonition-title caution'>Caution</p>");
    $('.attention').before("<p class='admonition-title attention'>Attention</p>");
    $('.tip').before("<p class='admonition-title tip'>Tip</p>");
    $('.important').before("<p class='admonition-title important'>Important</p>");
    $('.hint').before("<p class='admonition-title hint'>Hint</p>");
    $('.error').before("<p class='admonition-title error'>Error</p>");
    $('.danger').before("<p class='admonition-title danger'>Danger</p>");
});

$( document ).ready(function() {

    // Shift nav in mobile when clicking the menu.
    $(document).on('click', "[data-toggle='wy-nav-top']", function() {
      $("[data-toggle='wy-nav-shift']").toggleClass("shift");
      $("[data-toggle='rst-versions']").toggleClass("shift");
    });
    // Close menu when you click a link.
    $(document).on('click', ".wy-menu-vertical .current ul li a", function() {
      $("[data-toggle='wy-nav-shift']").removeClass("shift");
      $("[data-toggle='rst-versions']").toggleClass("shift");
    });
    $(document).on('click', "[data-toggle='rst-current-version']", function() {
      $("[data-toggle='rst-versions']").toggleClass("shift-up");
    });
    // Make tables responsive
    $("table.docutils:not(.field-list)").wrap("<div class='wy-table-responsive'></div>");
});

$( document ).ready(function() {
    // Inject toggle button for table of contents
    var tocToggle = $('<button id="toc-toggle" class="btn btn-default btn-sm">&#9776;</button>');
    $('body').append(tocToggle);
    
    // Toggle table of contents visibility
    var isExpanded = true;
    
    // 根据 isExpanded 更新布局（显示/隐藏 TOC 并调整内容区边距）
    function updateLayout() {
        if (isExpanded) {
            $('#table-of-contents, #postamble').show();
            // Center content in remaining space after subtracting 300px
            var remainingWidth = $(window).width() - 300;
            var leftMargin = (remainingWidth - 1000) / 2;
            if (leftMargin < 0) leftMargin = 0;
            $('#content').css({'margin-left': (300 + leftMargin) + 'px'});
        } else {
            $('#table-of-contents, #postamble').hide();
            $('#content').css({'margin-left': 'auto', 'margin-right': 'auto'});
        }
    }
    
    // 窗口大小改变时重新计算居中位置
    $(window).on('resize', updateLayout);
    
    // 点击按钮时切换状态并更新布局
    $('#toc-toggle').click(function() {
        isExpanded = !isExpanded;
        updateLayout();
    });
    
    // 页面首次加载时执行一次，确保初始状态居中
    updateLayout();
    
    $('#text-table-of-contents ul').first().addClass('nav');
    // ScrollSpy also requires that we use
    // a Bootstrap nav component.
    $('body').scrollspy({target: '#text-table-of-contents'});
});

window.SphinxRtdTheme = (function (jquery) {
    var stickyNav = (function () {
        var navBar,
            win,
            stickyNavCssClass = 'stickynav',
            applyStickNav = function () {
                if (navBar.height() <= win.height()) {
                    navBar.addClass(stickyNavCssClass);
                } else {
                    navBar.removeClass(stickyNavCssClass);
                }
            },
            enable = function () {
                applyStickNav();
                win.on('resize', applyStickNav);
            },
            init = function () {
                navBar = jquery('nav.wy-nav-side:first');
                win    = jquery(window);
            };
        jquery(init);
        return {
            enable : enable
        };
    }());
    return {
        StickyNav : stickyNav
    };
}($));
