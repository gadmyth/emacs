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

    // Per-page storage keys: pathname already excludes query string and hash,
    // so the same page with different URL params shares the same state.
    var storagePrefix = window.location.pathname + ':';
    var keyExpanded = storagePrefix + 'sidebar-expanded';
    var keyWidth = storagePrefix + 'sidebar-width';

    // Toggle table of contents visibility (persisted across reloads, per page)
    var savedExpanded = localStorage.getItem(keyExpanded);
    var isExpanded = (savedExpanded === null) ? true : (savedExpanded === 'true');

    // Sidebar width: toc-toggle button width + its padding-left + padding-right is the minimum,
    // default 300px is the maximum
    var SIDEBAR_MIN = 50;
    var SIDEBAR_MAX = 300;
    // Below this window width the sidebar auto-hides (responsive) regardless of toggle state
    var SIDEBAR_VISIBLE_THRESHOLD = 757;
    var savedWidth = parseInt(localStorage.getItem(keyWidth), 10);
    var currentWidth = (savedWidth >= SIDEBAR_MIN && savedWidth <= SIDEBAR_MAX)
        ? savedWidth : SIDEBAR_MAX;
    var docEl = document.documentElement;
    var $body = $('body');
    docEl.style.setProperty('--sidebar-width', currentWidth + 'px');

    // Content centering is handled in CSS via max() + calc(); this only toggles visibility.
    // Sidebar is visible only when BOTH the user wants it expanded AND the window is wide enough.
    function updateLayout() {
        var isNarrow = window.innerWidth <= SIDEBAR_VISIBLE_THRESHOLD;
        var shouldShow = isExpanded && !isNarrow;
        $body.toggleClass('toc-collapsed', !shouldShow);
        $body.toggleClass('toc-narrow', isNarrow);
        $('#table-of-contents, #postamble').toggle(shouldShow);
    }

    // Resize handle: drag the right edge of the sidebar to adjust its width.
    // Hot path uses rAF + native events to keep drag smooth on high-Hz mice.
    var resizeHandle = document.createElement('div');
    resizeHandle.id = 'resize-handle';
    resizeHandle.title = 'Drag to resize sidebar';
    document.body.appendChild(resizeHandle);

    resizeHandle.addEventListener('mousedown', function(e) {
        e.preventDefault();
        $body.addClass('resizing');
        resizeHandle.classList.add('dragging');

        var pendingWidth = null;
        var rafScheduled = false;

        function applyPendingWidth() {
            rafScheduled = false;
            if (pendingWidth === null) return;
            docEl.style.setProperty('--sidebar-width', pendingWidth + 'px');
            pendingWidth = null;
        }

        function onMouseMove(e) {
            var w = e.clientX;
            if (w < SIDEBAR_MIN) w = SIDEBAR_MIN;
            else if (w > SIDEBAR_MAX) w = SIDEBAR_MAX;
            currentWidth = w;
            pendingWidth = w;
            if (!rafScheduled) {
                rafScheduled = true;
                requestAnimationFrame(applyPendingWidth);
            }
        }

        function onMouseUp() {
            document.removeEventListener('mousemove', onMouseMove);
            document.removeEventListener('mouseup', onMouseUp);
            $body.removeClass('resizing');
            resizeHandle.classList.remove('dragging');
            localStorage.setItem(keyWidth, currentWidth);
        }

        document.addEventListener('mousemove', onMouseMove);
        document.addEventListener('mouseup', onMouseUp);
    });

    // Double-click handle: snap to opposite end of the midpoint
    resizeHandle.addEventListener('dblclick', function() {
        currentWidth = (currentWidth < SIDEBAR_MAX / 2) ? SIDEBAR_MAX : SIDEBAR_MIN;
        docEl.style.setProperty('--sidebar-width', currentWidth + 'px');
        localStorage.setItem(keyWidth, currentWidth);
        updateLayout();
    });

    // 窗口大小改变时重新计算居中位置
    $(window).on('resize', updateLayout);

    // 点击按钮时切换状态并更新布局
    $('#toc-toggle').click(function() {
        isExpanded = !isExpanded;
        localStorage.setItem(keyExpanded, String(isExpanded));
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
