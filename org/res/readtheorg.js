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

    // Anchor by the viewport CENTER's percentage of the document so the section
    // the user is looking at stays put when layout re-wraps. ResizeObserver
    // fires AFTER the browser has actually re-laid out the content (more
    // reliable than rAF, which can race with layout).
    var savedRatio = 0;
    function captureScrollRatio() {
        var scrollEl = document.scrollingElement || document.documentElement;
        var h = scrollEl.scrollHeight;
        savedRatio = h > 0 ? (scrollEl.scrollTop + window.innerHeight / 2) / h : 0;
    }
    function restoreScrollRatio() {
        if (savedRatio <= 0) return;
        var scrollEl = document.scrollingElement || document.documentElement;
        scrollEl.scrollTop = savedRatio * scrollEl.scrollHeight - window.innerHeight / 2;
    }
    var contentEl = document.getElementById('content');
    if (contentEl && typeof ResizeObserver !== 'undefined') {
        new ResizeObserver(restoreScrollRatio).observe(contentEl);
    }
    // Keep savedRatio in sync with the user's current scroll position, so
    // window-resize (which has no explicit "start" hook) uses the right anchor.
    window.addEventListener('scroll', captureScrollRatio, { passive: true });

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

        // Capture viewport-center ratio so re-wrapping during the drag
        // doesn't shift the section the user is looking at. The ResizeObserver
        // callback restores the scroll position once the new layout settles.
        captureScrollRatio();

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
        captureScrollRatio();
        currentWidth = (currentWidth < SIDEBAR_MAX / 2) ? SIDEBAR_MAX : SIDEBAR_MIN;
        docEl.style.setProperty('--sidebar-width', currentWidth + 'px');
        localStorage.setItem(keyWidth, currentWidth);
        updateLayout();
    });

    // Window resize: updateLayout() handles the responsive show/hide classes;
    // the ResizeObserver callback restores the viewport-center Y position.
    $(window).on('resize', updateLayout);

    // TOC toggle: capture ratio before flipping the class so the
    // post-layout ResizeObserver callback restores the viewport center.
    $('#toc-toggle').click(function() {
        captureScrollRatio();
        isExpanded = !isExpanded;
        localStorage.setItem(keyExpanded, String(isExpanded));
        updateLayout();
    });

    // ----- Image lightbox -----
    // Click any <img> inside #content to open a full-screen overlay.
    // Wheel: zoom (anchored to cursor). Drag: pan. Esc / × / click backdrop: close.
    (function() {
        var overlay = document.createElement('div');
        overlay.id = 'image-overlay';
        document.body.appendChild(overlay);

        var closeBtn = document.createElement('button');
        closeBtn.id = 'image-overlay-close';
        closeBtn.innerHTML = '&times;';
        closeBtn.title = 'Close (Esc)';
        overlay.appendChild(closeBtn);

        var ZOOM_MIN = 0.1;
        var ZOOM_MAX = 10;
        var ZOOM_STEP = 1.15;
        var DRAG_CLOSE_THRESHOLD = 5;

        var overlayImg = null;
        var scale = 1;
        var panX = 0;
        var panY = 0;
        var isDragging = false;
        var mousedownX = 0;
        var mousedownY = 0;
        var panStartX = 0;
        var panStartY = 0;
        // Max Manhattan distance from the mousedown point. Using the
        // *peak* displacement (not accumulated path length) means a
        // user who wiggles back and forth near the start still gets
        // treated as a click once they release.
        var maxDisplacement = 0;

        function updateImageTransform() {
            if (!overlayImg) return;
            // The overlay uses flexbox to center the image, so its visual
            // center sits at the viewport center. We only need to apply
            // scale (around center) and a pan offset on top of that.
            overlayImg.style.transform =
                'translate(' + panX + 'px, ' + panY + 'px) scale(' + scale + ')';
        }

        function openOverlay(src) {
            // Drop any previous <img> first so a still-loading one can't
            // race the new src and run onload against the wrong element.
            if (overlayImg) {
                overlay.removeChild(overlayImg);
                overlayImg = null;
            }
            overlayImg = document.createElement('img');
            overlayImg.src = src;
            overlayImg.draggable = false;
            // Hide until the fit transform is applied — otherwise the
            // browser briefly paints the image at its natural size before
            // onload fires, causing a visible shrink-and-center flicker.
            overlayImg.style.visibility = 'hidden';
            overlay.appendChild(overlayImg);

            overlayImg.onload = function() {
                var img = this;
                var vw = window.innerWidth;
                var vh = window.innerHeight;
                var nw = img.naturalWidth;
                var nh = img.naturalHeight;
                // Fit-to-viewport, never upscale beyond natural size.
                scale = Math.min(vw / nw, vh / nh, 1);
                panX = 0;
                panY = 0;
                updateImageTransform();
                img.style.visibility = 'visible';
            };

            overlayImg.onerror = function() {
                // Show the broken-image marker so the user isn't stuck
                // looking at an empty overlay.
                this.style.visibility = 'visible';
            };

            overlay.classList.add('open');
            document.body.style.overflow = 'hidden';
        }

        function closeOverlay() {
            overlay.classList.remove('open');
            overlay.classList.remove('dragging');
            if (overlayImg) {
                overlay.removeChild(overlayImg);
                overlayImg = null;
            }
            document.body.style.overflow = '';
        }

        closeBtn.addEventListener('click', closeOverlay);

        // Click backdrop (but not the image or close button) to close.
        // Suppress close if the user actually dragged the image.
        overlay.addEventListener('click', function(e) {
            if (e.target === overlay && maxDisplacement < DRAG_CLOSE_THRESHOLD) {
                closeOverlay();
            }
        });

        // Wheel zoom: anchor the point under the cursor so the user zooms
        // into (or out from) the area they're pointing at, not the center.
        overlay.addEventListener('wheel', function(e) {
            e.preventDefault();
            if (!overlayImg) return;

            var vw = window.innerWidth;
            var vh = window.innerHeight;
            var oldScale = scale;
            var factor = e.deltaY < 0 ? ZOOM_STEP : 1 / ZOOM_STEP;
            scale = Math.max(ZOOM_MIN, Math.min(ZOOM_MAX, scale * factor));
            // Solve for new pan so the image point under the cursor stays
            // pinned at the cursor after the scale change. The offset from
            // the viewport center to the cursor shrinks/grows with the
            // scale ratio; we re-anchor pan accordingly.
            panX = (e.clientX - vw / 2) * (1 - scale / oldScale) + panX * scale / oldScale;
            panY = (e.clientY - vh / 2) * (1 - scale / oldScale) + panY * scale / oldScale;
            updateImageTransform();
        }, { passive: false });

        overlay.addEventListener('mousedown', function(e) {
            if (e.target !== overlayImg) return;
            isDragging = true;
            maxDisplacement = 0;
            overlay.classList.add('dragging');
            mousedownX = e.clientX;
            mousedownY = e.clientY;
            panStartX = panX;
            panStartY = panY;
            e.preventDefault();
        });

        document.addEventListener('mousemove', function(e) {
            if (!isDragging) return;
            var dx = e.clientX - mousedownX;
            var dy = e.clientY - mousedownY;
            var dist = Math.abs(dx) + Math.abs(dy);
            if (dist > maxDisplacement) maxDisplacement = dist;
            panX = panStartX + dx;
            panY = panStartY + dy;
            updateImageTransform();
        });

        document.addEventListener('mouseup', function() {
            if (!isDragging) return;
            isDragging = false;
            overlay.classList.remove('dragging');
        });

        document.addEventListener('keydown', function(e) {
            if (e.key === 'Escape' && overlay.classList.contains('open')) {
                closeOverlay();
            }
        });

        // Click any <img> in the main content area to open it.
        // Use delegation so images added later (e.g. by other scripts) also work.
        $('#content').on('click', 'img', function(e) {
            e.preventDefault();
            openOverlay($(this).attr('src'));
        });
    })();
    // ----- End image lightbox -----

    // 页面首次加载时执行一次，确保初始状态居中
    updateLayout();
    captureScrollRatio();

    // Strip the trailing period from org-exported section numbers (e.g. "1.1." -> "1.1").
    // The numbering is baked into the HTML by org-html-export, so we fix it post-render.
    $('[class^="section-number-"]').each(function() {
        var text = $(this).text();
        if (text && text.charAt(text.length - 1) === '.') {
            $(this).text(text.slice(0, -1));
        }
    });

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
