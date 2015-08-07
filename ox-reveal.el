;;; ox-reveal.el --- reveal.js Presentation Back-End for Org Export Engine

;; Copyright (C) 2013 Yujie Wen

;; Author: Yujie Wen <yjwen.ty at gmail dot com>
;; Created: 2013-04-27
;; Version: 1.0
;; Package-Requires: ((org "8.0"))
;; Keywords: outlines, hypermedia, slideshow, presentation

;; This file is not part of GNU Emacs.

;;; Copyright Notice:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; Please see "Readme.org" for detail introductions.

;; Pull request: Multiplex Support - Stephen Barrett <Stephen dot Barrewtt at scss dot tcd dot ie

;;; Code:

(require 'ox-html)
(eval-when-compile (require 'cl))

(org-export-define-derived-backend 'reveal 'html

  :menu-entry
  '(?R "Export to reveal.js HTML Presentation"
       ((?R "To file" org-reveal-export-to-html)
        (?B "To file and Browse" org-reveal-export-to-html-and-browse)))

  :options-alist
  '((:reveal-control nil "reveal_control" org-reveal-control t)
    (:reveal-progress nil "reveal_progress" org-reveal-progress t)
    (:reveal-history nil  "reveal_history" org-reveal-history t)
    (:reveal-center nil "reveal_center" org-reveal-center t)
    (:reveal-rolling-links nil "reveal_rolling_links" org-reveal-rolling-links t)
    (:reveal-slide-number nil "reveal_slide_number" org-reveal-slide-number t)
    (:reveal-keyboard nil "reveal_keyboard" org-reveal-keyboard t)
    (:reveal-overview nil "reveal_overview" org-reveal-overview t)
    (:reveal-width nil "reveal_width" org-reveal-width t)
    (:reveal-height nil "reveal_height" org-reveal-height)
    (:reveal-margin "REVEAL_MARGIN" nil org-reveal-margin t)
    (:reveal-min-scale "REVEAL_MIN_SCALE" nil org-reveal-min-scale t)
    (:reveal-max-scale "REVEAL_MAX_SCALE" nil org-reveal-max-scale t)
    (:reveal-root "REVEAL_ROOT" nil org-reveal-root t)
    (:reveal-trans "REVEAL_TRANS" nil org-reveal-transition t)
    (:reveal-speed "REVEAL_SPEED" nil org-reveal-transition-speed t)
    (:reveal-theme "REVEAL_THEME" nil org-reveal-theme t)
    (:reveal-extra-css "REVEAL_EXTRA_CSS" nil org-reveal-extra-css nil)
    (:reveal-extra-js "REVEAL_EXTRA_JS" nil org-reveal-extra-js nil)
    (:reveal-hlevel "REVEAL_HLEVEL" nil nil t)
    (:reveal-title-slide nil "reveal_title_slide" org-reveal-title-slide t)
    (:reveal-title-slide-template "REVEAL_TITLE_SLIDE_TEMPLATE" nil org-reveal-title-slide-template t)
    (:reveal-title-slide-background "REVEAL_TITLE_SLIDE_BACKGROUND" nil nil t)
    (:reveal-title-slide-background-size "REVEAL_TITLE_SLIDE_BACKGROUND_SIZE" nil nil t)
    (:reveal-title-slide-background-repeat "REVEAL_TITLE_SLIDE_BACKGROUND_REPEAT" nil nil t)
    (:reveal-title-slide-background-transition "REVEAL_TITLE_SLIDE_BACKGROUND_TRANSITION" nil nil t)
    (:reveal-mathjax nil "reveal_mathjax" org-reveal-mathjax t)
    (:reveal-mathjax-url "REVEAL_MATHJAX_URL" nil org-reveal-mathjax-url t)
    (:reveal-preamble "REVEAL_PREAMBLE" nil org-reveal-preamble t)
    (:reveal-head-preamble "REVEAL_HEAD_PREAMBLE" nil org-reveal-head-preamble t)
    (:reveal-postamble "REVEAL_POSTAMBLE" nil org-reveal-postamble t)
    (:reveal-multiplex-id "REVEAL_MULTIPLEX_ID" nil org-reveal-multiplex-id nil)
    (:reveal-multiplex-secret "REVEAL_MULTIPLEX_SECRET" nil org-reveal-multiplex-secret nil)
    (:reveal-multiplex-url "REVEAL_MULTIPLEX_URL" nil org-reveal-multiplex-url nil)
    (:reveal-multiplex-socketio-url "REVEAL_MULTIPLEX_SOCKETIO_URL" nil org-reveal-multiplex-socketio-url nil)
    (:reveal-slide-header "REVEAL_SLIDE_HEADER" nil org-reveal-slide-header t)
    (:reveal-slide-footer "REVEAL_SLIDE_FOOTER" nil org-reveal-slide-footer t)
    (:reveal-plugins "REVEAL_PLUGINS" nil nil t)
    (:reveal-default-frag-style "REVEAL_DEFAULT_FRAG_STYLE" nil org-reveal-default-frag-style t)
    (:reveal-single-file nil "reveal_single_file" org-reveal-single-file t)
    )

  :translate-alist
  '((export-block . org-reveal-export-block)
    (headline . org-reveal-headline)
    (inner-template . org-reveal-inner-template)
    (item . org-reveal-item)
    (keyword . org-reveal-keyword)
    (link . org-reveal-link)
    (plain-list . org-reveal-plain-list)
    (quote-block . org-reveal-quote-block)
    (section . org-reveal-section)
    (src-block . org-reveal-src-block)
    (template . org-reveal-template))

  :filters-alist '((:filter-parse-tree . org-reveal-filter-parse-tree))
  :export-block '("REVEAL" "NOTES")
  )

(defcustom org-reveal-root "./reveal.js"
  "The root directory of reveal.js packages. It is the directory
  within which js/reveal.js is."
  :group 'org-export-reveal)

(defcustom org-reveal-hlevel 1
  "The minimum level of headings that should be grouped into
vertical slides."
  :group 'org-export-reveal
  :type 'integer)

(defun org-reveal--get-hlevel (info)
  "Get HLevel value safely.
If option \"REVEAL_HLEVEL\" is set, retrieve integer value from it,
else get value from custom variable `org-reveal-hlevel'."
  (let ((hlevel-str (plist-get info :reveal-hlevel)))
    (if hlevel-str (string-to-number hlevel-str)
      org-reveal-hlevel)))

(defcustom org-reveal-title-slide t
  "Include a title slide."
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-title-slide-template
  "<h1>%t</h1>
<h2>%a</h2>
<h2>%e</h2>
<h2>%d</h2>"
  "Format template to specify title page slide. The format string
can contain the following escaping elements:

  %s stands for the title.
  %a stands for the author's name.
  %e stands for the author's email.
  %d stands for the date.
  %% stands for a literal %.
"
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-transition
  "default"
  "Reveal transistion style."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-transition-speed
  "default"
  "Reveal transistion speed."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-theme
  "moon"
  "Reveal theme."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-extra-js
  ""
  "URL to extra JS file."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-extra-css
  ""
  "URL to extra css file."
  :group 'org-export-reveal
  :type 'string)


(defcustom org-reveal-multiplex-id ""
  "The ID to use for multiplexing."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-multiplex-secret ""
  "The secret to use for master slide."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-multiplex-url ""
  "The url of the socketio server."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-multiplex-socketio-url
  ""
  "the url of the socketio.js library"
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-control t
  "Reveal control applet."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-progress t
  "Reveal progress applet."
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-history nil
  "Reveal history applet."
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-center t
  "Reveal center applet."
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-rolling-links nil
  "Reveal use rolling links."
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-slide-number t
  "Reveal showing slide numbers."
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-keyboard t
  "Reveal use keyboard navigation."
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-overview t
  "Reveal show overview."
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-width -1
  "Slide width"
  :group 'org-export-reveal
  :type 'integer)

(defcustom org-reveal-height -1
  "Slide height"
  :group 'org-export-reveal
  :type 'integer)

(defcustom org-reveal-margin "-1"
  "Slide margin"
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-min-scale "-1"
  "Minimum bound for scaling slide."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-max-scale "-1"
  "Maximum bound for scaling slide."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-mathjax nil
  "Enable MathJax script."
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-mathjax-url
  "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  "Default MathJax URL."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-preamble nil
  "Preamble contents."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-head-preamble nil
  "Preamble contents for head part."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-postamble nil
  "Postamble contents."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-slide-header nil
  "HTML content used as Reveal.js slide header"
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-slide-footer nil
  "HTML content used as Reveal.js slide footer"
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-default-frag-style nil
  "Default fragment style."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-plugins
  '(classList markdown zoom notes)
  "Default builtin plugins"
  :group 'org-export-reveal
  :type '(set
          (const classList)
          (const markdown)
          (const highlight)
          (const zoom)
          (const notes)
          (const search)
          (const remotes)
          (const multiplex)))

(defcustom org-reveal-single-file nil
  "Export presentation into one single HTML file, which embedded
  JS scripts and pictures."
  :group 'org-export-reveal
  :type 'boolean)

(defun if-format (fmt val)
  (if val (format fmt val) ""))

(defun frag-style (frag info)
  "Return proper fragment string according to FRAG and the default fragment style.
FRAG is the fragment style set on element, INFO is a plist
holding contextual information."
  (cond
   ((string= frag t)
    (let ((default-frag-style (plist-get info :reveal-default-frag-style)))
      (if default-frag-style (format "fragment %s" default-frag-style)
        "fragment")))
   (t (format "fragment %s" frag))))

(defun frag-class (frag info)
  "Return proper HTML string description of fragment style.
FRAG is the fragment style set on element, INFO is a plist
holding contextual information."
  (and frag
       (format " class=\"%s\"" (frag-style frag info))))

(defun org-reveal-export-block (export-block contents info)
  "Transocde a EXPORT-BLOCK element from Org to Reveal.
CONTENTS is nil. INFO is a plist holding contextual information."
  (let ((block-type (org-element-property :type export-block))
        (block-string (org-element-property :value export-block)))
    (cond ((string= block-type "NOTES")
           (format "<aside class=\"notes\">\n%s\n</aside>\n"
                   (org-export-string-as block-string 'html 'body-only)))
          ((string= block-type "HTML")
           (org-remove-indentation block-string)))))

;; Copied from org-html-headline and modified to embed org-reveal
;; specific attributes.
(defun org-reveal-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (if (org-export-low-level-p headline info)
        ;; This is a deep sub-tree: export it as in ox-html.
        (org-html-headline headline contents info)
      ;; Standard headline.  Export it as a slide
      (let* ((level (org-export-get-relative-level headline info))
	     (preferred-id (or (org-element-property :CUSTOM_ID headline)
			       (org-export-get-reference headline info)
			       (org-element-property :ID headline)))
	     (hlevel (org-reveal--get-hlevel info))
	     (header (plist-get info :reveal-slide-header))
	     (header-div (when header (format "<div class=\"slide-header\">%s</div>\n" header)))
	     (footer (plist-get info :reveal-slide-footer))
	     (footer-div (when footer (format "<div class=\"slide-footer\">%s</div>\n" footer)))
	     (first-sibling (org-export-first-sibling-p headline info))
	     (last-sibling (org-export-last-sibling-p headline info)))
        (concat
         (if (or (/= level 1) (not first-sibling))
             ;; Not the first heading. Close previou slide.
             (concat
              ;; Slide footer if any
              footer-div
              ;; Close previous slide
              "</section>\n"
              (if (<= level hlevel)
		  ;; Close previous vertical slide group.
		  "</section>\n")))
         (if (<= level hlevel)
             ;; Add an extra "<section>" to group following slides
             ;; into vertical slide group.
             "<section>\n")
         ;; Start a new slide.
         (format "<section %s%s>\n"
                 (org-html--make-attribute-string
                  `(:id ,(format "slide-%s" preferred-id)
                        :data-state ,(org-element-property :REVEAL_DATA_STATE headline)
                        :data-transition ,(org-element-property :REVEAL_DATA_TRANSITION headline)
                        :data-background ,(org-element-property :REVEAL_BACKGROUND headline)
                        :data-background-size ,(org-element-property :REVEAL_BACKGROUND_SIZE headline)
                        :data-background-repeat ,(org-element-property :REVEAL_BACKGROUND_REPEAT headline)
                        :data-background-transition ,(org-element-property :REVEAL_BACKGROUND_TRANS headline)))
                 (let ((extra-attrs (org-element-property :REVEAL_EXTRA_ATTR headline)))
                   (if extra-attrs (format " %s" extra-attrs) "")))
         ;; Slide header if any.
         header-div
         ;; The HTML content of the headline
         ;; Strip the <div> tags, if any
         (let ((html (org-html-headline headline contents info)))
           (if (string-prefix-p "<div" html)
               ;; Remove the first <div> and the last </div> tags from html
               (concat "<"
                       (mapconcat 'identity
                                  (butlast (cdr (split-string html "<" t)))
                                  "<"))
             ;; Return the HTML content unchanged
             html))
         (if (and (= level 1)
                  (org-export-last-sibling-p headline info))
             ;; Last head 1. Close all slides.
             (concat
              ;; Slide footer if any
              footer-div
              "</section>\n</section>\n")))))))

(defgroup org-export-reveal nil
  "Options for exporting Orgmode files to reveal.js HTML pressentations."
  :tag "Org Export Reveal"
  :group 'org-export)

(defun org-reveal--read-file (file)
  "Return the content of file"
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun org-reveal-stylesheets (info)
  "Return the HTML contents for declaring reveal stylesheets
using custom variable `org-reveal-root'."
  (let* ((root-path (file-name-as-directory (plist-get info :reveal-root)))
         (reveal-css (concat root-path "css/reveal.css"))
         (theme (plist-get info :reveal-theme))
         (theme-css (concat root-path "css/theme/" theme ".css"))
         ;; Local file names.
         (local-root (replace-regexp-in-string "^file:///" "" root-path))
         (local-reveal-css (concat local-root "css/reveal.css"))
         (local-theme-css (concat local-root "css/theme/" theme ".css"))
         (in-single-file (plist-get info :reveal-single-file)))
    (concat
     ;; stylesheets
     (if (and in-single-file
              (file-readable-p local-reveal-css)
              (file-readable-p local-theme-css))
         ;; CSS files exist and are readable. Embed them.
         (concat "<style type=\"text/css\">\n"
                 (org-reveal--read-file local-reveal-css)
                 "\n"
                 (org-reveal--read-file local-theme-css)
                 "</style>\n")
       ;; Fall-back to external CSS links.
       (if in-single-file
           ;; Tried to embed CSS files but failed. Print a message about possible errors.
           (error (concat "Cannot read "
                            (mapconcat 'identity
                                       (delq nil (mapcar (lambda (file) (if (not (file-readable-p file)) file))
                                                         (list local-reveal-css local-theme-css)))
                                       ", "))))
       ;; Create links to CSS files.
       (concat "<link rel=\"stylesheet\" href=\"" reveal-css "\"/>\n"
               "<link rel=\"stylesheet\" href=\"" theme-css "\" id=\"theme\"/>\n"))
     ;; extra css
     (let ((extra-css (plist-get info :reveal-extra-css)))
       (if extra-css (format "<link rel=\"stylesheet\" href=\"%s\"/>" extra-css) ""))
     ;; print-pdf
     (if in-single-file ""
       (format "
<!-- If the query includes 'print-pdf', include the PDF print sheet -->
<script>
    if( window.location.search.match( /print-pdf/gi ) ) {
        var link = document.createElement( 'link' );
        link.rel = 'stylesheet';
        link.type = 'text/css';
        link.href = '%scss/print/pdf.css';
        document.getElementsByTagName( 'head' )[0].appendChild( link );
    }
</script>
"
               root-path)))))

(defun org-reveal-mathjax-scripts (info)
  "Return the HTML contents for declaring MathJax scripts"
  (if (plist-get info :reveal-mathjax)
      ;; MathJax enabled.
      (format "<script type=\"text/javascript\" src=\"%s\"></script>\n"
              (plist-get info :reveal-mathjax-url))))

(defun org-reveal-scripts (info)
  "Return the necessary scripts for initializing reveal.js using
custom variable `org-reveal-root'."
  (let* ((root-path (file-name-as-directory (plist-get info :reveal-root)))
         (head-min-js (concat root-path "lib/js/head.min.js"))
         (reveal-js (concat root-path "js/reveal.js"))
         ;; Local files
         (local-root-path (replace-regexp-in-string "^file:///" "" root-path))
         (local-head-min-js (concat local-root-path "lib/js/head.min.js"))
         (local-reveal-js (concat local-root-path "js/reveal.js"))
         (in-single-file (plist-get info :reveal-single-file)))
    (concat
     ;; reveal.js/lib/js/head.min.js
     ;; reveal.js/js/reveal.js
     (if (and in-single-file
              (file-readable-p local-head-min-js)
              (file-readable-p local-reveal-js))
         ;; Embed scripts into HTML
         (concat "<script>\n"
                 (org-reveal--read-file local-head-min-js)
                 "\n"
                 (org-reveal--read-file local-reveal-js)
                 "\n</script>")
       ;; Fall-back to extern script links
       (if in-single-file
           ;; Tried to embed scripts but failed. Print a message about possible errors.
           (error (concat "Cannot read "
                            (mapconcat 'identity
                                       (delq nil (mapcar (lambda (file) (if (not (file-readable-p file)) file))
                                                         (list local-head-min-js local-reveal-js)))
                                       ", "))))
       (concat
        "<script src=\"" head-min-js "\"></script>\n"
        "<script src=\"" reveal-js "\"></script>\n"))
     ;; plugin headings
     "
<script>
// Full list of configuration options available here:
// https://github.com/hakimel/reveal.js#configuration
Reveal.initialize({
"
     (format "
controls: %s,
progress: %s,
history: %s,
center: %s,
slideNumber: %s,
rollingLinks: %s,
keyboard: %s,
overview: %s,
"
            (if (plist-get info :reveal-control) "true" "false")
            (if (plist-get info :reveal-progress) "true" "false")
            (if (plist-get info :reveal-history) "true" "false")
            (if (plist-get info :reveal-center) "true" "false")
            (if (plist-get info :reveal-slide-number) "true" "false")
            (if (plist-get info :reveal-rolling-links) "true" "false")
            (if (plist-get info :reveal-keyboard) "true" "false")
            (if (plist-get info :reveal-overview) "true" "false"))

     ;; slide width
     (let ((width (plist-get info :reveal-width)))
       (if (> width 0) (format "width: %d,\n" width) ""))

     ;; slide height
     (let ((height (plist-get info :reveal-height)))
       (if (> height 0) (format "height: %d,\n" height) ""))

     ;; slide margin
     (let ((margin (string-to-number (plist-get info :reveal-margin))))
       (if (>= margin 0) (format "margin: %.2f,\n" margin) ""))

     ;; slide minimum scaling factor
     (let ((min-scale (string-to-number (plist-get info :reveal-min-scale))))
       (if (> min-scale 0) (format "minScale: %.2f,\n" min-scale) ""))

     ;; slide maximux scaling factor
     (let ((max-scale (string-to-number (plist-get info :reveal-max-scale))))
       (if (> max-scale 0) (format "maxScale: %.2f,\n" max-scale) ""))

     ;; thems and transitions
     (format "
theme: Reveal.getQueryHash().theme, // available themes are in /css/theme
transition: Reveal.getQueryHash().transition || '%s', // default/cube/page/concave/zoom/linear/fade/none
transitionSpeed: '%s',\n"
             (plist-get info :reveal-trans)
             (plist-get info :reveal-speed))

     ;; multiplexing - depends on defvar 'client-multiplex'
     (when (plist-get info :reveal-multiplex-id)
       (format
"multiplex: {
    secret: %s, // null if client
    id: '%s', // id, obtained from socket.io server
    url: '%s' // Location of socket.io server
},\n"
             (if (eq client-multiplex nil)
                 (format "'%s'" (plist-get info :reveal-multiplex-secret))
               (format "null"))
             (plist-get info :reveal-multiplex-id)
             (plist-get info :reveal-multiplex-url)))

     ;; optional JS library heading
     (if in-single-file ""
       (concat
        "
// Optional libraries used to extend on reveal.js
dependencies: [
"
        ;; JS libraries
        (let* ((builtins
                '(classList
                  (format " { src: '%slib/js/classList.js', condition: function() { return !document.body.classList; } }" root-path)
                  markdown (format " { src: '%splugin/markdown/marked.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } },
 { src: '%splugin/markdown/markdown.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } }" root-path root-path)
                  highlight (format " { src: '%splugin/highlight/highlight.js', async: true, callback: function() { hljs.initHighlightingOnLoad(); } }" root-path)
                  zoom (format " { src: '%splugin/zoom-js/zoom.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                  notes (format " { src: '%splugin/notes/notes.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                  search (format " { src: '%splugin/search/search.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                  remotes (format " { src: '%splugin/remotes/remotes.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                  multiplex (format " { src: '%s', async: true },\n%s"
                                    (plist-get info :reveal-multiplex-socketio-url)
                                        ; following ensures that either client.js or master.js is included depending on defva client-multiplex value state
                                    (if (not client-multiplex)
                                        (progn
                                          (if (plist-get info :reveal-multiplex-secret)
                                              (setq client-multiplex t))
                                          (format " { src: '%splugin/multiplex/master.js', async: true }" root-path))
                                      (format " { src: '%splugin/multiplex/client.js', async: true }" root-path)))))
               (builtin-codes
                (mapcar
                 (lambda (p)
                   (eval (plist-get builtins p)))
                 (let ((buffer-plugins (plist-get info :reveal-plugins)))
                   (cond
                    ((string= buffer-plugins "") ())
                    (buffer-plugins (car (read-from-string buffer-plugins)))
                    (t org-reveal-plugins)))))
               (extra-codes (plist-get info :reveal-extra-js))
               (total-codes
                (if (string= "" extra-codes) builtin-codes
                  (append (list extra-codes) builtin-codes))))
          (mapconcat 'identity total-codes ",\n"))
        "]\n"
        )
       "\n")
     "});\n</script>\n")))

(defun org-reveal-toc (depth info)
  "Build a slide of table of contents."
  (format "<section id=\"table-of-contents\">\n%s</section>\n"
          (replace-regexp-in-string "<a href=\"#" "<a href=\"#/slide-"
                                    (org-html-toc depth info))))

(defun org-reveal-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-reveal-toc depth info)))
   ;; Document contents.
   contents))

(defun org-reveal-parse-token (key &optional value)
  "Return HTML tags or perform SIDE EFFECT according to key"
  (case (intern key)
    (split "</section>\n<section>")))

(defun org-reveal-parse-keyword-value (value)
  "According to the value content, return HTML tags to split slides."
  (let ((tokens (mapcar
                 (lambda (x) (split-string x ":"))
                 (split-string value))))
    (mapconcat
     (lambda (x) (apply 'org-reveal-parse-token x))
     tokens
     "")))

;; Copied from org-html-format-list-item. Overwrite HTML class
;; attribute when there is attr_html attributes.
(defun org-reveal-format-list-item (contents type checkbox attributes info
					     &optional term-counter-id
					     headline)
  "Format a list item into HTML."
  (let ((attr-html (cond (attributes (format " %s" (org-html--make-attribute-string attributes)))
                         (checkbox (format " class=\"%s\"" (symbol-name checkbox)))
                         (t "")))
	(checkbox (concat (org-html-checkbox checkbox info)
			  (and checkbox " ")))
	(br (org-html-close-tag "br" nil info)))
    (concat
     (case type
       (ordered
	(let* ((counter term-counter-id)
	       (extra (if counter (format " value=\"%s\"" counter) "")))
	  (concat
	   (format "<li%s%s>" attr-html extra)
	   (when headline (concat headline br)))))
       (unordered
	(let* ((id term-counter-id)
	       (extra (if id (format " id=\"%s\"" id) "")))
	  (concat
	   (format "<li%s%s>" attr-html extra)
	   (when headline (concat headline br)))))
       (descriptive
	(let* ((term term-counter-id))
	  (setq term (or term "(no term)"))
	  ;; Check-boxes in descriptive lists are associated to tag.
	  (concat (format "<dt%s>%s</dt>"
			  attr-html (concat checkbox term))
		  "<dd>"))))
     (unless (eq type 'descriptive) checkbox)
     (and contents (org-trim contents))
     (case type
       (ordered "</li>")
       (unordered "</li>")
       (descriptive "</dd>")))))

;; Copied from org-html-item, changed to call
;; org-reveal-format-list-item.
(defun org-reveal-item (item contents info)
  "Transcode an ITEM element from Org to Reveal.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list))
	 (counter (org-element-property :counter item))
         (attributes (org-export-read-attribute :attr_html item))
         ; (attributes (org-html--make-attribute-string (org-export-read-attribute :attr_html item)))
	 (checkbox (org-element-property :checkbox item))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag (org-export-data tag info)))))
    (org-reveal-format-list-item
     contents type checkbox attributes info (or tag counter))))

(defun org-reveal-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to Reveal,
and may change custom variables as SIDE EFFECT.
CONTENTS is nil. INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (case (intern key)
      (REVEAL (org-reveal-parse-keyword-value value))
      (REVEAL_HTML value))))
(defun org-reveal-embedded-svg (path)
  "Embed the SVG content into Reveal HTML."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (let ((start (re-search-forward "<[ \t\n]*svg[ \t\n]"))
          (end (re-search-forward "<[ \t\n]*/svg[ \t\n]*>")))
      (concat "<svg " (buffer-substring-no-properties start end)))))

(defun org-reveal--format-image-data-uri (link path info)
  "Generate the data URI for the image referenced by LINK."
  (let* ((ext (downcase (file-name-extension path))))
    (if (string= ext "svg")
        (org-reveal-embedded-svg path)
      (org-html-close-tag
       "img"
       (org-html--make-attribute-string
        (list :src
              (concat
               "data:image/"
               ;; Image type
               ext
               ";base64,"
               ;; Base64 content
               (with-temp-buffer
                 (insert-file-contents-literally path)
                 (base64-encode-region 1 (point-max))
                 (buffer-string)))))
       info))))

(defun org-reveal-link (link desc info)
  "Transcode a LINK object from Org to Reveal. The result is
  identical to ox-html expect for image links. When `org-reveal-single-file' is t,
the result is the Data URIs of the referenced image."
  (let* ((want-embed-image (and (plist-get info :reveal-single-file)
                                (plist-get info :html-inline-images)
                                (org-export-inline-image-p
                                 link (plist-get info :html-inline-image-rules))))
         (raw-path (org-element-property :path link))
         (clean-path (replace-regexp-in-string "^file:///" "" raw-path))
         (can-embed-image (and want-embed-image
                               (file-readable-p clean-path))))
    (if can-embed-image
        (org-reveal--format-image-data-uri link clean-path info)
      (if want-embed-image
          (error "Cannot embed image %s" raw-path)
        (org-html-link link desc info)))))

(defun org-reveal-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Reveal.

CONTENTS is the contents of the list. INFO is a plist holding
contextual information.

Extract and set `attr_html' to plain-list tag attributes."
  (let ((tag (case (org-element-property :type plain-list)
               (ordered "ol")
               (unordered "ul")
               (descriptive "dl")))
        (attrs (org-export-read-attribute :attr_html plain-list)))
    (format "<%s%s>\n%s\n</%s>\n"
            tag
            (if attrs (concat " " (org-html--make-attribute-string attrs)) "")
            contents
            tag)))

(defun org-reveal--build-pre/postamble (type info)
  "Return document preamble or postamble as a string, or nil."
  (let ((section (plist-get info (intern (format ":reveal-%s" type))))
        (spec (org-html-format-spec info)))
    (when section
      (let ((section-contents
             (if (functionp (intern section)) (funcall (intern section) info)
               ;; else section is a string.
               (format-spec section spec))))
        (when (org-string-nw-p section-contents)
           (org-element-normalize-string section-contents))))))


(defun org-reveal-section (section contents info)
  "Transcode a SECTION element from Org to Reveal.
CONTENTS holds the contents of the section. INFO is a plist
holding contextual information."
  ;; Just return the contents. No "<div>" tags.
  contents)

(defun org-reveal-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Reveal.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((buffer-plugins (plist-get info :reveal-plugins))
           (use-highlight (memq 'highlight
                                (cond
                                 ((string= buffer-plugins "") nil)
                                 (buffer-plugins (car (read-from-string buffer-plugins)))
                                 (t org-reveal-plugins))))
           (lang (org-element-property :language src-block))
           (caption (org-export-get-caption src-block))
           (code (if (not use-highlight)
                     (org-html-format-code src-block info)
                   (cl-letf (((symbol-function 'org-html-htmlize-region-for-paste)
                              #'buffer-substring))
                     (org-html-format-code src-block info))))
           (frag (org-export-read-attribute :attr_reveal src-block :frag))
           (label (let ((lbl (org-element-property :name src-block)))
                    (if (not lbl) ""
                      (format " id=\"%s\"" lbl)))))
      (if (not lang)
          (format "<pre %s%s>\n%s</pre>"
                  (or (frag-class frag info) " class=\"example\"")
                  label
                  code)
        (format
         "<div class=\"org-src-container\">\n%s%s\n</div>"
         (if (not caption) ""
           (format "<label class=\"org-src-name\">%s</label>"
                   (org-export-data caption info)))
         (if use-highlight
             (format "\n<pre%s%s><code class=\"%s\">%s</code></pre>"
                     (or (frag-class frag info) "")
                     label lang code)
           (format "\n<pre %s%s>%s</pre>"
                   (or (frag-class frag info)
                       (format " class=\"src src-%s\"" lang))
                   label code)))))))

(defun org-reveal-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to Reveal.
CONTENTS holds the contents of the block INFO is a plist holding
contextual information."
  (format "<blockquote %s>\n%s</blockquote>"
          (frag-class (org-export-read-attribute :attr_reveal quote-block :frag) info)
          contents))


(defun org-reveal-template (contents info)
  "Return complete document string after HTML conversion.
contents is the transcoded contents string.
info is a plist holding export options."
  (concat
   (format "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html>\n<html%s>\n<head>\n"
           (if-format " lang=\"%s\"" (plist-get info :language)))
   "<meta charset=\"utf-8\"/>\n"
   (if-format "<title>%s</title>\n" (org-export-data (plist-get info :title) info))
   (if-format "<meta name=\"author\" content=\"%s\"/>\n" (plist-get info :author))
   (if-format "<meta name=\"description\" content=\"%s\"/>\n" (plist-get info :description))
   (if-format "<meta name=\"keywords\" content=\"%s\"/>\n" (plist-get info :keywords))
   (org-reveal-stylesheets info)
   (org-reveal-mathjax-scripts info)
   (org-reveal--build-pre/postamble 'head-preamble info)
   "</head>
<body>\n"
   (org-reveal--build-pre/postamble 'preamble info)
"<div class=\"reveal\">
<div class=\"slides\">\n"
   (if (plist-get info :reveal-title-slide)
       (concat
        (format "<section id=\"sec-title-slide\"%s%s%s%s>\n"
                (if-format " data-background=\"%s\""
                           (plist-get info :reveal-title-slide-background))
                (if-format " data-background-size=\"%s\""
                           (plist-get info :reveal-title-slide-background-size))
                (if-format " data-background-repeat=\"%s\""
                           (plist-get info :reveal-title-slide-background-repeat))
                (if-format " data-background-transition=\"%s\""
                           (plist-get info :reveal-title-slide-background-transition)))
        (format-spec (plist-get info :reveal-title-slide-template) (org-html-format-spec info))
        "\n</section>\n")
     "")
   contents
   "</div>
</div>\n"
   (org-reveal--build-pre/postamble 'postamble info)
   (org-reveal-scripts info)
   "</body>
</html>\n"))

(defun org-reveal-filter-parse-tree (tree backend info)
  "Do filtering before parsing TREE.

Tree is the parse tree being exported. BACKEND is the export
back-end used. INFO  is a plist-used as a communication channel.

Assuming BACKEND is `reveal'.

Each `attr_reveal' attribute is mapped to corresponding
`attr_html' attributes."
  (let ((default-frag-style (plist-get info :reveal-default-frag-style)))
    (org-element-map tree (remq 'item org-element-all-elements)
      (lambda (elem) (org-reveal-append-frag elem default-frag-style))))
  ;; Return the updated tree.
  tree)

(defun org-reveal--update-attr-html (elem frag &optional frag-index)
  "Update ELEM's attr_html atrribute with reveal's
fragment attributes."
  (let ((attr-html (org-element-property :attr_html elem)))
    (when (and frag (not (string= frag "none")))
      (push (cond ((string= frag t) ":class fragment")
                  (t (format ":class fragment %s" frag)))
            attr-html)
      (when frag-index
        (push (format ":data-fragment-index %s" frag-index) attr-html)))
    (org-element-put-property elem :attr_html attr-html)))

(defun org-reveal-append-frag (elem default-style)
  "Read org-reveal's fragment attribute from ELEM and append
transformed fragment attribute to ELEM's attr_html plist."
  (let ((frag (org-export-read-attribute :attr_reveal elem :frag))
        (frag-index (org-export-read-attribute :attr_reveal elem :frag_idx)))
    (if frag
        (cond ((and (string= (org-element-type elem) 'plain-list)
                    (char-equal (string-to-char frag) ?\())
               (let* ((frag-list (car (read-from-string frag)))
                      (frag-list (if default-style
                                     (mapcar (lambda (s)
                                               "Replace t with default-style"
                                               (if (string= s t) default-style
                                                 s))
                                             frag-list)
                                   frag-list))
                      (items (org-element-contents elem)))
                 (if frag-index
                     (mapcar* 'org-reveal--update-attr-html
                              items frag-list (car (read-from-string frag-index)))
                   ;; Make frag-list tail circular
                   (nconc frag-list (last frag-list))
                   (mapcar* 'org-reveal--update-attr-html items frag-list))))
              (t (org-reveal--update-attr-html elem frag frag-index))))
    elem))

(defvar client-multiplex nil
  "used to cause generation of client html file for multiplex")

(defun org-reveal-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reveal.js HTML file."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (clientfile (org-export-output-file-name (concat "_client" extension) subtreep)))

    ; export filename_client HTML file if multiplexing
    (setq client-multiplex nil)
    (setq retfile (org-export-to-file 'reveal file
                    async subtreep visible-only body-only ext-plist))

    ; export the client HTML file if client-multiplex is set true
    ; by previous call to org-export-to-file
    (if (eq client-multiplex t)
        (org-export-to-file 'reveal clientfile
          async subtreep visible-only body-only ext-plist))
    (cond (t retfile))))

(defun org-reveal-export-to-html-and-browse
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reveal.js and browse HTML file."
  (interactive)
  (browse-url-of-file (expand-file-name (org-reveal-export-to-html async subtreep visible-only body-only ext-plist))))

;;;###autoload
(defun org-reveal-publish-to-reveal
 (plist filename pub-dir)
  "Publish an org file to Html.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'reveal filename ".html" plist pub-dir))


(provide 'ox-reveal)

;;; ox-reveal.el ends here
