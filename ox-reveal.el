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

(defun frag-style (frag)
  "Return \"fragment\" if frag is t, which indicates to use
default fragment style, otherwise return \"fragment style\"."
  (cond
   ((string= frag t) "fragment")
   (t (format "fragment %s" frag))))


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
    (:reveal-extra-css "REVEAL_EXTRA_CSS" nil nil nil)
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
    (:reveal-plugins "REVEAL_PLUGINS" nil nil t)
    )

  :translate-alist
  '((export-block . org-reveal-export-block)
    (headline . org-reveal-headline)
    (inner-template . org-reveal-inner-template)
    (item . org-reveal-item)
    (keyword . org-reveal-keyword)
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
  "Format template to specify title page slide.
See `org-html-postamble-format' for the valid elements which
can be include."
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
  "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
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

(defcustom org-reveal-plugins
  '(classList markdown highlight zoom notes)
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

(defun if-format (fmt val)
  (if val (format fmt val) ""))

(defun org-reveal-attrs-list (attrs)
  "Generate an HTML attribute list string from the list
ATTRS. ATTRS is expected to be a list of key-value pairs, ((key0
. value0) (key1 . value1) ...), and for each pair of none-nil
value, an HTML attribute statement key=\"value\" is inserted into
the result string."
  (mapconcat
   (lambda (elem)
     (let ((key (car elem))
           (value (car (cdr elem))))
       (if value (format " %s=\"%s\"" key value))))
   attrs ""))
(defun org-reveal-tag (tagname attrs content &optional sep)
  "Generate an HTML tag of form <TAGNAME ATTRS>CONTENT</TAGNAME>.  If
SEP is given, then the CONTENT is enclosed by SEP, otherwise it is enclosed by 
a '\\n'"
  (let ((sep_ (or sep "\n")))
    (format "<%s %s>%s%s%s</%s>"
          tagname                       ; The leading tagname.
          (if attrs (org-html--make-attribute-string attrs) "")
          sep_ content sep_ tagname)))

(defun frag-class (frag)
  ;; Return proper HTML string description of fragment style.
  (and frag
       (format " class=\"%s\"" (frag-style frag))))

(defun org-reveal-export-block (export-block contents info)
  "Transocde a EXPORT-BLOCK element from Org to Reveal.
CONTENTS is nil. INFO is a plist holding contextual information."
  (let ((block-type (org-element-property :type export-block))
        (block-string (org-element-property :value export-block)))
    (cond ((string= block-type "NOTES")
           (org-reveal-tag 'aside '(('class . 'notes))
                           (org-export-string-as block-string 'html 'body-only)))
          ((string= block-type "HTML")
           (org-remove-indentation block-string)))))

(defun org-reveal-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Reveal.
CONTENTS holds the contents of the headline. INFO is a plist
holding contextual information."
  ;; First call org-html-headline to get the formatted HTML contents.
  ;; Then add enclosing <section> tags to mark slides.
  (setq contents (or contents ""))
  (let* ((numberedp (org-export-numbered-headline-p headline info))
         (level (org-export-get-relative-level headline info))
         (text (org-export-data (org-element-property :title headline) info))
         (todo (and (plist-get info :with-todo-keywords)
                    (let ((todo (org-element-property :todo-keyword headline)))
                      (and todo (org-export-data todo info)))))
         (todo-type (and todo (org-element-property :todo-type headline)))
         (tags (and (plist-get info :with-tags)
                    (org-export-get-tags headline info)))
         (priority (and (plist-get info :with-priority)
                        (org-element-property :priority headline)))
         ;; Create the headline text.
         (full-text (funcall (or (plist-get info :html-format-headline-function)
                                 ;; nil function, return a suggestive error
                                 (error "Export failed. It seems you are using a stable release of Org-mode. Please use Org-reveal `stable' branch for Org-mode stable releases."))
                             todo todo-type priority text tags info)))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((org-export-low-level-p headline info)
      ;; Build the real contents of the sub-tree.
      (let* ((type (if numberedp 'ordered 'unordered))
             (itemized-body (org-reveal-format-list-item
                             contents type nil info nil 'none full-text)))
        (concat
         (and (org-export-first-sibling-p headline info)
              (org-html-begin-plain-list type))
         itemized-body
         (and (org-export-last-sibling-p headline info)
              (org-html-end-plain-list type)))))
     ;; Case 3. Standard headline.  Export it as a section.
     (t
      (let* ((level1 (format "h%d" (+ level (1- org-html-toplevel-hlevel))))
             (hlevel (org-reveal--get-hlevel info))
             (first-content (car (org-element-contents headline))))
        (concat
         (if (or (/= level 1)
                 (not (org-export-first-sibling-p headline info)))
             ;; Stop previous slide.
             "</section>\n")
         (if (eq level hlevel)
             ;; Add an extra "<section>" to group following slides
             ;; into vertical ones.
             "<section>\n")
         ;; Start a new slide.
         (format "<section%s%s>\n"
                 (org-reveal-attrs-list
                  `(("id" ,(or (org-element-property :CUSTOM_ID headline)
                                (concat "sec-"
                                        (mapconcat 'number-to-string
                                                   (org-export-get-headline-number headline info)
                                                   "-"))))
                    ("data-state" ,(org-element-property :REVEAL_DATA_STATE headline))
                    ("data-transition" ,(org-element-property :REVEAL_DATA_TRANSITION headline))
                    ("data-background" ,(org-element-property :REVEAL_BACKGROUND headline))
                    ("data-background-size" ,(org-element-property :REVEAL_BACKGROUND_SIZE headline))
                    ("data-background-repeat" ,(org-element-property :REVEAL_BACKGROUND_REPEAT headline))
                    ("data-background-transition" ,(org-element-property :REVEAL_BACKGROUND_TRANS headline))))
                   (let ((extra-attrs (org-element-property :REVEAL_EXTRA_ATTR headline)))
                     (if extra-attrs (format " %s" extra-attrs) "")))
         "\n"
         ;; The HTML content of this headline.
         (org-reveal-tag  level1 ;;"\n<h%d%s>%s</h%d>\n"
                          (let ((fragment (org-element-property :REVEAL-FRAG headline)))
                            (if fragment `(("class" ,(concat "fragment " fragment)))))
                          full-text
                          "")
         "\n"
         ;; When there is no section, pretend there is an empty
         ;; one to get the correct <div class="outline- ...>
         ;; which is needed by `org-info.js'.
         (if (not (eq (org-element-type first-content) 'section))
             (concat (org-reveal-section first-content "" info)
                     contents)
           contents)
         (if (= level hlevel)
             ;; Add an extra "</section>" to stop vertical slide
             ;; grouping.
             "</section>\n")
         (if (and (= level 1)
                  (org-export-last-sibling-p headline info))
             ;; Last head 1. Stop all slides.
             "</section>")))))))

(defgroup org-export-reveal nil
  "Options for exporting Orgmode files to reveal.js HTML pressentations."
  :tag "Org Export Reveal"
  :group 'org-export)

(defun org-reveal-stylesheets (info)
  "Return the HTML contents for declaring reveal stylesheets
using custom variable `org-reveal-root'."
  (let ((root-path (file-name-as-directory (plist-get info :reveal-root))))
    (concat
     ;; stylesheets
     (format "
<link rel=\"stylesheet\" href=\"%scss/reveal.css\"/>
<link rel=\"stylesheet\" href=\"%scss/theme/%s.css\" id=\"theme\"/>
"
             root-path root-path
             (plist-get info :reveal-theme))
     ;; extra css
     (let ((extra-css (plist-get info :reveal-extra-css)))
       (if extra-css (format "<link rel=\"stylesheet\" href=\"%s\"/>" extra-css) ""))
     ;; print-pdf
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
             root-path))))

(defun org-reveal-mathjax-scripts (info)
  "Return the HTML contents for declaring MathJax scripts"
  (if (plist-get info :reveal-mathjax)
      ;; MathJax enabled.
      (format "<script type=\"text/javascript\" src=\"%s\"></script>\n"
              (plist-get info :reveal-mathjax-url))))

(defun org-reveal-scripts (info)
  "Return the necessary scripts for initializing reveal.js using
custom variable `org-reveal-root'."
  (let* ((root-path (file-name-as-directory (plist-get info :reveal-root))))
    (concat
     ;; reveal.js/lib/js/head.min.js
     ;; reveal.js/js/reveal.js
     (format "
<script src=\"%slib/js/head.min.js\"></script>
<script src=\"%sjs/reveal.js\"></script>
"
             root-path root-path)
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
     "
// Optional libraries used to extend on reveal.js
dependencies: [
"
     ;; JS libraries
     (let* ((builtins
             '(classList (format " { src: '%slib/js/classList.js', condition: function() { return !document.body.classList; } }" root-path)
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
     "
]
});
</script>\n")))

(defun org-reveal-toc-headlines-r (headlines info prev_level hlevel prev_x prev_y)
  "Generate toc headline text recursively."
  (let* ((headline (car headlines))
         (text (org-export-data (org-element-property :title headline) info))
         (level (org-export-get-relative-level headline info))
         (x (if (<= level hlevel) (+ prev_x 1) prev_x))
         (y (if (<= level hlevel) 0 (+ prev_y 1)))
         (remains (cdr headlines))
         (remain-text
          (if remains
              ;; Generate text for remain headlines
              (org-reveal-toc-headlines-r remains info level hlevel x y)
            "")))
    (concat
     (cond
      ((> level prev_level)
       ;; Need to start a new level of unordered list
       "<ul>\n")
      ((< level prev_level)
       ;; Need to end previous list item and the whole list.
       "</li>\n</ul>\n")
      (t
       ;; level == prev_level, Need to end previous list item.
       "</li>\n"))
     (format "<li>\n<a href=\"#%s\">%s</a>\n%s"
             (or (org-element-property :CUSTOM_ID headline)
                 (concat "sec-" (mapconcat 'number-to-string
                                           (org-export-get-headline-number headline info)
                                           "-")))
             text remain-text))))

(defun org-reveal-toc-headlines (headlines info)
  "Generate the Reveal.js contents for headlines in table of contents.
Add proper internal link to each headline."
  (let ((level (org-export-get-relative-level (car headlines) info))
        (hlevel (org-reveal--get-hlevel info)))
    (concat
     (format "<h2>%s</h2>"
             (org-export-translate "Table of Contents" :html info))
     (org-reveal-toc-headlines-r headlines info 0 hlevel 1 1)
     (if headlines "</li>\n</ul>\n" ""))))


(defun org-reveal-toc (depth info)
  "Build a slide of table of contents."
  (let ((headlines (org-export-collect-headlines info depth)))
    (and headlines
         (format "<section id=\"table-of-contents\">\n%s</section>\n"
                 (org-reveal-toc-headlines headlines info)))))

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
  "Transcode a KEYWORD element from Org to HTML,
and may change custom variables as SIDE EFFECT.
CONTENTS is nil. INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (case (intern key)
      (REVEAL (org-reveal-parse-keyword-value value))
      (REVEAL_HTML value))))

(defun org-reveal-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Reveal.

CONTENTS is the contents of the list. INFO is a plist holding
contextual information.

Extract and set `attr_html' to plain-list tag attributes."
  (org-reveal-tag (case (org-element-property :type plain-list)
                    (ordered "ol")
                    (unordered "ul")
                    (descriptive "dl"))
                  (org-export-read-attribute :attr_html plain-list)
                  contents))

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
    (let ((lang (org-element-property :language src-block))
          (caption (org-export-get-caption src-block))
          (code (org-html-format-code src-block info))
          (frag (org-export-read-attribute :attr_reveal src-block :frag))
          (label (let ((lbl (org-element-property :name src-block)))
                   (if (not lbl) ""
                     (format " id=\"%s\""
                             (org-export-solidify-link-text lbl))))))
      (if (not lang)
          (format "<pre %s%s>\n%s</pre>"
                  (or (frag-class frag) " class=\"example\"")
                  label
                  code)
        (format
         "<div class=\"org-src-container\">\n%s%s\n</div>"
         (if (not caption) ""
           (format "<label class=\"org-src-name\">%s</label>"
                   (org-export-data caption info)))
         (format "\n<pre %s%s>%s</pre>"
                 (or (frag-class frag)
                     (format " class=\"src src-%s\"" lang))
                 label code))))))

(defun org-reveal-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to Reveal.
CONTENTS holds the contents of the block INFO is a plist holding
contextual information."
  (format "<blockquote %s>\n%s</blockquote>"
          (frag-class (org-export-read-attribute :attr_reveal quote-block :frag))
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
  (org-element-map tree (remq 'item org-element-all-elements)
    'org-reveal-append-frag)
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

(defun org-reveal-append-frag (elem)
  "Read org-reveal's fragment attribute from ELEM and append
transformed fragment attribute to ELEM's attr_html plist."
  (let ((frag (org-export-read-attribute :attr_reveal elem :frag))
        (frag-index (org-export-read-attribute :attr_reveal elem :frag_idx)))
    (if frag
        (cond ((and (string= (org-element-type elem) 'plain-list)
                    (char-equal (string-to-char frag) ?\())
               (let ((frag-list (car (read-from-string frag)))
                     (items (org-element-contents elem)))
                 (if frag-index
                     (mapcar* 'org-reveal--update-attr-html
                              items frag-list (car (read-from-string frag-index)))
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
  (org-publish-org-to 'reval filename ".html" plist pub-dir))


(provide 'ox-reveal)

;;; ox-reveal.el ends here
