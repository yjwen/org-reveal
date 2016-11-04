;;; ox-reveal.el --- reveal.js Presentation Back-End for Org Export Engine

;; Copyright (C) 2013 Yujie Wen

;; Author: Yujie Wen <yjwen.ty at gmail dot com>
;; Created: 2013-04-27
;; Version: 1.0
;; Package-Requires: ((org "20150330"))
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
(require 'cl)

(org-export-define-derived-backend 'reveal 'html

  :menu-entry
  '(?R "Export to reveal.js HTML Presentation"
       ((?R "To file" org-reveal-export-to-html)
        (?B "To file and browse" org-reveal-export-to-html-and-browse)
        (?S "Current subtree to file" org-reveal-export-current-subtree)))

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
    (:reveal-extra-css "REVEAL_EXTRA_CSS" nil org-reveal-extra-css newline)
    (:reveal-extra-js "REVEAL_EXTRA_JS" nil org-reveal-extra-js nil)
    (:reveal-hlevel "REVEAL_HLEVEL" nil nil t)
    (:reveal-title-slide nil "reveal_title_slide" org-reveal-title-slide t)
    (:reveal-title-slide-background "REVEAL_TITLE_SLIDE_BACKGROUND" nil nil t)
    (:reveal-title-slide-background-size "REVEAL_TITLE_SLIDE_BACKGROUND_SIZE" nil nil t)
    (:reveal-title-slide-background-position "REVEAL_TITLE_SLIDE_BACKGROUND_POSITION" nil nil t)
    (:reveal-title-slide-background-repeat "REVEAL_TITLE_SLIDE_BACKGROUND_REPEAT" nil nil t)
    (:reveal-title-slide-background-transition "REVEAL_TITLE_SLIDE_BACKGROUND_TRANSITION" nil nil t)
    (:reveal-default-slide-background "REVEAL_DEFAULT_SLIDE_BACKGROUND" nil nil t)
    (:reveal-default-slide-background-size "REVEAL_DEFAULT_SLIDE_BACKGROUND_SIZE" nil nil t)
    (:reveal-default-slide-background-position "REVEAL_DEFAULT_SLIDE_BACKGROUND_POSITION" nil nil t)
    (:reveal-default-slide-background-repeat "REVEAL_DEFAULT_SLIDE_BACKGROUND_REPEAT" nil nil t)
    (:reveal-default-slide-background-transition "REVEAL_DEFAULT_SLIDE_BACKGROUND_TRANSITION" nil nil t)
    (:reveal-mathjax-url "REVEAL_MATHJAX_URL" nil org-reveal-mathjax-url t)
    (:reveal-preamble "REVEAL_PREAMBLE" nil org-reveal-preamble t)
    (:reveal-head-preamble "REVEAL_HEAD_PREAMBLE" nil org-reveal-head-preamble newline)
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
    (:reveal-init-script "REVEAL_INIT_SCRIPT" nil org-reveal-init-script space)
    (:reveal-highlight-css "REVEAL_HIGHLIGHT_CSS" nil org-reveal-highlight-css nil)
    )

  :translate-alist
  '((headline . org-reveal-headline)
    (inner-template . org-reveal-inner-template)
    (item . org-reveal-item)
    (keyword . org-reveal-keyword)
    (link . org-reveal-link)
    (latex-environment . org-reveal-latex-environment)
    (latex-fragment . (lambda (frag contents info)
                        (setq info (plist-put info :reveal-mathjax t))
                        (org-html-latex-fragment frag contents info)))
    (plain-list . org-reveal-plain-list)
    (quote-block . org-reveal-quote-block)
    (section . org-reveal-section)
    (src-block . org-reveal-src-block)
    (special-block . org-reveal-special-block)
    (template . org-reveal-template))

  :filters-alist '((:filter-parse-tree . org-reveal-filter-parse-tree))
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

(defcustom org-reveal-title-slide 'auto
  "Non-nil means insert a title slide.

When set to `auto', an automatic title slide is generated. When
set to a string, use this string as a format string for title
slide, where the following escaping elements are allowed:

  %s stands for the title
  %a stands for the author's name.
  %e stands for the author's email.
  %d stands for the date.
  %% stands for a literal %."
  :group 'org-export-reveal
  :type '(choice (const :tag "No title slide" nil)
                 (const :tag "Auto title slide" 'auto)
                 (string :tag "Custom title slide")))

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

(defcustom org-reveal-slide-number "c"
  "Reveal showing slide numbers."
  :group 'org-export-reveal
  :type 'string)

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
  "Obsolete. Org-reveal enable mathjax when it find latex
content."
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

(defcustom org-reveal-init-script nil
  "Custom script that will be passed to Reveal.initialize."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-highlight-css "%r/lib/css/zenburn.css"
  "Hightlight.js CSS file."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-note-key-char "n"
  "If not nil, org-reveal-note-key-char's value is registered as
  the key character to Org-mode's structure completion for
  Reveal.js notes. When `<' followed by the key character are
  typed and then the completion key is pressed, which is usually
  `TAB', \"#+BEGIN_NOTES\" and \"#+END_NOTES\" is inserted.

  The default value is \"n\". Set the variable to nil to disable
  registering the completion"
  :group 'org-export-reveal
  :type 'string)

(defvar org-reveal--last-slide-section-tag ""
  "Variable to cache the section tag from the last slide. ")

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

(defun org-reveal-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Reveal.
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information.

If the block type is 'NOTES', transcode the block into a
Reveal.js slide note. Otherwise, export the block as by the HTML
exporter."
  (let ((block-type (org-element-property :type special-block)))
    (if (string= block-type "NOTES")
        (format "<aside class=\"notes\">\n%s\n</aside>\n" contents)
      (org-html-special-block special-block contents info))))

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
	     (last-sibling (org-export-last-sibling-p headline info))
             (default-slide-background (plist-get info :reveal-default-slide-background))
             (default-slide-background-size (plist-get info :reveal-default-slide-background-size))
             (default-slide-background-position (plist-get info :reveal-default-slide-background-position))
             (default-slide-background-repeat (plist-get info :reveal-default-slide-background-repeat))
             (default-slide-background-transition (plist-get info :reveal-default-slide-background-transition))
             (slide-section-tag (format "<section %s%s>\n"
                                        (org-html--make-attribute-string
                                         `(:id ,(format "slide-%s" preferred-id)
                                           :data-transition ,(org-element-property :REVEAL_DATA_TRANSITION headline)
                                           :data-state ,(org-element-property :REVEAL_DATA_STATE headline)
                                           :data-background ,(or (org-element-property :REVEAL_BACKGROUND headline)
                                                                 default-slide-background)
                                           :data-background-size ,(or (org-element-property :REVEAL_BACKGROUND_SIZE headline)
                                                                      default-slide-background-size)
                                           :data-background-position ,(or (org-element-property :REVEAL_BACKGROUND_POSITION headline)
                                                                          default-slide-background-position)
                                           :data-background-repeat ,(or (org-element-property :REVEAL_BACKGROUND_REPEAT headline)
                                                                        default-slide-background-repeat)
                                           :data-background-transition ,(or (org-element-property :REVEAL_BACKGROUND_TRANS headline)
                                                                            default-slide-background-transition)))
                                        (let ((extra-attrs (org-element-property :REVEAL_EXTRA_ATTR headline)))
                                          (if extra-attrs (format " %s" extra-attrs) ""))))
             (ret (concat
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
                       ;; into vertical slide group. Transition override
                       ;; attributes are attached at this level, too.
                       (let ((attrs
                              (org-html--make-attribute-string
                               `(:data-transition ,(org-element-property :REVEAL_DATA_TRANSITION headline)))))
                         (if (string= attrs "")
                             "<section>\n"
                           (format "<section %s>\n" attrs))))
                   ;; Start a new slide.
                   (prog1
                       slide-section-tag
                     ;; Cache the current slide's section tag, except the id attr
                     (setq org-reveal--last-slide-section-tag
                           (replace-regexp-in-string "id\\s-*=\\s-*[\][\"].*?[\][\"]"
                                                     "" slide-section-tag)))
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
                        "</section>\n</section>\n")))))
        ret))))

(defgroup org-export-reveal nil
  "Options for exporting Orgmode files to reveal.js HTML pressentations."
  :tag "Org Export Reveal"
  :group 'org-export)

(defun org-reveal--read-file (file)
  "Return the content of file"
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun org-reveal--file-url-to-path (url)
  "Convert URL that points to local files to file path."
  (replace-regexp-in-string
   (if (string-equal system-type "windows-nt") "^file:///" "^file://")
   "" url))

(defun org-reveal--css-label (in-single-file file-name style-id)
  "Generate an HTML label for including a CSS file, if
  IN-SINGLE-FILE is t, the content of FILE-NAME is embedded,
  otherwise, a `<link>' label is generated."
  (when (and file-name (not (string= file-name "")))
    (if in-single-file
        ;; Single-file
        (let ((local-file-name (org-reveal--file-url-to-path file-name)))
          (if (file-readable-p local-file-name)
              (concat "<style type=\"text/css\">\n"
                      (org-reveal--read-file local-file-name)
                      "\n</style>\n")
            ;; But file is not readable.
            (error "Cannot read %s" file-name)))
      ;; Not in-single-file
      (concat "<link rel=\"stylesheet\" href=\"" file-name "\""
              (if style-id  (format " id=\"%s\"" style-id))
              "/>\n"))))

(defun org-reveal-stylesheets (info)
  "Return the HTML contents for declaring reveal stylesheets
using custom variable `org-reveal-root'."
  (let* ((root-path (file-name-as-directory (plist-get info :reveal-root)))
         (reveal-css (concat root-path "css/reveal.css"))
         (theme (plist-get info :reveal-theme))
         (theme-css (concat root-path "css/theme/" theme ".css"))
         (extra-css (plist-get info :reveal-extra-css))
         (in-single-file (plist-get info :reveal-single-file)))
    (concat
     ;; Default embedded style sheets
     "<style type=\"text/css\">
.underline { text-decoration: underline; }
</style>
"
     ;; stylesheets
     (mapconcat (lambda (elem) (org-reveal--css-label in-single-file (car elem) (cdr elem)))
                (append (list (cons reveal-css nil)
                              (cons theme-css "theme"))
                        (mapcar (lambda (a) (cons a nil))
                                (split-string extra-css "\n")))
                "\n")

     ;; Include CSS for highlight.js if necessary
     (if (org-reveal--using-highlight.js info)
         (format "<link rel=\"stylesheet\" href=\"%s\"/>" 
                 (format-spec (plist-get info :reveal-highlight-css)
                              `((?r . ,(directory-file-name root-path))))))
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
         (local-root-path (org-reveal--file-url-to-path root-path))
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
            (let ((slide-number (plist-get info :reveal-slide-number)))
              (if slide-number (format "'%s'" slide-number)
                "false"))
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
                 (let ((buffer-plugins (condition-case e
                                           (car (read-from-string (plist-get info :reveal-plugins)))
                                         (end-of-file nil)
                                         (wrong-type-argument nil))))
                   (or (and buffer-plugins (listp buffer-plugins) buffer-plugins)
                       org-reveal-plugins))))
               (extra-codes (plist-get info :reveal-extra-js))
               (total-codes
                (if (string= "" extra-codes) builtin-codes
                  (append (list extra-codes) builtin-codes))))
          (mapconcat 'identity total-codes ",\n"))
        "]\n"
         ))
        (let ((init-script (plist-get info :reveal-init-script)))
          (if init-script (concat (if in-single-file "" ",") init-script)))
     "});\n</script>\n")))

(defun org-reveal-toc (depth info)
  "Build a slide of table of contents."
  (let ((toc (org-html-toc depth info)))
    (if toc
        (format "<section id=\"table-of-contents\">\n%s</section>\n"
                (replace-regexp-in-string "<a href=\"#" "<a href=\"#/slide-" toc)))))

(defun org-reveal-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when (and depth
                (not (plist-get info :reveal-subtree)))
       (org-reveal-toc depth info)))
   ;; Document contents.
   contents))

(defun org-reveal-parse-token (key &optional value)
  "Return HTML tags or perform SIDE EFFECT according to key.
Use the previous section tag as the tag of the split section. "
  (case (intern key)
    (split (format "</section>\n%s" org-reveal--last-slide-section-tag))))

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
		 (format "<dd%s>" attr-html)))))
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
      (REVEAL_HTML value)
      (HTML value))))
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
        (org-combine-plists
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
                  (buffer-string))))
         ;; Get attribute list from parent element
         ;; Copied from ox-html.el
         (let* ((parent (org-export-get-parent-element link))
                (link (let ((container (org-export-get-parent link)))
                        (if (and (eq (org-element-type container) 'link)
                                 (org-html-inline-image-p link info))
                            container
                          link))))
           (and (eq (org-element-map parent 'link 'identity info t) link)
                (org-export-read-attribute :attr_html parent)))))
       info))))

(defun org-reveal-link (link desc info)
  "Transcode a LINK object from Org to Reveal. The result is
  identical to ox-html expect for image links. When `org-reveal-single-file' is t,
the result is the Data URIs of the referenced image."
  (let* ((want-embed-image (and (plist-get info :reveal-single-file)
                                (plist-get info :html-inline-images)
                                (string= "file" (org-element-property :type link))
                                (org-export-inline-image-p
                                 link (plist-get info :html-inline-image-rules))))
         (raw-path (org-element-property :path link))
         (clean-path (org-reveal--file-url-to-path raw-path))
         (can-embed-image (and want-embed-image
                               (file-readable-p clean-path))))
    (if can-embed-image
        (org-reveal--format-image-data-uri link clean-path info)
      (if want-embed-image
          (error "Cannot embed image %s" raw-path)
        (replace-regexp-in-string "<a href=\"#" "<a href=\"#/slide-"
                                  (org-html-link link desc info))))))

(defun org-reveal-latex-environment (latex-env contents info)
  "Transcode a LaTeX environment from Org to Reveal.

LATEX-ENV is the Org element. CONTENTS is the contents of the environment. INFO is a plist holding contextual information "
  (setq info (plist-put info :reveal-mathjax t))
  (let ((attrs (org-export-read-attribute :attr_html latex-env)))
    (format "<div%s>\n%s\n</div>\n"
            (if attrs (concat " " (org-html--make-attribute-string attrs)) "")
            (org-html-latex-environment latex-env contents info))))

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
    (format "%s<%s%s>\n%s\n</%s>%s"
            (if (string= org-html-checkbox-type 'html) "<form>" "")
            tag
            (if attrs (concat " " (org-html--make-attribute-string attrs)) "")
            contents
            tag
            (if (string= org-html-checkbox-type 'html) "</form>" ""))))

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

(defun org-reveal--using-highlight.js (info)
  "Check whether highlight.js plugin is enabled."
  (let ((reveal-plugins (condition-case e
                            (car (read-from-string (plist-get info :reveal-plugins)))
                          (end-of-file nil)
                          (wrong-type-argument nil))))
    (memq 'highlight (or (and reveal-plugins (listp reveal-plugins) reveal-plugins)
                         org-reveal-plugins))))

(defun org-reveal-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Reveal.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((use-highlight (org-reveal--using-highlight.js info))
           (lang (org-element-property :language src-block))
           (caption (org-export-get-caption src-block))
           (code (if (not use-highlight)
                     (org-html-format-code src-block info)
                   (cl-letf (((symbol-function 'org-html-htmlize-region-for-paste)
                              #'buffer-substring))
                     (org-html-format-code src-block info))))
           (frag (org-export-read-attribute :attr_reveal src-block :frag))
	   (code-attribs (or (org-export-read-attribute
			 :attr_reveal src-block :code_attribs) ""))
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
             (format "\n<pre%s%s><code class=\"%s\" %s>%s</code></pre>"
                     (or (frag-class frag info) "")
                     label lang code-attribs code)
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


(defun org-reveal--auto-title-slide-template (info)
  "Generate the automatic title slide template."
  (let* ((spec (org-html-format-spec info))
         (title (org-export-data (plist-get info :title) info))
         (author (cdr (assq ?a spec)))
         (email (cdr (assq ?e spec)))
         (date (cdr (assq ?d spec))))
    (concat
     (when (and (plist-get info :with-title)
                (org-string-nw-p title))
       (concat "<h1 class=\"title\">" title "</h1>"))
     (when (and (plist-get info :with-author)
                (org-string-nw-p author))
       (concat "<h2 class=\"author\">" author "</h2>"))
     (when (and (plist-get info :with-email)
                (org-string-nw-p email))
       (concat "<h2 class=\"email\">" email "</h2>"))
     (when (and (plist-get info :with-date)
                (org-string-nw-p date))
       (concat "<h2 class=\"date\">" date "</h2>"))
     (when (plist-get info :time-stamp-file)
       (concat "<p class=\"date\">"
               (org-html--translate "Created" info)
               ": "
               (format-time-string
                (plist-get info :html-metadata-timestamp-format))
               "</p>")))))

(defun org-reveal-template (contents info)
  "Return complete document string after HTML conversion.
contents is the transcoded contents string.
info is a plist holding export options."
  (concat
   (format "<!DOCTYPE html>\n<html%s>\n<head>\n"
           (if-format " lang=\"%s\"" (plist-get info :language)))
   "<meta charset=\"utf-8\"/>\n"
   (if-format "<title>%s</title>\n" (org-export-data (plist-get info :title) info))
   (if-format "<meta name=\"author\" content=\"%s\"/>\n" (plist-get info :author))
   (if-format "<meta name=\"description\" content=\"%s\"/>\n" (plist-get info :description))
   (if-format "<meta name=\"keywords\" content=\"%s\"/>\n" (plist-get info :keywords))
   (org-reveal-stylesheets info)
   (org-reveal-mathjax-scripts info)
   (org-reveal--build-pre/postamble 'head-preamble info)
   (org-element-normalize-string (plist-get info :html-head))
   (org-element-normalize-string (plist-get info :html-head-extra))
   "</head>
<body>\n"
   (org-reveal--build-pre/postamble 'preamble info)
   "<div class=\"reveal\">
<div class=\"slides\">\n"
   ;; Title slides
   (let ((title-slide (plist-get info :reveal-title-slide)))
     (when (and title-slide (not (plist-get info :reveal-subtree)))
       (let ((title-slide-background (plist-get info :reveal-title-slide-background))
             (title-slide-background-size (plist-get info :reveal-title-slide-background-size))
             (title-slide-background-position (plist-get info :reveal-title-slide-background-position))
             (title-slide-background-repeat (plist-get info :reveal-title-slide-background-repeat))
             (title-slide-background-transition (plist-get info :reveal-title-slide-background-transition)))
         (concat "<section id=\"sec-title-slide\""
                 (when title-slide-background
                   (concat " data-background=\"" title-slide-background "\""))
                 (when title-slide-background-size
                   (concat " data-background-size=\"" title-slide-background-size "\""))
                 (when title-slide-background-position
                   (concat " data-background-position=\"" title-slide-background-position "\""))
                 (when title-slide-background-repeat
                   (concat " data-background-repeat=\"" title-slide-background-repeat "\""))
                 (when title-slide-background-transition
                   (concat " data-background-transition=\"" title-slide-background-transition "\""))
                 ">"
                 (cond ((eq title-slide nil) nil)
                       ((stringp title-slide) (format-spec title-slide (org-html-format-spec info)))
                       ((eq title-slide 'auto) (org-reveal--auto-title-slide-template info)))
                 "\n</section>\n"))))
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

(defun org-reveal--update-attr-html (elem frag default-style &optional frag-index)
  "Update ELEM's attr_html attribute with reveal's
fragment attributes."
  (let ((attr-html (org-element-property :attr_html elem)))
    (when (and frag (not (string= frag "none")))
      (push (cond ((string= frag t)
                   (if default-style (format ":class fragment %s" default-style)
                     ":class fragment"))
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
                              items frag-list default-style (car (read-from-string frag-index)))
                   (let* ((last-frag (car (last frag-list)))
                          (tail-list (mapcar (lambda (a) last-frag)
                                             (number-sequence (+ (length frag-list) 1)
                                                              (length items))))
                          (default-style-list
                            (mapcar (lambda (a) default-style)
                                    (number-sequence 1 (length items)))))
                     (nconc frag-list tail-list)
                     (mapcar* 'org-reveal--update-attr-html items frag-list default-style-list)))))
              (t (org-reveal--update-attr-html elem frag default-style frag-index)))
      elem)))

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

(defun org-reveal-export-current-subtree
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current subtree to a Reveal.js HTML file."
  (interactive)
  (org-narrow-to-subtree)
  (let ((ret (org-reveal-export-to-html async subtreep visible-only body-only (plist-put ext-plist :reveal-subtree t))))
    (widen)
    ret))

;;;###autoload
(defun org-reveal-publish-to-reveal
 (plist filename pub-dir)
  "Publish an org file to Html.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'reveal filename ".html" plist pub-dir))

;; Register auto-completion for speaker notes.
(when org-reveal-note-key-char
  (add-to-list 'org-structure-template-alist
               (list org-reveal-note-key-char "#+BEGIN_NOTES\n\?\n#+END_NOTES")))

(provide 'ox-reveal)

;;; ox-reveal.el ends here
