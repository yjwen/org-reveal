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
    (:reveal-extra-css "REVEAL_EXTRA_CSS" nil nil nil)
    (:reveal-extra-js "REVEAL_EXTRA_JS" nil nil nil)
    (:reveal-hlevel "REVEAL_HLEVEL" nil nil t)
    (:reveal-title-slide-template "REVEAL_TITLE_SLIDE_TEMPLATE" nil org-reveal-title-slide-template t)
    (:reveal-mathjax nil "reveal_mathjax" org-reveal-mathjax t)
    (:reveal-mathjax-url "REVEAL_MATHJAX_URL" nil org-reveal-mathjax-url t)
    (:reveal-preamble "REVEAL_PREAMBLE" nil org-reveal-preamble t)
    (:reveal-head-preamble "REVEAL_HEAD_PREAMBLE" nil org-reveal-head-preamble t)
    (:reveal-postamble "REVEAL_POSTAMBLE" nil org-reveal-postamble t)
    )

  :translate-alist
  '((export-block . org-reveal-export-block)
    (headline . org-reveal-headline)
    (inner-template . org-reveal-inner-template)
    (item . org-reveal-item)
    (keyword . org-reveal-keyword)
    (paragraph . org-reveal-paragraph)
    (section . org-reveal-section)
    (src-block . org-reveal-src-block)
    (template . org-reveal-template))

  :export-block '("REVEAL" "NOTES")
  )

(defcustom org-reveal-root "./reveal.js"
  "The root directory of reveal.js packages. It is the directory
  within which js/reveal.min.js is."
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
  "default"
  "Reveal theme."
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


(defun if-format (fmt val)
  (if val (format fmt val) ""))

(defun frag-class (frag)
  ;; Return proper HTML string description of fragment style.
  (cond
   ((string= frag t) " class=\"fragment\"")
   (frag (format " class=\"fragment %s\"" frag))))
  

(defun org-reveal-export-block (export-block contents info)
  "Transocde a EXPORT-BLOCK element from Org to Reveal.
CONTENTS is nil. NFO is a plist holding contextual information."
  (let ((block-type (org-element-property :type export-block))
        (block-string (org-element-property :value export-block)))
    (cond ((string= block-type "NOTES")
           (concat
            "<aside class=\"notes\">\n"
            (org-export-string-as block-string 'html 'body-only)
            "</aside>"))
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
	 (full-text (org-html-format-headline--wrap headline info)))
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
      (let* ((level1 (+ level (1- org-html-toplevel-hlevel)))
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
         (format "<section id=\"%s\" %s%s%s%s%s%s%s>\n"
                 (or (org-element-property :CUSTOM_ID headline)
                     (concat "sec-" (mapconcat 'number-to-string
                                               (org-export-get-headline-number headline info)
                                               "-")))
                 (if-format " data-state=\"%s\"" (org-element-property :REVEAL_DATA_STATE headline))
                 (if-format " data-transition=\"%s\"" (org-element-property :REVEAL_DATA_TRANSITION headline))
                 (if-format " data-background=\"%s\"" (org-element-property :REVEAL_BACKGROUND headline))
                 (if-format " data-background-size=\"%s\"" (org-element-property :REVEAL_BACKGROUND_SIZE headline))
                 (if-format " data-background-repeat=\"%s\"" (org-element-property :REVEAL_BACKGROUND_REPEAT headline))
                 (if-format " data-background-transition=\"%s\"" (org-element-property :REVEAL_BACKGROUND_TRANS headline))
                 (if-format " %s" (org-element-property :REVEAL_EXTRA_ATTR headline)))
         ;; The HTML content of this headline.
         (format "\n<h%d%s>%s</h%d>\n"
                 level1
                 (if-format " class=\"fragment %s\""
                            (org-element-property :REVEAL-FRAG headline))
                 full-text
                 level1)
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
  :tag "Org Export reveal"
  :group 'org-export)

(defun org-reveal--append-path (dir-name path-name)
  "Append `path-name' to the end of `dir-name' to form a legal path name."
  (concat (file-name-as-directory dir-name) path-name))

(defun org-reveal--append-pathes (dir-name pathes)
  "Append all the path names in `pathes' to the end of `dir-name'
to form a legal path name."
  (if pathes
      (org-reveal--append-pathes
       (org-reveal--append-path dir-name (car pathes))
       (cdr pathes))
    dir-name))


(defun org-reveal-stylesheets (info)
  "Return the HTML contents for declaring reveal stylesheets
using custom variable `org-reveal-root'."
  (let* ((root-path (plist-get info :reveal-root))
         (css-dir-name (org-reveal--append-path root-path "css"))
         (min-css-file-name (org-reveal--append-path css-dir-name "reveal.min.css"))
         (theme-file (format "%s.css" (plist-get info :reveal-theme)))
         (theme-path (org-reveal--append-path css-dir-name "theme"))
         (theme-full (org-reveal--append-path theme-path theme-file))
         (extra-css (plist-get info :reveal-extra-css))
         (extra-css-link-tag (if extra-css
                                 (format "<link rel=\"stylesheet\" href=\"%s\"/>" extra-css)
                               ""))
         (pdf-css (org-reveal--append-pathes css-dir-name '("print" "pdf.css"))))
    (format "<link rel=\"stylesheet\" href=\"%s\"/>
<link rel=\"stylesheet\" href=\"%s\" id=\"theme\"/>
%s
<!-- If the query includes 'print-pdf', include the PDF print sheet -->
<script>
    if( window.location.search.match( /print-pdf/gi ) ) {
        var link = document.createElement( 'link' );
        link.rel = 'stylesheet';
        link.type = 'text/css';
        link.href = '%s';
        document.getElementsByTagName( 'head' )[0].appendChild( link );
    }
</script>
"
                min-css-file-name theme-full extra-css-link-tag
                pdf-css)))

(defun org-reveal-mathjax-scripts (info)
  "Return the HTML contents for declaring MathJax scripts"
  (if (plist-get info :reveal-mathjax)
      ;; MathJax enabled.
      (format "<script type=\"text/javascript\" src=\"%s\"></script>\n"
              (plist-get info :reveal-mathjax-url))))

(defun org-reveal-scripts (info)
  "Return the necessary scripts for initializing reveal.js using
custom variable `org-reveal-root'."
  (let* ((root-path (plist-get info :reveal-root))
         (root-dir-name (file-name-as-directory root-path))
         (lib-dir-name (org-reveal--append-path root-path "lib"))
         (lib-js-dir-name (org-reveal--append-path lib-dir-name "js"))
         (plugin-dir-name (org-reveal--append-path root-path "plugin"))
         (markdown-dir-name (org-reveal--append-path plugin-dir-name "markdown"))
         (extra-js (plist-get info :reveal-extra-js)))
    (concat
     (format "<script src=\"%s\"></script>\n<script src=\"%s\"></script>\n"
             (org-reveal--append-path lib-js-dir-name "head.min.js")
             (org-reveal--append-pathes root-path '("js" "reveal.min.js")))
     "<script>\n"
     (format "
        		// Full list of configuration options available here:
        		// https://github.com/hakimel/reveal.js#configuration
        		Reveal.initialize({
        			controls: %s,
        			progress: %s,
        			history: %s,
        			center: %s,
                                slideNumber: %s,
        			rollingLinks: %s,
        			keyboard: %s,
        			overview: %s,
        			%s // slide width
        			%s // slide height
        			%s // slide margin
        			%s // slide minimum scaling factor
        			%s // slide maximum scaling factor


        			theme: Reveal.getQueryHash().theme, // available themes are in /css/theme
        			transition: Reveal.getQueryHash().transition || '%s', // default/cube/page/concave/zoom/linear/fade/none
        			transitionSpeed: '%s',\n"
             (if (plist-get info :reveal-control) "true" "false")
             (if (plist-get info :reveal-progress) "true" "false")
             (if (plist-get info :reveal-history) "true" "false")
             (if (plist-get info :reveal-center) "true" "false")
             (if (plist-get info :reveal-slide-number) "true" "false")
             (if (plist-get info :reveal-rolling-links) "true" "false")
             (if (plist-get info :reveal-keyboard) "true" "false")
             (if (plist-get info :reveal-overview) "true" "false")
             (let ((width (plist-get info :reveal-width)))
               (if (> width 0) (format "width: %d," width)
                 ""))
             (let ((height (plist-get info :reveal-height)))
               (if (> height 0) (format "height: %d," height)
                 ""))
             (let ((margin (string-to-number (plist-get info :reveal-margin))))
               (if (>= margin 0) (format "margin: %.2f," margin)
                 ""))
             (let ((min-scale (string-to-number (plist-get info :reveal-min-scale))))
               (if (> min-scale 0) (format "minScale: %.2f," min-scale)
                 ""))
             (let ((max-scale (string-to-number (plist-get info :reveal-max-scale))))
               (if (> max-scale 0) (format "maxScale: %.2f," max-scale)
                 ""))

             (plist-get info :reveal-trans)
             (plist-get info :reveal-speed))
     (format "
        			// Optional libraries used to extend on reveal.js
        			dependencies: [
        				{ src: '%s', condition: function() { return !document.body.classList; } }
        				,{ src: '%s', condition: function() { return !!document.querySelector( '[data-markdown]' ); } }
        				,{ src: '%s', condition: function() { return !!document.querySelector( '[data-markdown]' ); } }
        				,{ src: '%s', async: true, callback: function() { hljs.initHighlightingOnLoad(); } }
        				,{ src: '%s', async: true, condition: function() { return !!document.body.classList; } }
        				,{ src: '%s', async: true, condition: function() { return !!document.body.classList; } }
        				// { src: '%s', async: true, condition: function() { return !!document.body.classList; } }
        				// { src: '%s', async: true, condition: function() { return !!document.body.classList; } }
         				%s
        			]
        		});\n"
               (org-reveal--append-path lib-js-dir-name "classList.js")
               (org-reveal--append-path markdown-dir-name "showdown.js")
               (org-reveal--append-path markdown-dir-name "markdown.js")
               (org-reveal--append-pathes plugin-dir-name '("highlight" "highlight.js"))
               (org-reveal--append-pathes plugin-dir-name '("zoom-js" "zoom.js"))
               (org-reveal--append-pathes plugin-dir-name '("notes" "notes.js"))
               (org-reveal--append-pathes plugin-dir-name '("search" "search.js"))
               (org-reveal--append-pathes plugin-dir-name '("remotes" "remotes.js"))
               (if extra-js (concat "," extra-js) ""))
                "</script>\n")))

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
         (format "<section>\n%s</section>\n"
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

(defun org-reveal-format-list-item
  (contents type checkbox info &optional term-counter-id frag headline)
  "Format a list item into Reveal.js HTML."
  (let* (;; The argument definition of `org-html-checkbox' differs
         ;; between Org-mode master and 8.2.5h. To deal both cases,
         ;; both argument definitions are tried here.
         (org-checkbox (condition-case nil
                           (org-html-checkbox checkbox info)
                         ;; In case of wrong number of arguments, try another one
                         ((debug wrong-number-of-arguments) (org-html-checkbox checkbox))))
         (checkbox (concat org-checkbox (and checkbox " "))))
    (concat
     (case type
       (ordered
        (concat
         "<li"
         (if-format " value=\"%s\"" term-counter-id)
         (frag-class frag)
         ">"
         (if headline (concat headline "<br/>"))))
       (unordered
        (concat
         "<li"
         (frag-class frag)
         ">"
         (if headline (concat headline "<br/>"))))
       (descriptive
        (concat
         "<dt"
         (frag-class frag)
         "><b>"
         (concat checkbox (or term-counter-id "(no term)"))
         "</b></dt><dd>")))
     (unless (eq type 'descriptive) checkbox)
     contents
     (case type
       (ordered "</li>")
       (unordered "</li>")
       (descriptive "</dd>")))))

(defun org-reveal-item (item contents info)
  "Transcode an ITEM element from Org to Reveal.
CONTENTS holds the contents of the item. INFO is aplist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item))
         (type (org-element-property :type plain-list))
         (counter (org-element-property :counter item))
         (checkbox (org-element-property :checkbox item))
         (tag (let ((tag (org-element-property :tag item)))
                (and tag (org-export-data tag info))))
         (frag (org-export-read-attribute :attr_reveal plain-list :frag)))
    (org-reveal-format-list-item
     contents type checkbox info (or tag counter) frag)))

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


(defun org-reveal-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to HTML,
and may change custom variables as SIDE EFFECT.
CONTENTS is nil. INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (case (intern key)
      (REVEAL (org-reveal-parse-keyword-value value))
      (REVEAL_HTML value))))

(defun org-reveal-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Reveal HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let ((parent (org-export-get-parent paragraph)))
    (cond
     ((and (eq (org-element-type parent) 'item)
	   (= (org-element-property :begin paragraph)
	      (org-element-property :contents-begin parent)))
      ;; leading paragraph in a list item have no tags
      contents)
     ((org-html-standalone-image-p paragraph info)
      ;; standalone image
      (let ((frag (org-export-read-attribute :attr_reveal paragraph :frag)))
        (if frag
            (progn
              ;; This is ugly; need to update if the output from
              ;; org-html-format-inline-image changes.
              (unless (string-match "class=\"figure\"" contents)
                (error "Unexpected HTML output for image!"))
              (replace-match (concat "class=\"figure fragment " frag " \"") t t contents))
          contents)))
     (t (format "<p%s>\n%s</p>"
                (or (frag-class (org-export-read-attribute :attr_reveal paragraph :frag))
                    "")
                contents)))))

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
<div class=\"slides\">
<section>
"
   (format-spec (plist-get info :reveal-title-slide-template) (org-html-format-spec info))
   "</section>\n"
   contents
   "</div>
</div>\n"
   (org-reveal--build-pre/postamble 'postamble info)
   (org-reveal-scripts info)
   "</body>
</html>\n"))



(defun org-reveal-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reveal.js HTML file."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep)))
    (org-export-to-file 'reveal file
      async subtreep visible-only body-only ext-plist)))

(defun org-reveal-export-to-html-and-browse
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reveal.js and browse HTML file."
  (interactive)
  (browse-url-of-file (expand-file-name (org-reveal-export-to-html async subtreep visible-only body-only ext-plist))))

(provide 'ox-reveal)

;;; ox-reveal.el ends here
