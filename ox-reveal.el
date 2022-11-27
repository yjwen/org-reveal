;;; ox-reveal.el --- reveal.js Presentation Back-End for Org Export Engine

;; Copyright (C) 2013 Yujie Wen

;; Author: Yujie Wen <yjwen.ty at gmail dot com>
;; Created: 2013-04-27
;; Version: 1.0
;; Package-Requires: ((org "8.3"))
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
(require 'cl-lib)

(org-export-define-derived-backend 'reveal 'html

  :menu-entry
  '(?R "Export to reveal.js HTML Presentation"
       ((?R "To file" org-reveal-export-to-html)
        (?B "To file and browse" org-reveal-export-to-html-and-browse)
        (?S "Current subtree to file" org-reveal-export-current-subtree)))

  :options-alist
  '((:reveal-control nil "reveal_control" nil t)
    (:reveal-progress nil "reveal_progress" nil t)
    (:reveal-history nil  "reveal_history" nil t)
    (:reveal-center nil "reveal_center" nil t)
    (:reveal-rolling-links nil "reveal_rolling_links" nil t)
    (:reveal-slide-number nil "reveal_slide_number" nil t)
    (:reveal-keyboard nil "reveal_keyboard" nil t)
    (:reveal-overview nil "reveal_overview" nil t)
    (:reveal-width nil "reveal_width" nil t)
    (:reveal-height nil "reveal_height" nil t)
    (:reveal-margin "REVEAL_MARGIN" nil nil t)
    (:reveal-min-scale "REVEAL_MIN_SCALE" nil nil t)
    (:reveal-max-scale "REVEAL_MAX_SCALE" nil nil t)
    (:reveal-root "REVEAL_ROOT" nil org-reveal-root t)
    (:reveal-trans "REVEAL_TRANS" nil nil t)
    (:reveal-speed "REVEAL_SPEED" nil nil t)
    (:reveal-theme "REVEAL_THEME" nil org-reveal-theme t)
    (:reveal-extra-css "REVEAL_EXTRA_CSS" nil org-reveal-extra-css newline)
    (:reveal-extra-js "REVEAL_EXTRA_JS" nil org-reveal-extra-js nil)
    (:reveal-extra-initial-js "REVEAL_EXTRA_INITIAL_JS" nil org-reveal-extra-initial-js newline)
    (:reveal-hlevel "REVEAL_HLEVEL" nil nil t)
    (:reveal-title-slide "REVEAL_TITLE_SLIDE" "reveal_title_slide" org-reveal-title-slide newline)
    (:reveal-slide-global-header nil "reveal_global_header" org-reveal-global-header t)
    (:reveal-slide-global-footer nil "reveal_global_footer" org-reveal-global-footer t)
    (:reveal-title-slide-background "REVEAL_TITLE_SLIDE_BACKGROUND" nil nil t)
    (:reveal-title-slide-background-size "REVEAL_TITLE_SLIDE_BACKGROUND_SIZE" nil nil t)
    (:reveal-title-slide-background-position "REVEAL_TITLE_SLIDE_BACKGROUND_POSITION" nil nil t)
    (:reveal-title-slide-background-repeat "REVEAL_TITLE_SLIDE_BACKGROUND_REPEAT" nil nil t)
    (:reveal-title-slide-background-transition "REVEAL_TITLE_SLIDE_BACKGROUND_TRANSITION" nil nil t)
    (:reveal-title-slide-background-opacity "REVEAL_TITLE_SLIDE_BACKGROUND_OPACITY" nil nil t)
    (:reveal-title-slide-state "REVEAL_TITLE_SLIDE_STATE" nil nil t)
    (:reveal-toc-slide-background "REVEAL_TOC_SLIDE_BACKGROUND" nil nil t)
    (:reveal-toc-slide-background-size "REVEAL_TOC_SLIDE_BACKGROUND_SIZE" nil nil t)
    (:reveal-toc-slide-background-position "REVEAL_TOC_SLIDE_BACKGROUND_POSITION" nil nil t)
    (:reveal-toc-slide-background-repeat "REVEAL_TOC_SLIDE_BACKGROUND_REPEAT" nil nil t)
    (:reveal-toc-slide-background-transition "REVEAL_TOC_SLIDE_BACKGROUND_TRANSITION" nil nil t)
    (:reveal-toc-slide-background-opacity "REVEAL_TOC_SLIDE_BACKGROUND_OPACITY" nil nil t)
    (:reveal-default-slide-background "REVEAL_DEFAULT_SLIDE_BACKGROUND" nil nil t)
    (:reveal-default-slide-background-size "REVEAL_DEFAULT_SLIDE_BACKGROUND_SIZE" nil nil t)
    (:reveal-default-slide-background-position "REVEAL_DEFAULT_SLIDE_BACKGROUND_POSITION" nil nil t)
    (:reveal-default-slide-background-repeat "REVEAL_DEFAULT_SLIDE_BACKGROUND_REPEAT" nil nil t)
    (:reveal-default-slide-background-opacity "REVEAL_DEFAULT_SLIDE_BACKGROUND_OPACITY" nil nil t)
    (:reveal-default-slide-background-transition "REVEAL_DEFAULT_SLIDE_BACKGROUND_TRANSITION" nil nil t)
    (:reveal-mathjax-url "REVEAL_MATHJAX_URL" nil org-reveal-mathjax-url t)
    (:reveal-preamble "REVEAL_PREAMBLE" nil org-reveal-preamble t)
    (:reveal-head-preamble "REVEAL_HEAD_PREAMBLE" nil org-reveal-head-preamble newline)
    (:reveal-postamble "REVEAL_POSTAMBLE" nil org-reveal-postamble t)
    (:reveal-prologue "REVEAL_PROLOGUE" nil org-reveal-prologue t)
    (:reveal-epilogue "REVEAL_EPILOGUE" nil org-reveal-epilogue t)
    (:reveal-multiplex-id "REVEAL_MULTIPLEX_ID" nil org-reveal-multiplex-id nil)
    (:reveal-multiplex-secret "REVEAL_MULTIPLEX_SECRET" nil org-reveal-multiplex-secret nil)
    (:reveal-multiplex-url "REVEAL_MULTIPLEX_URL" nil org-reveal-multiplex-url nil)
    (:reveal-multiplex-socketio-url "REVEAL_MULTIPLEX_SOCKETIO_URL" nil org-reveal-multiplex-socketio-url nil)
    (:reveal-slide-header "REVEAL_SLIDE_HEADER" nil org-reveal-slide-header t)
    (:reveal-slide-footer "REVEAL_SLIDE_FOOTER" nil org-reveal-slide-footer t)
    (:reveal-plugins "REVEAL_PLUGINS" nil nil t)
    (:reveal-external-plugins "REVEAL_EXTERNAL_PLUGINS" nil nil space)
    (:reveal-default-frag-style "REVEAL_DEFAULT_FRAG_STYLE" nil org-reveal-default-frag-style t)
    (:reveal-single-file nil "reveal_single_file" org-reveal-single-file t)
    (:reveal-extra-script "REVEAL_EXTRA_SCRIPT" nil org-reveal-extra-script space)
    (:reveal-extra-script-src "REVEAL_EXTRA_SCRIPT_SRC" nil org-reveal-extra-script-src split)
    (:reveal-extra-script-before-src "REVEAL_EXTRA_SCRIPT_BEFORE_SRC" nil org-reveal-extra-script-before-src split)
    (:reveal-init-options "REVEAL_INIT_OPTIONS" nil org-reveal-init-options newline)
    (:reveal-highlight-css "REVEAL_HIGHLIGHT_CSS" nil org-reveal-highlight-css nil)
    (:reveal-reveal-js-version "REVEAL_REVEAL_JS_VERSION" nil nil t)
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
  :group 'org-export-reveal
  :type 'string)

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

  %t stands for the title
  %a stands for the author's name.
  %e stands for the author's email.
  %d stands for the date.
  %% stands for a literal %."
  :group 'org-export-reveal
  :type '(choice (const :tag "No title slide" nil)
                 (const :tag "Auto title slide" 'auto)
                 (string :tag "Custom title slide")))
(defcustom org-reveal-init-options
  ""
  "Reveal.js initialization options, JS code snippet to be
embedded into Reveal.initialize()."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-transition
  "default"
  "Reveal transition style."
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

(defcustom org-reveal-extra-initial-js
  ""
  "Scripts to be embedded into reveal.js initialization."
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

(defcustom org-reveal-mathjax-url
  "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
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

(defcustom org-reveal-prologue nil
  "Prologue contents to be inserted between opening <div reveal> and <div slides>."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-epilogue nil
  "Prologue contents to be inserted between closing <div reveal> and <div slides>."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-slide-header nil
  "HTML content used as Reveal.js slide header"
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-global-header nil
  "If non nil, slide header defined in org-reveal-slide-header
  is displayed also on title and toc slide"
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-global-footer nil
  "If non nil, slide footer defined in org-reveal-slide-footer
  is displayed also on title and toc slide"
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-slide-footer nil
  "HTML content used as Reveal.js slide footer"
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-default-frag-style nil
  "Default fragment style."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-plugins
  '(markdown zoom notes)
  "Default builtin plugins"
  :group 'org-export-reveal
  :type '(set
          (const markdown)
          (const highlight)
          (const zoom)
          (const notes)
          (const search)
          (const remotes)
          (const multiplex)))

(defcustom org-reveal-external-plugins nil
  "Additional third-party Plugins to load with reveal.

* When \"REVEAL_REVEAL_JS_VERSION\" is lower than 4

Each entry should contain a name and an expression of the form
\"{src: '%srelative/path/from/reveal/root', async:true/false,condition: jscallbackfunction(){}}\"
Note that some plugins have dependencies such as jquery; these must be included here as well,
BEFORE the plugins that depend on them.

* When \"REVEAL_REVEAL_JS_VERSION\" is 4 or higher

The value should be an association list where the key of an entry
is the name of the RevealJS plugin (e.g. RevealHighlight), and
the value is either a string or a list of strings. Each string is
going to be translated to an <script> tag in the output HTML.

Example:

(setq org-reveal-external-plugins
      '((CopyCode .
                  (\"https://cdn.jsdelivr.net/npm/reveal.js-copycode@1.0.2/plugin/copycode/copycode.js\"
                   \"https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js\"))))
"
  :group 'org-export-reveal
  :type 'alist)

(defcustom org-reveal-single-file nil
  "Export presentation into one single HTML file, which embedded
  JS scripts and pictures."
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-extra-script nil
  "Custom script that will be passed added to the script block, after Reveal.initialize."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-extra-script-src '()
  "Custom script source that will be embedded in a <script src> tag, after the call to Reveal.initialize()."
  :group 'org-export-reveal
  :type 'list)

(defcustom org-reveal-extra-script-before-src '()
  "Custom script source that will be embedded in a <script src> tag, before the call to Reveal.initialize()."
  :group 'org-export-reveal
  :type 'list)

(defcustom org-reveal-highlight-css "%r/plugin/highlight/zenburn.css"
  "Highlight.js CSS file."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-klipsify-src nil
  "Set to non-nil if you would like to make source code blocks editable in exported presentation."
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-klipse-css "https://storage.googleapis.com/app.klipse.tech/css/codemirror.css"
  "Location of the codemirror css file for use with klipse."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-klipse-js "https://storage.googleapis.com/app.klipse.tech/plugin_prod/js/klipse_plugin.min.js"
  "location of the klipse js source code."
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-slide-id-head "slide-"
  "The heading string for slide ID"
  :group 'org-export-reveal
  :type 'string)

(defcustom org-reveal-ignore-speaker-notes nil
  "Ignore speaker notes."
  :group 'org-export-reveal
  :type 'boolean)

(defcustom org-reveal-reveal-js-version nil
  "Reveal.js version. Determine the locations of reveal.js scripts, CSS and the"
  :group 'org-export-reveal
  :type '(radio (const :tag "Reveal.js 4.0 and later" 4)
                (const :tag "Reveal.js 3.x and before" 3)
                (const :tag "Automatic" nil)))

(defun org-reveal--get-reveal-js-version (info)
  "Get reveal.js version value safely.
If option \"REVEAL_REVEAL_JS_VERSION\" is set, retrieve integer
value from it, else get value from custom variable
`org-reveal-reveal-js-version'."
  (let ((version-str (plist-get info :reveal-reveal-js-version)))
    (if version-str (string-to-number version-str)
      org-reveal-reveal-js-version)))

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

(defun frag-class (elem info)
  "Return proper HTML string description of fragment style.
BLOCK is the element, INFO is a plist holding contextual
information."
  (let ((frag (org-export-read-attribute :attr_reveal elem :frag)))
    (and frag
	 (format " class=\"%s\"%s"
		 (frag-style frag info)
		 (let ((frag-index (org-export-read-attribute :attr_reveal elem :frag_idx)))
		   (if frag-index
		       (format " data-fragment-index=\"%s\"" frag-index)
		     ""))))))

(defun org-reveal-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Reveal.
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information.

If the block type is 'NOTES', transcode the block into a
Reveal.js slide note. Otherwise, export the block as by the HTML
exporter."
  (let ((block-type (org-element-property :type special-block)))
    (if (string= (upcase block-type) "NOTES")
	(if org-reveal-ignore-speaker-notes
	    ""
          (format "<aside class=\"notes\">\n%s\n</aside>\n" contents))
      (org-html-special-block special-block contents info))))

(defun org-reveal-is-background-img (info)
  "Check if a background string is an image format or not."
  (and info
       (string-match-p
	"\\(\\(^\\(http\\|https\\|file\\)://\\)\\|\\(\\\.\\(svg\\|png\\|jpg\\|jpeg\\|gif\\|bmp\\)\\([?#\\\s]\\|$\\)\\)\\)"
	info)))

(defun org-reveal-slide-section-tag (headline info for-split)
  "Generate the <section> tag for a slide."
  (let* ((preferred-id (or (org-element-property :CUSTOM_ID headline)
			   (org-export-get-reference headline info)))
	 (default-slide-background (plist-get info :reveal-default-slide-background))
	 (default-slide-background-size (plist-get info :reveal-default-slide-background-size))
         (default-slide-background-position (plist-get info :reveal-default-slide-background-position))
         (default-slide-background-repeat (plist-get info :reveal-default-slide-background-repeat))
         (default-slide-background-transition (plist-get info :reveal-default-slide-background-transition))
	 (slide-background-iframe (org-element-property :REVEAL_BACKGROUND_IFRAME headline))
	 )
    (if slide-background-iframe
	;; for iframe, we will wrap the slide in a new div
	;; all the background configuration of the original slide will be used for this new div
	(format "<section %s data-background-interactive>\n<div %s>\n"
		(org-html--make-attribute-string
		 `(:id ,(concat org-reveal-slide-id-head
				preferred-id
				(if for-split "-split" ""))
		       :class ,(org-element-property :HTML_CONTAINER_CLASS headline)
		       :data-transition ,(org-element-property :REVEAL_DATA_TRANSITION headline)
		       :data-state ,(org-element-property :REVEAL_DATA_STATE headline) ;; 3.9 legacy
		       :data-background-iframe ,slide-background-iframe))
		(concat
		 "style=\""
		 (let ((attr (or (org-element-property :REVEAL_BACKGROUND headline)
				 default-slide-background)))
		   (if (org-reveal-is-background-img attr)
		       (format "background-image: url('%s'); " attr)
		     (if-format "background: %s; " attr)))
		 (let ((attr (or (org-element-property :REVEAL_BACKGROUND_REPEAT headline)
				 default-slide-background-repeat)))
		   (if-format "background-repeat: %s; " attr))
		 (let ((attr (or (org-element-property :REVEAL_BACKGROUND_SIZE headline)
				 default-slide-background)))
		   (if-format "background-size: %s; " attr))
		 (let ((attr (or (org-element-property :REVEAL_BACKGROUND_POSITION headline)
				 default-slide-background)))
		   (if-format "position: %s; " attr))
		 (let ((attr (or (org-element-property :REVEAL_BACKGROUND_OPACITY headline)
				 default-slide-background)))
		   (if-format "opacity: %s; " attr))
		 (let ((extra-attrs (org-element-property :REVEAL_EXTRA_ATTR headline)))
		   (if-format "%s;" extra-attrs))
		 "\""))
      (format "<section %s%s>\n"
	      (org-html--make-attribute-string
	       `(:id ,(concat org-reveal-slide-id-head
			      preferred-id
			      (if for-split "-split" ""))
		     :class ,(org-element-property :HTML_CONTAINER_CLASS headline)
		     :data-transition ,(org-element-property :REVEAL_DATA_TRANSITION headline)
		     :data-state ,(org-element-property :REVEAL_DATA_STATE headline) ;; 3.9 legacy
		     :data-background ,(or (org-element-property :REVEAL_BACKGROUND headline)
					   default-slide-background)
		     :data-background-size ,(or (org-element-property :REVEAL_BACKGROUND_SIZE headline)
						default-slide-background-size)
		     :data-background-position ,(or (org-element-property :REVEAL_BACKGROUND_POSITION headline)
						    default-slide-background-position)
		     :data-background-repeat ,(or (org-element-property :REVEAL_BACKGROUND_REPEAT headline)
						  default-slide-background-repeat)
		     :data-background-transition ,(or (org-element-property :REVEAL_BACKGROUND_TRANS headline)
						      default-slide-background-transition)
		     :data-background-opacity
		     ,(or (org-element-property :REVEAL_BACKGROUND_OPACITY headline)
			  (plist-get info :reveal-default-slide-background-opacity))))
	      (let ((extra-attrs (org-element-property :REVEAL_EXTRA_ATTR headline)))
		(if-format " %s" extra-attrs))))))

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
	     (section-number (mapconcat #'number-to-string
					(org-export-get-headline-number headline info)
					"-"))
	     (hlevel (org-reveal--get-hlevel info))
	     (header (plist-get info :reveal-slide-header))
	     (header-div (when header (format "<div class=\"slide-header\">%s</div>\n" header)))
	     (footer (plist-get info :reveal-slide-footer))
	     (footer-div (when footer (format "<div class=\"slide-footer\">%s</div>\n" footer)))
	     (first-sibling (org-export-first-sibling-p headline info))
	     (last-sibling (org-export-last-sibling-p headline info)))
        (concat
         (if (or (/= level 1) (not first-sibling))
             ;; Not the first heading. Close previous slide.
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
         (org-reveal-slide-section-tag headline info nil)
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
	      (if (org-element-property :REVEAL_BACKGROUND_IFRAME headline)
		  "</div>")
              "</section>\n</section>\n")
	   (if (org-element-property :REVEAL_BACKGROUND_IFRAME headline)
		  "</div>")))))))

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

;; Choose path by reveal.js version
;;
;; Side-effect: When org-reveal-reveal-js-version is nil, updated it
;; to determined version if possible
(defun org-reveal--choose-path (root-path version file-path-0 file-path-1)
  (if (or (eq version 4) ;; Explict reveal.js 4.0
          (and (not version)
               ;; Automatic location. Choose an existing path
               (let ((root-file-path
                      ;; root-path could be a local file path or a URL. Try to
                      ;; extract the local root path from root-path
                      (cond ((string-prefix-p "file://" root-path)
                             ;; A local file URL
                             (substring root-path 7))
                            ((or (string-prefix-p "http://" root-path)
                                 (string-prefix-p "https://" root-path))
                             ;; A remote URL
                             nil)
                            ;; Otherwise, assuming it is a local file path
                            (t root-path))))
                 (or (not root-file-path) ;; Not a local URL, assuming file-path-0 exists
                     (when (file-exists-p (concat root-file-path file-path-0))
                       (setq org-reveal-reveal-js-version 4)
                       t)))))
      (concat root-path file-path-0)
    (concat root-path file-path-1)))

(defun org-reveal-root-path (info)
  (file-name-as-directory (plist-get info :reveal-root)))
(defun org-reveal-stylesheets (info)
  "Return the HTML contents for declaring reveal stylesheets
using custom variable `org-reveal-root'."
  (let* ((root-path (org-reveal-root-path info))
         (version (org-reveal--get-reveal-js-version info))
         (reveal-css (org-reveal--choose-path root-path version "dist/reveal.css" "css/reveal.css"))
         (theme (plist-get info :reveal-theme))
         (theme-css (if (or (string-prefix-p "http://" theme)
                            (string-prefix-p "https://" theme)
                            (string-prefix-p "file://" theme))
                        ;; theme is just the URL to a custom theme CSS
                        theme
                      (org-reveal--choose-path root-path
                                               version
                                               (concat "dist/theme/" theme ".css")
                                               (concat "css/theme/" theme ".css"))))
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
     (if (or in-single-file (eq version 4)) ""
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

(defun org-reveal--get-plugins (info)
  (let ((buffer-plugins (condition-case e
                            (car (read-from-string (plist-get info :reveal-plugins)))
                          (end-of-file nil)
                          (wrong-type-argument nil))))
    (or (and buffer-plugins (listp buffer-plugins) buffer-plugins)
        org-reveal-plugins)))

(defun org-reveal--legacy-dependency (root-path plugins info)
  (concat
      "
// Optional libraries used to extend on reveal.js
dependencies: [
"
      ;; JS libraries
      (let* ((builtins
              '(
                classList (format " { src: '%slib/js/classList.js', condition: function() { return !document.body.classList; } }" root-path)
                markdown (format " { src: '%splugin/markdown/marked.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } },
 { src: '%splugin/markdown/markdown.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } }" root-path root-path)
                highlight (format " { src: '%splugin/highlight/highlight.js', async: true, callback: function() { hljs.initHighlightingOnLoad(); } }" root-path)
                zoom (format " { src: '%splugin/zoom-js/zoom.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                notes (format " { src: '%splugin/notes/notes.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                search (format " { src: '%splugin/search/search.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                remotes (format " { src: '%splugin/remotes/remotes.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                ;; multiplex setup for reveal.js 3.x
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
              (mapcar (lambda (p)
                        (eval (plist-get builtins p)))
                      plugins))
             (external-plugins
              (append
               ;; Global setting
               (cl-loop for (key . value) in org-reveal-external-plugins
                        collect (org-reveal--replace-first-%s value root-path ))
               ;; Local settings
               (let ((local-plugins (plist-get info :reveal-external-plugins)))
                 (and local-plugins
                      (list (org-reveal--replace-first-%s local-plugins root-path))))))

             (all-plugins (if external-plugins (append external-plugins builtin-codes) builtin-codes))
             (extra-codes (plist-get info :reveal-extra-js))
             (total-codes
              (if (string= "" extra-codes) all-plugins (append (list extra-codes) all-plugins))))
        (mapconcat 'identity total-codes ",\n"))
      "]\n"))

(defun org-reveal--script-tag-by-file-name (fname in-single-file)
  "Create a <script> tag for including scripts from the given
file name. If in single file mode, the <script> tag encloses the
contents of the file, otherwise it is a tag pointing to the file"
  (if in-single-file
      (let ((local-fname (org-reveal--file-url-to-path fname)))
        (if (file-readable-p local-fname)
            ;; Embed script into HTML
            (concat "<script>\n"
                    (org-reveal--read-file local-fname)
                    "\n</script>\n")
          ;; Cannot read fname, just error out
          (error (concat "Cannot generate single file presentation due to "
                         local-fname
                         " is not readable"))))
    (format "<script src=\"%s\"></script>\n" fname)))

(defun org-reveal--script-tags-by-auto-file-names (fnames in-single-file)
  "Create multiple <script> tags for multiple file names, but
  also accept single file name and create only one tag."
  (if (stringp fnames)
      (org-reveal--script-tag-by-file-name fnames in-single-file)
    (mapconcat (lambda (fname)
                 (org-reveal--script-tag-by-file-name fname in-single-file))
               fnames
               "")))

(defun org-reveal--multi-level-mapconcat (func sequence seperator)
  "Multilevel mapconcat. FUNC is applied to each element of seq
  unless the element itself is a list, in which case func is
  lifted and applied."
  (mapconcat (lambda (e)
               (if (listp e)
                   (org-reveal--multi-level-mapconcat func e seperator)
                 (funcall func e)))
             sequence seperator))

(defun org-reveal-scripts (info)
  "Return the necessary scripts for initializing reveal.js using
custom variable `org-reveal-root'."
  (let* ((root-path (org-reveal-root-path info))
         (version (org-reveal--get-reveal-js-version info))
         (reveal-js (org-reveal--choose-path root-path version "dist/reveal.js" "js/reveal.js"))
         ;; Local files
         (local-root-path (org-reveal--file-url-to-path root-path))
         (local-reveal-js (org-reveal--choose-path local-root-path version "dist/reveal.js" "js/reveal.js"))
         (plugins (org-reveal--get-plugins info))
         (in-single-file (plist-get info :reveal-single-file))
         (reveal-4-plugin (if (eq 4 version)
                              (org-reveal-plugin-scripts-4 plugins info in-single-file)
                            (cons nil nil))))
    (concat
     ;; reveal.js/js/reveal.js
     (org-reveal--script-tag-by-file-name local-reveal-js in-single-file)
     ;; plugin headings
     (if-format "%s\n" (car reveal-4-plugin))

     ;; Extra <script src="..."></script> tags before the call to Reveal.initialize()
     (org-reveal--script-tags-by-auto-file-names (plist-get info :reveal-extra-script-before-src)
                                                 in-single-file)

     ;; Reveal.initialize
     (let ((reveal-4-plugin-statement (cdr reveal-4-plugin))
           (init-options (plist-get info :reveal-init-options))
           (multiplex-statement
            ;; multiplexing - depends on defvar 'client-multiplex'
            (let ((multiplex-id (plist-get info :reveal-multiplex-id)))
              (when (not (string-empty-p multiplex-id))        ;Multiplex setup found
                (concat
                 (format "multiplex: {
    secret: %s, // null if client
    id: '%s', // id, obtained from socket.io server
    url: '%s' // Location of socket.io server
},\n"
                         (if (eq client-multiplex nil)
                             (format "'%s'" (plist-get info :reveal-multiplex-secret))
                           (format "null"))
                         multiplex-id
                         (plist-get info :reveal-multiplex-url))
                 (let ((url (plist-get info :reveal-multiplex-url)))
                   (format "dependencies: [ { src: '%s/socket.io/socket.io.js', async: true }, { src: '%s/%s', async: true } ]"
                           url url
                           (if client-multiplex "client.js"
                             (progn
                               (setq client-multiplex t)
                               "master.js")))))
                )))
           (extra-initial-js-statement (plist-get info :reveal-extra-initial-js))
           (legacy-dependency-statement
            (unless (or in-single-file (eq version 4))
              (org-reveal--legacy-dependency root-path plugins info))))
       (format "
<script>
// Full list of configuration options available here:
// https://github.com/hakimel/reveal.js#configuration
Reveal.initialize({
%s
});
%s
</script>
"
               (mapconcat 'identity
                          (seq-filter (lambda (x) ;; Remove nil and ""
                                        (and x (not (string= x ""))))
                                      (list reveal-4-plugin-statement
                                            init-options
                                            multiplex-statement
                                            (org-reveal--replace-first-%s extra-initial-js-statement root-path)
                                            legacy-dependency-statement))
                          ",\n")
               ;; Extra initialization scripts
               (or (plist-get info :reveal-extra-script) "")))
     ;; Extra <script src="..."></script> tags
     (org-reveal--script-tags-by-auto-file-names (plist-get info :reveal-extra-script-src)
                                                 in-single-file))))

(defun org-reveal--read-sexps-from-string (s)
  (let ((s (string-trim s)))
    (and (not (string-empty-p s))
      (let ((r (read-from-string s)))
        (let ((obj (car r))
              (remain-index (cdr r)))
          (and obj
               (cons obj (org-reveal--read-sexps-from-string (substring s remain-index)))))))))

(defun org-reveal--search-first-%s-or-%% (s start)
  "Search the first appearance of '%s' or '%%' in fmt from
  start. Return the location (of the '%') if found, nil
  otherwise"
  (when (< start (length s))
    (if (eq (aref s start) ?%)
        (let ((next (1+ start)))
          (when (< next (length s))
            ;; Only when % is not the last character
            (let ((next-c (aref s next)))
              (if (or (eq next-c ?s) (eq next-c ?%))
                  ;; ^ Found the first %s or %%. Return the index
                  start
                ;; Not a %s or %%. Keep searching
                (org-reveal--search-first-%s-or-%% s (1+ next))))))
      ;; Not a %. Keep search
      (org-reveal--search-first-%s-or-%% s (1+ start)))))

(defun org-reveal--replace-first-%s (fmt str)
  (let ((idx (org-reveal--search-first-%s-or-%% fmt 0)))
    (if idx
        (concat
         ;; Pre
         (substring fmt 0 idx)
         ;; To be replaced
         (if (eq ?% (aref fmt (1+ idx)))
             "%" ;; %% -> %
           str ;; %s -> str
           )
         ;; Post
         (substring fmt (+ 2 idx) nil))
      ;; nil, no %s or %% found
      fmt)))

(defun org-reveal-plugin-scripts-4 (plugins info in-single-file)
  "Return scripts for initializing reveal.js 4.x builtin scripts."
  ;; Return a pair whose first value is the HTML contents for the
  ;; plugin scripts, the second value is a list of import statements
  ;; to be embedded in the Reveal.initialize call
  (if (not (null plugins))
      ;; Generate plugin scripts
      (let* ((plugins (mapcar
                       (lambda (p)
                         ;; Convert legacy
                         ;; plugin names into
                         ;; reveal.js 4.0 ones
                         (cond
                          ((eq p 'highlight) 'RevealHighlight)
                          ((eq p 'markdown) 'RevealMarkdown)
                          ((eq p 'search) 'RevealSearch)
                          ((eq p 'notes) 'RevealNotes)
                          ((eq p 'math) 'RevealMath)
                          ((eq p 'zoom) 'RevealZoom)
                          (t p)))
                       plugins))
             (available-plugins
              (append '((RevealHighlight . "%splugin/highlight/highlight.js")
                        (RevealMarkdown . "%splugin/markdown/markdown.js")
                        (RevealSearch . "%splugin/search/search.js")
                        (RevealNotes . "%splugin/notes/notes.js")
                        (RevealMath . "%splugin/math/math.js")
                        (RevealZoom . "%splugin/zoom/zoom.js"))
                      org-reveal-external-plugins
                      ;; Buffer local plugins
                      (let ((local-plugins (plist-get info :reveal-external-plugins)))
                        (and local-plugins
                             (org-reveal--read-sexps-from-string local-plugins)))))
             (plugin-js (seq-filter 'identity ;; Filter out nil
                                    (mapcar (lambda (p)
                                              (cdr (assoc p available-plugins)))
                                            plugins))))
        (if (not (null plugin-js))
            (cons
             ;; First value of the pair, a list of script file names
             (let ((root-path (org-reveal-root-path info)))
               (org-reveal--multi-level-mapconcat
                (lambda (p)
                  (org-reveal--script-tag-by-file-name (org-reveal--replace-first-%s p root-path)
                                                       in-single-file))
                plugin-js ""))
             ;; Second value of the tuple, a list of Reveal plugin
             ;; initialization statements
             (format "plugins: [%s]"
                     (mapconcat 'symbol-name
                                ;; Remove multiplex from plugins, as
                                ;; the multiplex plugin has been moved
                                ;; out of reveal.js.
                                (seq-filter (lambda (p) (not (eq p 'multiplex))) plugins) ", ")))
          ;; No available plugin info found. Perhaps wrong plugin
          ;; names are given
          (cons nil nil)))
    ;; No plugins, return empty string
    (cons nil nil)))
(defun org-reveal-toc (depth info)
  "Build a slide of table of contents."
  (let ((toc (org-html-toc depth info)))
    (when toc
      (let ((toc-slide-background (plist-get info :reveal-toc-slide-background))
            (toc-slide-background-size (plist-get info :reveal-toc-slide-background-size))
            (toc-slide-background-position (plist-get info :reveal-toc-slide-background-position))
            (toc-slide-background-repeat (plist-get info :reveal-toc-slide-background-repeat))
            (toc-slide-background-transition (plist-get info :reveal-toc-slide-background-transition))
            (toc-slide-background-opacity (plist-get info :reveal-toc-slide-background-opacity))
            (toc-slide-with-header (plist-get info :reveal-slide-global-header))
            (toc-slide-with-footer (plist-get info :reveal-slide-global-footer)))
        (concat "<section id=\"sec-table-of-contents\""
                (when toc-slide-background
                  (concat " data-background=\"" toc-slide-background "\""))
                (when toc-slide-background-size
                  (concat " data-background-size=\"" toc-slide-background-size "\""))
                (when toc-slide-background-position
                  (concat " data-background-position=\"" toc-slide-background-position "\""))
                (when toc-slide-background-repeat
                  (concat " data-background-repeat=\"" toc-slide-background-repeat "\""))
                (when toc-slide-background-transition
                  (concat " data-background-transition=\"" toc-slide-background-transition "\""))
                (when toc-slide-background-opacity
                  (concat " data-background-opacity=\"" toc-slide-background-opacity "\""))
                ">"
                (when toc-slide-with-header
                   (let ((header (plist-get info :reveal-slide-header)))
                     (when header (format "<div class=\"slide-header\">%s</div>\n" header))))
                (replace-regexp-in-string "<a href=\"#" "<a href=\"#/slide-" toc)
                (when toc-slide-with-footer
                   (let ((footer (plist-get info :reveal-slide-footer)))
                     (when footer (format "<div class=\"slide-footer\">%s</div>\n" footer))))
                "</section>\n")))))

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

(defun org-reveal-parse-token (keyword info key &optional value)
  "Return HTML tags or perform SIDE EFFECT according to key.
Use the previous section tag as the tag of the split section. "
  (cl-case (intern key)
    (split
     (let ((headline (org-element-property
		      :parent
		      (org-element-property
		       :parent
		       keyword))))
       (concat
	"</section>\n" ;; Close the previous section and start a new one.
	(org-reveal-slide-section-tag headline info t)
	(and value
	     (string= value "t")
	     ;; Add a title for the split slide
	     ;; Copy from `org-html-headline' and modified.
	     (let* ((title (org-export-data
			    (org-element-property :title headline)
			    info))
		    (level (+ (org-export-get-relative-level headline info)
			      (1- (plist-get info :html-toplevel-hlevel))))
		    (numberedp (org-export-numbered-headline-p headline info))
		    (numbers (org-export-get-headline-number headline info)))
	       (format "\n<h%d>%s</h%d>"
		       level
		       (concat
			(and numberedp
			     (format
			      "<span class=\"section-number-%d\">%s</span> "
			      level
			      (mapconcat #'number-to-string numbers ".")))
			title)
		       level))))))))

(defun org-reveal-parse-keyword-value (keyward value info)
  "According to the value content, return HTML tags to split slides."
  (let ((tokens (mapcar
                 (lambda (x) (split-string x ":"))
                 (split-string value))))
    (mapconcat
     (lambda (x) (apply 'org-reveal-parse-token keyward info x))
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
     (cl-case type
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
     (cl-case type
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
    (cl-case (intern key)
      (REVEAL (org-reveal-parse-keyword-value keyword value info))
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
  (let ((tag (cl-case (org-element-property :type plain-list)
               (ordered "ol")
               (unordered "ul")
               (descriptive "dl")))
        (attrs (org-export-read-attribute :attr_html plain-list)))
    (format "<%s%s>\n%s\n</%s>"
            tag
            (if attrs (concat " " (org-html--make-attribute-string attrs)) "")
            contents
            tag
            )))

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
		   ;; Use our own function for org-export-format-code,
		   ;; since org-html-format-code will wrap line
		   ;; numbers with HTML tags that will be display as
		   ;; part of the code by highlight.js
		   (let* ((num-start (org-export-get-loc src-block info))
			  (code (car (org-export-unravel-code src-block)))
			  (code-length (length (split-string code "\n")))
			  (num-fmt (and num-start
					(format "%%%ds: "
						(length (number-to-string (+ code-length
									     num-start)))))))
		     (org-export-format-code
		      (org-html-encode-plain-text code)
		      (lambda (loc line-num ref)
			(setq loc
			      (concat
			       ;; Add line number, if needed
			       (when num-start (format num-fmt line-num))
			       loc)))
		      num-start))))
	   (code-attribs (or (org-export-read-attribute
			      :attr_reveal src-block :code_attribs) ""))
           (data-id (if-format " data-id=\"%s\"" (org-export-read-attribute
                                                 :attr_reveal src-block :data_id)))
           (label (if-format "id=\"%s\"" (org-element-property :name src-block)))
           (klipsify  (and  org-reveal-klipsify-src
                           (member lang '("javascript" "js" "ruby" "scheme" "clojure" "php" "html"))))
           (langselector (cond ((or (string= lang "js") (string= lang "javascript")) "selector_eval_js")
                               ((string= lang "clojure") "selector")
                               ((string= lang "python") "selector_eval_python_client")
                               ((string= lang "scheme") "selector_eval_scheme")
                               ((string= lang "ruby") "selector_eval_ruby")
                               ((string= lang "html") "selector_eval_html"))
                         ))
      (if (not lang)
          (format "<pre %s>\n%s</pre>"
                  (string-join (list (or (frag-class src-block info) " class=\"example\"")
                                     label)
                               " ")

                  code)
        (if klipsify
            (concat
             "<iframe style=\"background-color:white;\" height=\"500px\" width= \"100%\" srcdoc='<html><body><pre><code "
             (if (string= lang "html" )"data-editor-type=\"html\"  "  "") "class=\"klipse\" "code-attribs ">
" (if (string= lang "html")
      (replace-regexp-in-string "'" "&#39;"
                                (replace-regexp-in-string "&" "&amp;"
                                                          (replace-regexp-in-string "<" "&lt;"
                                                                                    (replace-regexp-in-string ">" "&gt;"
                                                                                                              (cl-letf (((symbol-function 'org-html-htmlize-region-for-paste)
                                                                                                                         #'buffer-substring))
                                                                                                                (org-html-format-code src-block info))))))
    (replace-regexp-in-string "'" "&#39;"
                              code))  "
</code></pre>
<link rel= \"stylesheet\" type= \"text/css\" href=\"" org-reveal-klipse-css "\">
<style>
.CodeMirror { font-size: 2em; }
</style>
<script>
window.klipse_settings = { " langselector  ": \".klipse\" };
</script>
<script src= \"" org-reveal-klipse-js "\"></script></body></html>
'>
</iframe>")
          (format
	   "<div class=\"org-src-container\">\n%s%s\n</div>"
	   (if (not caption) ""
	     (format "<label class=\"org-src-name\">%s</label>"
		     (org-export-data caption info)))
	   (if use-highlight
	       (format "\n<pre %s><code class=\"%s\" %s>%s</code></pre>"
                       (string-join (list (or (frag-class src-block info) "")
                                          label
                                          data-id)
                                    " ")
		        lang code-attribs code)
	     (format "\n<pre %s><code trim>%s</code></pre>"
                     (string-join (list (or (frag-class src-block info)
			                    (format " class=\"src src-%s\"" lang))
		                        label
                                        data-id
                                        code-attribs)
                                  " ")

                     code))))))))

(defun org-reveal-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to Reveal.
CONTENTS holds the contents of the block INFO is a plist holding
contextual information."
  (format "<blockquote %s>\n%s</blockquote>"
          (or (frag-class quote-block info)
	      "")
          contents))


(defun org-reveal--auto-title-slide-template (info)
  "Generate the automatic title slide template."
  (let* ((spec (org-html-format-spec info))
         (title (org-export-data (plist-get info :title) info))
	 (subtitle (cdr (assq ?s spec)))
         (author (cdr (assq ?a spec)))
         (email (cdr (assq ?e spec)))
         (date (cdr (assq ?d spec))))
    (concat
     (when (and (plist-get info :with-title)
                (org-string-nw-p title))
       (concat "<h1 class=\"title\">" title "</h1>"
	       (if-format "<p class=\"subtitle\">%s</p>\n" subtitle)))
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
               (format-time-string org-html-metadata-timestamp-format)
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
   (if-format "<meta name=\"author\" content=\"%s\"/>\n" (org-export-data (plist-get info :author) info))
   (if-format "<meta name=\"description\" content=\"%s\"/>\n" (org-export-data (plist-get info :description) info))
   (if-format "<meta name=\"keywords\" content=\"%s\"/>\n" (org-export-data (plist-get info :keywords) info))
   (org-reveal-stylesheets info)
   (org-reveal-mathjax-scripts info)
   (org-reveal--build-pre/postamble 'head-preamble info)
   (org-element-normalize-string (plist-get info :html-head))
   (org-element-normalize-string (plist-get info :html-head-extra))
   "</head>
<body>\n"
   (org-reveal--build-pre/postamble 'preamble info)
   "<div class=\"reveal\">\n"
   (org-reveal--build-pre/postamble 'prologue info)
   "<div class=\"slides\">\n"
   ;; Title slides
   (let ((title-slide (plist-get info :reveal-title-slide)))
     (when (and title-slide (not (plist-get info :reveal-subtree)))
       (let ((title-slide-background (plist-get info :reveal-title-slide-background))
             (title-slide-background-size (plist-get info :reveal-title-slide-background-size))
             (title-slide-background-position (plist-get info :reveal-title-slide-background-position))
             (title-slide-background-repeat (plist-get info :reveal-title-slide-background-repeat))
             (title-slide-background-transition (plist-get info :reveal-title-slide-background-transition))
	     (title-slide-background-opacity (plist-get info :reveal-title-slide-background-opacity))
             (title-slide-state (plist-get info :reveal-title-slide-state))
             (title-slide-with-header (plist-get info :reveal-slide-global-header))
             (title-slide-with-footer (plist-get info :reveal-slide-global-footer)))
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
		 (when title-slide-background-opacity
		   (concat " data-background-opacity=\"" title-slide-background-opacity "\""))
                 (when title-slide-state
		   (concat " data-state=\"" title-slide-state "\""))
                 ">"
                 (when title-slide-with-header
                   (let ((header (plist-get info :reveal-slide-header)))
                     (when header (format "<div class=\"slide-header\">%s</div>\n" header))))
                 (cond ((eq title-slide nil) nil)
                       ((stringp title-slide) (format-spec title-slide (org-html-format-spec info)))
                       ((eq title-slide 'auto) (org-reveal--auto-title-slide-template info)))
                 "\n"
                 (when title-slide-with-footer
                   (let ((footer (plist-get info :reveal-slide-footer)))
                     (when footer (format "<div class=\"slide-footer\">%s</div>\n" footer))))
                 "</section>\n"))))
   contents
   "</div>\n"
   (org-reveal--build-pre/postamble 'epilogue info)
   "</div>\n"
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
  ;; Hint for obsolete options
  (when (memq t
	       (mapcar
		(lambda (key) (plist-get info key))
		'(:reveal-control
		  :reveal-progress
		  :reveal-history
		  :reveal-center
		  :reveal-slide-number
		  :reveal-rolling-links
		  :reveal-keyboard
		  :reveal-overview
		  :reveal-width
		  :reveal-height
		  :reveal-margin
		  :reveal-min-scale
		  :reveal-max-scale
		  :reveal-trans
		  :reveal-speed)))
    (with-output-to-temp-buffer
	"* ox-reveal hints *"
      (princ "
Note: Options and custom variables for initializing reveal.js are
obsolete. Use '#+REVEAL_INIT_OPTIONS:' instead to give JS code
snippet for initializing reveal.js. The following is a such
example:

#+REVEAL_INIT_OPTIONS: width:1200, height:800, controlsLayout: 'edges'

Obsolete options and variables are listed below:

  | Option                | Variable                    |
  |-----------------------+-----------------------------|
  | reveal_control       | org-reveal-control          |
  | reveal_progress      | org-reveal-progress         |
  | reveal_history       | org-reveal-history          |
  | reveal_center        | org-reveal-center           |
  | reveal_slide_number  | org-reveal-slide-number     |
  | reveal_rolling_links | org-reveal-rolling-links    |
  | reveal_keyboard      | org-reveal-keyboard         |
  | reveal_overview      | org-reveal-overview         |
  | reveal_width         | org-reveal-width            |
  | reveal_height        | org-reveal-height           |
  | #+REVEAL_MARGIN      | org-reveal-margin           |
  | #+REVEAL_MIN_SCALE   | org-reveal-min-scale        |
  | #+REVEAL_MAX_SCALE   | org-reveal-max-scale        |
  | #+REVEAL_TRANS       | org-reveal-transition       |
  | #+REVEAL_SPEED       | org-reveal-transition-speed |
")))
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
                      (items (org-element-contents elem))
		      (default-style-list
                        (mapcar (lambda (a) default-style)
                                (number-sequence 1 (length items)))))
                 (if frag-index
                     (cl-mapcar 'org-reveal--update-attr-html
                                items frag-list default-style-list (car (read-from-string frag-index)))
                   (let* ((last-frag (car (last frag-list)))
                          (tail-list (mapcar (lambda (a) last-frag)
                                             (number-sequence (+ (length frag-list) 1)
                                                              (length items)))))
                     (nconc frag-list tail-list)
                     (cl-mapcar 'org-reveal--update-attr-html items frag-list default-style-list)))))
              (t (org-reveal--update-attr-html elem frag default-style frag-index)))
      elem)))

(defun org-reveal-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reveal.js HTML file."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (clientfile (org-export-output-file-name (concat "_client" extension) subtreep))
         (org-export-exclude-tags (cons "noexport_reveal" org-export-exclude-tags))
         (client-multiplex nil))
    ; export filename_client HTML file if multiplexing
    (let ((retfile (org-export-to-file 'reveal file
                     async subtreep visible-only body-only ext-plist)))

       ; export the client HTML file if client-multiplex is set true
       ; by previous call to org-export-to-file
      (when client-multiplex
        (org-export-to-file 'reveal clientfile
          async subtreep visible-only body-only ext-plist))
      retfile)))

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

;; Generate a heading of ToC for current buffer and write to current
;; point
(defun org-reveal-manual-toc (gh-links)
  (interactive "P")
  (insert ;; at current point
   (mapconcat
    'identity
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
	(let ((title (org-element-property :raw-value headline)))
	  (concat
	   ;; Leading spaces by headline level
	   (make-string (* 2 (org-element-property :level headline)) ?\s)
	   "- [["
	   title
	   "]["
	   title
	   "]]"
	   (when gh-links
	     ;; Add another link to github readme page
	     (concat
	      "([[https://github.com/yjwen/org-reveal#"
	      ;; Follow the github link-naming convension
	      (replace-regexp-in-string
	       ;; remove any  non-alphanum character
	       "[.,']"
	       ""
	       (replace-regexp-in-string
		;; Any spaces with hyphen
		" +" "-"
		(downcase title)))
	      "][gh]])")
	     )))))
    "\n")))
;;;###autoload
(defun org-reveal-publish-to-reveal
 (plist filename pub-dir)
  "Publish an org file to Html.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let ((client-multiplex nil))
    (org-publish-org-to 'reveal filename ".html" plist pub-dir)
    (when client-multiplex
      (org-publish-org-to 'reveal filename "_client.html" plist pub-dir))))

(provide 'ox-reveal)

;;; ox-reveal.el ends here
