
;;; website-publish.el --- Configuracao publicar site com org-mode

;; Copyright (C) 2017 Ildenir Barbosa

;; Author: I. C. Barbosa <ildenir+github@googlemail.com>
;; Version: 0.0
;; Keywords: website
;; URL: http://github.com/ildenir/ildenir.github.com

;;; Commentary:

;; Este pacote configura/customiza o exportador do org-mode para gerar
;; o website casa esquilo de pirai.

;;; Code:

(defvar project-dir (expand-file-name (file-name-as-directory "~/ProjectsGitHub/ildenir.github.com/"))
  "Diretorio do projeto do website.")

(defvar publish-dir (expand-file-name (concat project-dir "website"))
  "Diretorio onde sera publicado o website.")

(defvar src-dir (expand-file-name (concat project-dir "org"))
  "Diretorio dos arquivos fonte org, imagens, css e ...")
(defvar website-html-preamble "
  <div id=\"mySidenav\" class=\"sidenav\">
    <ul class=\"menu-principal\">
      <li><a href=\"javascript:void(0)\" class=\"closebtn\" onclick=\"closeNav()\">&times;</a>
      <li><a href=\"index.html\"> Home </a></li>
      <li> <a href=\"articles.html\"> Artigos </a></li>
      <li> <a href=\"books.html\"> Livros </a></li>
      <li><a href=\"about.html\"> Sobre </a></li>
    </ul>

    <ul class=\"rede-social\">
      <li><a href=\"http://twitter.com/Uilcoder\"><span class=\"fa fa-twitter\"></span></a></li>
      <li><a href=\"http://github.com/ildenir\"><span class=\"fa fa-github\"></span></a></li>
    </ul>
  </div>

  <header class=\"barra\">
    <div class=\"cabecalho-barra\">
      <span onclick=\"openNav()\" class=\"w3-button\" >
        <span class=\"fa fa-bars\"></span>
      </span>
    </div>
  </header>

  "
  "Cabecalho inserido em toda pagina.")

(defvar website-html-head "<link rel=\"stylesheet\" href=\"css/style.css\">
  <link rel=\"stylesheet\" href=\"font-awesome-4.7.0/css/font-awesome.css\">
  <script src=\"js/main.js\"></script>"
  "Referencia para estilo css e scripts.")


(require 'org-element)
(require 'cl-lib)

(defun website--extrack-kv (ast)
  "Rotina interna para extrair (key value) da AST."
  (org-element-map ast 'keyword
    (lambda(key) (list
                  (org-element-property :key key)
                  (org-element-property :value key)) )))

(defun website--extract-link (ast)
  "Rotina interna para extrair link para image da AST."
  (org-element-map ast 'link
    (lambda(lk) (when (string= (org-element-property :type lk) "fuzzy")
                  lk))))

(defun website-filter-kv (kws filterregexp)
  "Filtra lista KWS com key match padrao FILTERREGEXP."
  (cl-remove-if-not (lambda (el) (string-match filterregexp (car el))) kws))

(defun website-extract-article-data (filename)
  "Extrai dados do artigo com nome FILENAME.
Retorna plist keys title image description date"
  (with-temp-buffer
    (insert-file-contents filename)
    (org-mode)
    (let* ((filterregex "\\(TITLE\\|DATE\\|DESCRIPTION\\)")
           (ast (org-element-parse-buffer))
           (kv (website--extrack-kv ast))
           (link (website--extract-link ast))
           (kv-filtered (website-filter-kv kv filterregex))
           kv-plist)
      (setq kv-plist
            (plist-put kv-plist
                       'image (org-element-interpret-data (car link))))
      (dolist (k kv-filtered kv-plist)
        (message (car k))
        (setq kv-plist
              (plist-put kv-plist
                         (intern (downcase (car k))) (car (cdr k))))))))

(defun website-generate-article ()
  "Gera lista com dados de artigos do projeto.
A lista retornada possui o formato
'(filename (title desc link-img pub-date)) onde link-img pode ser nil caso nao
exista.  Description vai ser extraida de #+DESCRIPTION:"
  (let ((files (directory-files-recursively src-dir "\.org$")))
    (mapcar (lambda (fn) (list fn (website-extract-article-data fn)))
            files)))

(require 'ox-publish)
(setq org-publish-project-alist
      `(
        ("org-notes"
         :base-directory ,src-dir
         :base-extension "org"
         :publishing-directory ,publish-dir
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         :org-html-doctype html5
         :org-html-html5-fancy t
         :exclude "^ *\-.+"
         :html-preamble ,website-html-preamble
         :html-postamble-format ""
         :html-head ,website-html-head
         :auto-sitemap t
         :sitemap-title "Site map"
         :sitemap-filename "site-map.org"
         )
        ("org-static"
         :base-directory ,src-dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf|otf\\|woff\\|woff2\\|ttf\\|svg"
         :publishing-directory ,publish-dir
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("org" :components ("org-notes" "org-static"))))



(provide 'website-publish)
;;; website-publish.el ends here
