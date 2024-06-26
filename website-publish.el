;;; website-publish.el --- Configuracao publicar site com org-mode -*- lexical-binding:t -*-

;; Copyright (C) 2017 Ildenir Barbosa

;; Author: I. C. Barbosa <ildenir+github@googlemail.com>
;; Version: 0.0
;; Keywords: website
;; URL: http://github.com/ildenir/ildenir.github.com

;;; Commentary:

;; Este pacote configura/customiza o exportador do org-mode para gerar
;; o website casa esquilo de pirai.

;;; Code:

(defcustom website-project-dir
  (expand-file-name
   (file-name-as-directory "~/ProjectsGitHub/ildenir.github.com/"))
  "Diretorio do projeto do website."
  :type 'directory)

(defsubst publish-dir ()
  "Diretorio onde sera publicado o website."
  website-project-dir)

(defsubst src-dir ()
  "Diretorio dos arquivos fonte org, imagens, css e ..."
  website-project-dir)

(defsubst src-articles-dir ()
  "Diretorio fonte dos artigos."
  (concat (file-name-as-directory (src-dir))
	  (file-name-as-directory "articles")))

(defsubst publish-articles-dir ()
  "Diretorio do artigos publicados."
  (concat (file-name-as-directory (publish-dir))
	  (file-name-as-directory "articles")))
  (defun website-load-file (filename)
  "Carrega arquivo apartir de FILENAME e retorna string com conteudo."
  (with-temp-buffer (insert-file-contents filename)
		    (buffer-substring (point-min) (point-max))))

(defun website-preamble-loader (val)
 (website-load-file (concat (file-name-as-directory (src-dir)) "preamble.html")))

    (defvar website-html-preamble 'website-preamble-loader
      "Cabecalho inserido em toda pagina.")


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
    (let* ((filterregex "\\(TITLE\\|DATE\\|DESCRIPTION\\|EMAIL\\|KEYWORDS\\|AUTHOR\\)")
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
  (let ((files (directory-files-recursively (src-dir) "\.org$")))
    (mapcar (lambda (fn) (list fn (website-extract-article-data fn)))
	    files)))

(require 'ox-publish)
(setq org-publish-project-alist
      `(
	("org-notes"
	 :base-directory ,(src-dir)
	 :base-extension "org"
	 :publishing-directory ,(publish-dir)
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4             ; Just the default for this project.
	 :auto-preamble t
	 :org-html-doctype html5
	 :html-doctype "html5"
	 :org-html-html5-fancy t
	 :exclude ".*--ig--.*"
	 :html-preamble ,website-html-preamble
	 :html-postamble-format ""
	 :auto-sitemap t
	 :sitemap-title "Site map"
	 :sitemap-filename "site-map.org"
	 )
	("org-static"
	 :base-directory ,(src-dir)
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf|otf\\|woff\\|woff2\\|ttf\\|svg"
	 :publishing-directory ,(publish-dir)
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	("org" :components ("org-notes" "org-static"))))


(defun website--extract-kw (kw)
  "Auxiliar cria funcao que extrai lista de KW de todos os artigos."
  (lambda (data)
    (let ((pl (car (cdr data))) )
      (plist-get pl kw))))


  (defun website--list-all (keyword)
    "Extrai lista de keyword de todos os arquivos"
    (let ((articles (website-generate-article)))
      (remove nil (mapcar (website--extract-kw keyword) articles))))

(defun website--keyword-list ()
  "Lista de todas opcoes KEYWORD dos artigos."
  (let ((articles-kw (website--list-all 'keywords))
	(split-string-default-separators "[ \f\t\n\r\v,]+")
	(kw-list (list)))
    (dolist (l articles-kw kw-list)
      (setq kw-list (append kw-list (split-string l))))
    (remove "nil" (delete-dups kw-list))))

  (defun website-new-article ()
    "Entrevista usuario e insera conteudo ao projeto"
    (interactive)
    (let* ((title (read-string "Title: " ))
	   (description (read-string "Descricao: "))
	   (author (completing-read "Autor: " (website--list-all 'author)))
	   (date (format-time-string "%d/%m/%Y"))
	   (email (completing-read "Email: " (website--list-all 'email)))
	   (keywords (completing-read-multiple "Palavras-chave: "
				      (website--keyword-list)))
	   (filename (string-join
		      (list (concat (file-name-as-directory (src-dir))
				    (file-name-as-directory "articles"))
			    (format-time-string "%Y%m%d") "-"
			    (string-join (split-string title) "_") ".org"))))
      (with-current-buffer (get-buffer-create filename)
	(insert "#+SETUPFILE: ../setup/xtreme-simple-theme")
	(insert (format "#+TITLE: %s\n" title))
	(insert (format "#+DATE: %s\n" date))
	(insert (format "#+AUTHOR: %s\n" author))
	(insert (format "#+EMAIL: %s\n" email))
	(insert (format "#+DESCRIPTION: %s\n" description))
	(insert (format "#+KEYWORDS: %s\n" keywords))
	(insert "#+LANGUAGE: pt_BR\n")
	(insert "#+OPTIONS: num:nil\n")
	(write-file filename))))


(provide 'website-publish)
;;; website-publish.el ends here
