
(defvar project-dir (expand-file-name (file-name-as-directory "~/ProjectsGitHub/ildenir.github.com/"))
"Diretorio do projeto do website")

(defvar publish-dir (expand-file-name (concat project-dir "website"))
"Diretorio onde sera publicado o website")

(defvar src-dir (expand-file-name (concat project-dir "org"))
"Diretorio dos arquivos fonte org, imagens, css e ...")

(defvar website-html-preamble "

<div id=\"mySidenav\" class=\"sidenav\">
  <ul class=\"menu-principal\">
    <li><a href=\"javascript:void(0)\" class=\"closebtn\" onclick=\"closeNav()\">&times;</a>
    <li><a href=\"index.html\"> Home </a></li>
    <li> <a href=\"articles.html\"> Articles </a></li>
    <li> <a href=\"site-map.html\"> Site Map </a></li>
    <li><a href=\"about.html\"> About </a></li>
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
    <p class=\"cabecalho-titulo\"> Pagina pessoal </p>
  </div>
</header>

")

(defvar website-html-head "<link rel=\"stylesheet\" href=\"css/style.css\">
<link rel=\"stylesheet\" href=\"font-awesome-4.7.0/css/font-awesome.css\">
<script src=\"js/main.js\"></script>")

(defun website-generate-article-list ()
  "Gera lista com dados de artigos do projeto. A lista retornada 
possui o formato (filename (title desc link-img pub-date)) onde 
link-img pode ser nil caso nao exista")

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
         :exclude "\-.+"
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
