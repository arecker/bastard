;; DIRECTORIES
(let ((folders (if (bastard:is-linux)
		'("bin" "desktop" "docs" "downloads" "music" "pics" "src")
		'("bin" "Desktop" "Documents" "Downloads" "Music" "Pictures" "src"))))
  (dolist (folder folders)
    (bastard:folder (bastard:join "~/" folder))))
(dolist (target '("~/.ssh/"))
  (bastard:folder target))

;; CONFIGS
(bastard:hardlink "configs/ansible/ansible.cfg" "~/.ansible.cfg")
(bastard:hardlink "configs/bash/bashrc.sh" "~/.bashrc")
(bastard:hardlink "configs/bash/profile.sh" "~/.profile")
(bastard:hardlink "configs/lisp/sbclrc.lisp" "~/.sbclrc")
(bastard:hardlink "configs/ruby/gemrc" "~/.gemrc")
(bastard:hardlink "configs/git/config" "~/.gitconfig")
(bastard:hardlink "configs/git/work" "~/.gitconfig-work")
(bastard:hardlink "configs/ssh/config" "~/.ssh/config")
(unless (bastard:is-linux)
  (bastard:symlink "configs/blog/macos.yml" "~/.blog.yml"))

;; SCRIPTS
(dolist (script (bastard:each "scripts/*.sh"))
  (bastard:symlink script (bastard:join "~/bin/" script)))

;; REPOS

;; src
(dolist (repo '("blog" "infra" "lisproom" "network"))
  (bastard:git (format nil "git@github.com:arecker/~A" repo) (bastard:join "~/src/" repo)))

;; other
(dolist (repo '(("emacs.d.git" "~/.emacs.d")
		("public" "~/public")))
  (let ((src (format nil "git@github.com:arecker/~A.git" (first repo)))
	(dest (second repo)))
    (bastard:git src dest)))

;; PYTHON
(bastard:git "https://github.com/pyenv/pyenv.git" "~/.pyenv")
(dolist (plugin (mapcar (lambda (p) (format nil "pyenv-~A" p))
			'("doctor" "installer" "update" "virtualenv" "which-ext")))
  (bastard:git (format nil "https://github.com/pyenv/~A.git" plugin)
	       (bastard:join "~/.pyenv/plugins/" plugin)))

;; RUBY
(bastard:git "https://github.com/rbenv/rbenv.git" "~/.rbenv")
(bastard:folder "~/.rbenv/cache")
(bastard:git "https://github.com/rbenv/ruby-build.git" "~/.rbenv/plugins/ruby-build")

;; GO
(bastard:git "https://github.com/syndbg/goenv.git" "~/.goenv")
