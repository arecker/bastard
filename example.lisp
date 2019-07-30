;; git
(bastard:symlink "configs/git/gitconfig" "~/.gitconfig")
(bastard:symlink "configs/git/gitconfig-work" "~/.gitconfig-work")

;; ssh
(bastard:symlink "configs/ssh/id_rsa.pub" "~/.ssh/id_rsa.pub")
(bastard:symlink "configs/ssh/config" "~/.ssh/config")

;; ansible
(bastard:symlink "configs/ansible/ansible.cfg" "~/.ansible.cfg")

;; lisp
(bastard:symlink "configs/lisp/sbclrc.lisp" "~/.sbclrc")

;; ruby
(bastard:symlink "configs/ruby/gemrc" "~/.gemrc")

;; scripts
(dolist (script (bastard:each "scripts/*.sh"))
  (bastard:symlink script (bastard:join "~/bin/" script)))
