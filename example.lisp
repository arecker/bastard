(bastard:hardlink "configs/sbclrc.lisp" "~/.sbclrc")
(bastard:hardlink "configs/gemrc" "~/.gemrc")
(bastard:hardlink "configs/gitconfig" "~/.gitconfig")
(bastard:hardlink "configs/gitconfig-work" "~/.gitconfig-work")
(bastard:hardlink "configs/ssh_config" "~/.ssh/config")

(dolist (script (bastard:each "scripts/*.sh"))
  (bastard:symlink script (bastard:join "~/bin/" script)))
