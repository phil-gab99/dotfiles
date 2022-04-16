((magit-am
  ("--3way"))
 (magit-branch nil)
 (magit-commit nil
               ("--all")
               ("--all" "--reuse-message=ORIG_HEAD")
               ("--no-verify"))
 (magit-dispatch nil)
 (magit-fetch nil)
 (magit-gitignore nil)
 (magit-log
  ("-n256" "--graph" "--decorate"))
 (magit-merge nil)
 (magit-pull nil)
 (magit-push nil
             ("--force" "--no-verify")
             ("--force")
             ("--force-with-lease" "--force" "--no-verify"))
 (magit-rebase nil
               ("--autostash")
               ("--autostash" "--no-verify"))
 (magit-reset nil)
 (magit-revert
  ("--edit"))
 (magit-revision-history "ORIG_HEAD")
 (magit-stash nil))
