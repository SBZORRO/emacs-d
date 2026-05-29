(use-package acp)
(use-package shell-maker)
(use-package agent-shell)
(setq agent-shell-openai-authentication
      (agent-shell-openai-make-authentication :login t))
