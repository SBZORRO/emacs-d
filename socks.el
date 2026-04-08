;; SOCKS5 for Emacs URL/HTTP stack (package.el, url.el, etc.)
(require 'socks)

(setq url-gateway-method 'socks
      ;; ("NAME" "HOST" PORT SOCKS-VERSION)
      socks-server '("default" "127.0.0.1" 1337 5))
