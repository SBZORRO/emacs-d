(setq socks-noproxy '("127.0.0.1"))
(setq socks-server '("Default server" "127.0.0.1" 1337 5))
(setq url-gateway-method 'socks)

(setq url-gateway-local-host-regexp
  (concat "\\`" (regexp-opt '("localhost" "127.0.0.1")) "\\'"))
