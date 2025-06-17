(asdf:defsystem :bakerposting
  :depends-on (:alexandria :bordeaux-threads :jsown
               :one-more-re-nightmare :drakma
               :freddie-laker :maiden-irc :plump :lquery)
  :version "2.0.0"
  :serial t
  :components ((:file "package")
               (:file "bot")
               (:file "quotes")
               (:file "discord")
               (:file "discord-acting")
               (:file "pleroma")
               (:file "irc")
               (:file "load-config")))
