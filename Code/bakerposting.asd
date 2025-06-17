(asdf:defsystem :bakerposting
  :depends-on (:alexandria :bordeaux-threads :jsown
               :one-more-re-nightmare :drakma
               :freddie-laker :maiden-irc :plump :lquery :stem)
  :version "2.1.0"
  :serial t
  :components ((:file "package")
               (:file "bot")
               (:file "quotes")
               (:file "discord")
               (:file "discord-acting")
               (:file "pleroma")
               (:file "irc")
               (:file "load-config")))
