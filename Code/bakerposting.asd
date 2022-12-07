(asdf:defsystem :bakerposting
  :depends-on (:alexandria :bordeaux-threads :jsown
               :one-more-re-nightmare :drakma
               :lispcord :maiden-irc :chirp :plump :lquery)
  :version "2.0.0"
  :serial t
  :components ((:file "package")
               (:file "bot")
               (:file "quotes")
               (:file "discord")
               (:file "discord-acting")
               (:file "twitter")
               (:file "pleroma")
               (:file "irc")))
