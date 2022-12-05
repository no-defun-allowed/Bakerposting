(asdf:defsystem :bakerposting
  :depends-on (:alexandria :bordeaux-threads
               :one-more-re-nightmare
               :lispcord :cl-irc :chirp :plump :lquery)
  :version "2.0.0"
  :serial t
  :components ((:file "package")
               (:file "bot")
               (:file "quotes")
               (:file "discord")
               (:file "twitter")
               (:file "irc")))
