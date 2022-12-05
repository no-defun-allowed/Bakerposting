((script
  "Telephone" "*du-du-du du-du-du*"
  "Joe" "*picks up phone* Hello, Mike."
  "Mike" "Hello, Joe. System working?"
  "Joe" "Seems to be."
  "Mike" "Okay, fine."
  "Joe" "Uh, okay."
  "Joe" "*hangs up*")
 (script
  "Telephone" "*du-du du-du du-du du-du*"
  "Joe" "*picks up phone*"
  "Mike" "Hello, Joe."
  "Joe" "Hello, Mike."
  "Mike" "Okay, I'm now going to leave this call set up from this...phone here, and I'm going to make another phone call on this other telephone."
  :again)
 (script
  "Telephone" "*du-du-du du-du-du*"
  "Joe" "*picks up phone* Hello!"
  "Mike" "Hello, Joe."
  "Mike" "*pop* Okay, uuh.."
  "Mike" "We made a call to Joe, I'm now going to make a call to Robert."
  :again)
 (script
  "Telephone" "*brrrrrrrrrrrt*"
  "Robert" "*picks up phone*"
  "Mike" "Hello, Robert."
  "Robert" "Hello, Mike.")
 (script
  "Mike" "First, I'll make a call to Joe. *dials*"
  "Telephone" "*du-du du-du du-du du-du*"
  "Joe" "*picks up phone* Hello Mike."
  "Mike" "Hello, Joe."
  "Mike" "I'll now make a call to robert. *dials again*"
  "Telephone" "*brrrrrrrrrrt*"
  "Robert" "Hello, Mike."
  "Mike" "Hello, Robert."
  "Mike" "Now I'll press the conference button, and see if it works this time."
  :again)
 (script
  "Mike" "Hello, Joe."
  "Joe" "Hello, Mike. Hello, Robert."
  "Robert" "Hello, Joe. Hello, Mike."
  "Mike" "Hello! Well, it worked this time. I'll leave Joe and Robert to talk to each other, and go out of the conference."
  "Joe" "Hello, Robert."
  "Robert" "Hello, Joe."
  "Joe" "You managed to fix the bug then?"
  "Robert" "Yes, finally."
  "Joe" "*hangs up*")
 (script
  "Joe" "Do you remember, before we went into conference, we set up a phone call?"
  "Joe" "Well, let's see if that call is still running. *picks up phone*"
  "Joe" "Hello, Mike."
  "Mike" "Hello, Joe."))
