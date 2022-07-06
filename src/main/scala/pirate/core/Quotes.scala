package pirate.core

import Constants.CharacterName

object Quotes {
  val get: CharacterName => String = {
    case CharacterName.Dave   => "I'm sorry Dave,\n I'm afraid I can't do that."
    case CharacterName.Dougie => "Dave, where can I\n find an organogram?"
    case CharacterName.Shah   => "Fine! I wont call\n people cowboys in\n public channels again!"
    case CharacterName.Pere   => "Not Elm, please..."
    case CharacterName.Maya   => "Which service can\n I kill next?"
    case CharacterName.Lee    => "My happiness is\n directly proportional to my\n Intellij time"
    case CharacterName.Dan    => "Dave, if I hear you mention\n the principle of least power\n one more time..."
    case CharacterName.Gokce  => "Do you have five minutes\n I need your advice"
    case CharacterName.Miles  => "It works in my version\n of the Scala compiler ..."
    case CharacterName.Josh   => "Hi Dave, I have another\n question about purchase orders"
    case CharacterName.Becky  => "Dave is this legit weird\n or just how ITV does things…"
    case CharacterName.Rob    => "Who do I talk to about..."
    case CharacterName.Anne   => "FNORD!!!"
    case CharacterName.Luke   => "Dave, I'm hoping to save\n The WORLD!! Can I buy another\n few days hols?"
    case CharacterName.Ali   =>  "Can you explain what a monad is one more time?"
  }
}
