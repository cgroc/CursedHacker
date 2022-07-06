package pirate.core

import Constants.CharacterName

object Quotes {
  val get: CharacterName => String = {
    case CharacterName.Dave   => "I'm sorry Dave, I'm afraid I can't do that."
    case CharacterName.Dougie => "Dave, where can I find an organogram?"
    case CharacterName.Shah   => "Fine! I wont call people cowboys in public channels again!"
    case CharacterName.Pere   => "Not Elm, please..."
    case CharacterName.Maya   => "Which service can I kill next?"
    case CharacterName.Lee    => "My happiness is directly proportional to my Intellij time"
    case CharacterName.Dan    => "Dave, if I hear you mention\n the principle of least power one more time..."
    case CharacterName.Gokce  => "TODO"
    case CharacterName.Miles  => "It works in my version of the Scala compiler ..."
    case CharacterName.Josh   => "Hi Dave, I have another question about purchase orders"
    case CharacterName.Becky  => "Dave is this legit weird or just how ITV does things…"
    case CharacterName.Rob    => "Who do I talk to about..."
  }
}
