package pirate.core

import Constants.CharacterName

object Quotes {
  val quotes: Map[CharacterName, String] = Map(
    CharacterName.Dougie -> "Dave, where can I find an organogram?",
    CharacterName.Shah   -> "Fine! I wont call people cowboys in public channels again!",
    CharacterName.Pere   -> "Not Elm, please...",
    CharacterName.Maya   -> "Which service can I kill next?",
    CharacterName.Dan    -> "Dave, if I hear you mention\n the principle of least power one more time..."
  )
  def get(characterName: CharacterName): String =
    quotes.getOrElse(characterName, "I'm sorry Dave, I'm afraid I can't do that.")
}
