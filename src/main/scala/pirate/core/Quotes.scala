package pirate.core

import Constants.CharacterName

object Quotes {
  val quotes: Map[CharacterName, String] = Map(
    CharacterName.Dougie -> "Dave, where can I find an organogram?"
  )
  def get(characterName: CharacterName): String =
    quotes.getOrElse(characterName, "I'm sorry Dave, I'm afraid I can't do that.")
}
