package pirate.scenes.level.viewmodel

import indigo._
import indigoextras.geometry.Vertex
import pirate.scenes.level.model.ItvCharacter
import pirate.core.Constants.CharacterName

/*
The view model cannot be initialised at game start up, because we want to load
some data during the loading screen, parse it, and use it to build the
`worldToScreenSpace` function, which relies on knowing the size of the tiles
which is stored in the Tiled data.
 */
sealed trait LevelViewModel {
  val notReady: Boolean

  def update(gameTime: GameTime, characters: List[ItvCharacter]): Outcome[LevelViewModel]
}
object LevelViewModel {

  // The uninitialised ViewModel
  case object NotReady extends LevelViewModel {
    val notReady: Boolean = true

    def update(gameTime: GameTime, characters: List[ItvCharacter]): Outcome[LevelViewModel] =
      Outcome(this)
  }

  // The initialised / useable ViewModel
  final case class Ready(characterStates: Map[CharacterName, CharacterViewState]) extends LevelViewModel {
    val notReady: Boolean = false

    def update(gameTime: GameTime, characters: List[ItvCharacter]): Outcome[LevelViewModel] =
      Outcome
        .sequence(characters.map { character =>
          (characterStates.get(character.name) match {
            case Some(state) => state.update(gameTime, character)
            case None        => Outcome(CharacterViewState.initial)
          }).map(character.name -> _)
        })
        .map(states => Ready(states.toMap))
  }

  object Ready {
    val initial: Ready = Ready(Map.empty)
  }

}
