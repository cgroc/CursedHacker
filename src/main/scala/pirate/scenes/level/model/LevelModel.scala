package pirate.scenes.level.model

import indigo._

/*
The model cannot be initialised at game start up, because we want to load
some data during the loading screen, parse it, and use it to generate part
of the model. We _could_ represent that with an Option, but that could get
messy.
 */
sealed trait LevelModel {
  val notReady: Boolean

  def update(gameTime: GameTime, inputState: InputState): Outcome[LevelModel]
}
object LevelModel {
  sealed trait Screen {
    def next: Screen
    def prev: Screen
  }
  object Screen {
    // NOTE: When adding a screen, remember to point Zero.prev to your new screen for wrapping around
    case object Start extends Screen {
      override def next: Screen = Screen.One
      override def prev: Screen = Screen.Start
    }
    case object One extends Screen {
      override def next: Screen = Screen.Two
      override def prev: Screen = Screen.Start
    }
    case object Two extends Screen {
      override def next: Screen = Screen.Three
      override def prev: Screen = Screen.One
    }
    case object Three extends Screen {
      override def next: Screen = Screen.Four
      override def prev: Screen = Screen.Two
    }
    case object Four extends Screen {
      override def next: Screen = Screen.End
      override def prev: Screen = Screen.Three
    }

    case object End extends Screen {
      override def next: Screen = Screen.End
      override def prev: Screen = Screen.End
    }

    val all: Seq[Screen] = Seq(Start, One, Two, Three, Four, End)
  }

  enum ScreenChange:
    case Next     extends ScreenChange
    case Previous extends ScreenChange
    case Remain   extends ScreenChange

  case object NotReady extends LevelModel {
    val notReady: Boolean = true

    def update(gameTime: GameTime, inputState: InputState): Outcome[NotReady.type] =
      Outcome(this)
  }

  final case class Ready(
      currentScreen: Screen,
      dave: PlayerCharacter,
      characters: Map[Screen, List[ItvCharacter]],
      platformNormal: Platform,
      platformNoLeftDoor: Platform
  ) extends LevelModel {
    def platform: Platform = currentScreen match
      case Screen.Start => platformNoLeftDoor
      case _            => platformNormal

    val notReady: Boolean = false

    def currentCharacters: List[ItvCharacter] = characters.getOrElse(currentScreen, Nil)

    def update(gameTime: GameTime, inputState: InputState): Outcome[Ready] = {
      val actions = inputState.mapInputs(ItvCharacter.inputMappings, Set.empty)

      dave
        .update(gameTime, actions, platform, currentCharacters)
        .flatMap { case (nextScreen, newDave) =>
          nextScreen match
            case ScreenChange.Next =>
              IndigoLogger.info(s"$currentScreen to ${currentScreen.next}")
              Outcome {
                this.copy(dave = newDave, currentScreen = currentScreen.next)
              }
            case ScreenChange.Previous =>
              IndigoLogger.info(s"$currentScreen to ${currentScreen.prev}")
              Outcome {
                this.copy(dave = newDave, currentScreen = currentScreen.prev)
              }
            case ScreenChange.Remain =>
              val interactingCharacter =
                if (actions.interacts)
                  currentCharacters.minByOption(_.center.distanceTo(dave.character.center)).map(_.name)
                else None
              Outcome
                .sequence(
                  currentCharacters
                    .map(c =>
                      c.update(
                        gameTime,
                        actions,
                        platform,
                        newDave.character +: currentCharacters.filterNot(_ == c),
                        interactingCharacter
                      )
                    )
                )
                .map(c => this.copy(dave = newDave, characters = characters.updated(currentScreen, c)))
        }

    }
  }
}
