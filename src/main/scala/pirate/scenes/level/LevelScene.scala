package pirate.scenes.level

import indigo.*
import indigo.scenes.*
import pirate.scenes.level.subsystems.CloudsAutomata
import pirate.scenes.level.subsystems.CloudsSubSystem
import pirate.scenes.level.LevelView
import pirate.core.{Constants, Model, StartupData, ViewModel}
import pirate.core.Constants.CharacterName
import pirate.scenes.level.model.Platform
import pirate.scenes.level.model.LevelModel
import pirate.scenes.level.model.LevelModel.Screen
import pirate.scenes.level.viewmodel.LevelViewModel
import pirate.scenes.level.viewmodel.CharacterViewState
import pirate.scenes.level.model.ItvCharacter
import indigoextras.geometry.Vertex
import pirate.scenes.level.model.ItvCharacter.otherItvCharacter

final case class LevelScene(screenWidth: Int) extends Scene[StartupData, Model, ViewModel] {
  type SceneModel     = LevelModel
  type SceneViewModel = LevelViewModel

  val name: SceneName = LevelScene.name
  val modelLens: Lens[Model, LevelModel] =
    Lens(
      _.gameScene,
      (m, sm) => m.copy(gameScene = sm)
    )

  val viewModelLens: Lens[ViewModel, LevelViewModel] =
    Lens(
      _.level,
      (vm, svm) => vm.copy(level = svm)
    )

  val eventFilters: EventFilters =
    EventFilters.Restricted

  val subSystems: Set[SubSystem] =
    Set(
      CloudsAutomata.automata,
      CloudsSubSystem(screenWidth)
    )

  def updateModel(
      context: FrameContext[StartupData],
      model: LevelModel
  ): GlobalEvent => Outcome[LevelModel] = {
    case FrameTick if model.notReady =>
      (model, context.startUpData.levelDataStore) match {
        case (LevelModel.NotReady, Some(levelDataStore)) =>
          Outcome(
            LevelModel.Ready(
              Screen.Zero,
              ItvCharacter.initialDave,
              Map(
                Screen.Zero -> List(
                  otherItvCharacter(CharacterName.Dougie, Vertex(7, 0)),
                  otherItvCharacter(CharacterName.Josh, Vertex(5, 0)),
                  otherItvCharacter(CharacterName.Becky, Vertex(10, 0)),
                  otherItvCharacter(CharacterName.Rob, Vertex(14, 0))
                ),
                Screen.One -> List(
                  otherItvCharacter(CharacterName.Maya, Vertex(10, 0)),
                  otherItvCharacter(CharacterName.Luke, Vertex(14, 0)),
                  otherItvCharacter(CharacterName.Anne, Vertex(5, 0))
                ),
                Screen.Two -> List(otherItvCharacter(CharacterName.Shah, Vertex(8, 0))),
                Screen.Three -> List(
                  otherItvCharacter(CharacterName.Pere, Vertex(10, 0)),
                  otherItvCharacter(CharacterName.Miles, Vertex(14, 0))
                ),
                Screen.Four -> List(
                  otherItvCharacter(CharacterName.Lee, Vertex(9, 0)),
                  otherItvCharacter(CharacterName.Dan, Vertex(11, 0)),
                  otherItvCharacter(CharacterName.Gokce, Vertex(6, 0))
                )
              ),
              Platform.fromTerrainMap(levelDataStore.terrainMap)
            )
          )

        case _ =>
          Outcome(model)
      }

    case FrameTick =>
      model.update(context.gameTime, context.inputState)

    case _ =>
      Outcome(model)
  }

  def updateViewModel(
      context: FrameContext[StartupData],
      model: LevelModel,
      viewModel: LevelViewModel
  ): GlobalEvent => Outcome[LevelViewModel] = {
    case FrameTick if viewModel.notReady =>
      (viewModel, context.startUpData.levelDataStore) match {
        case (LevelViewModel.NotReady, Some(_)) =>
          Outcome(LevelViewModel.Ready.initial)

        case _ =>
          Outcome(viewModel)
      }

    case FrameTick =>
      model match {
        case LevelModel.NotReady =>
          Outcome(viewModel)

        case LevelModel.Ready(_, dave, characters, _) =>
          viewModel.update(context.gameTime, dave.character +: characters.values.flatten.toList)
      }

    case _ =>
      Outcome(viewModel)
  }

  def present(
      context: FrameContext[StartupData],
      model: LevelModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      (model, viewModel) match {
        case (m @ LevelModel.Ready(_, _, _, _), vm @ LevelViewModel.Ready(_)) =>
          LevelView
            .draw(
              context.gameTime,
              m,
              vm,
              context.startUpData.spritesByName,
              context.startUpData.levelDataStore,
              context.startUpData.screenData
            )
            .withCamera {
              val desiredVertex = m.dave.character.center + Vertex(8, 4.5)
              println(s"DESIRED VERTEX IS $desiredVertex")
              val maxX: Double = 20
              val maxY: Double = 11.5
              val minX: Double = 13.25
              val minY: Double = 7
              val clamped      = desiredVertex.clamp(Vertex(minX, minY), Vertex(maxX, maxY))
              Camera.LookAt(
                Constants.MagicNumbers.modelPointScaledToView(clamped),
                Zoom(1.5),
                Radians.zero
              )
            }

        case _ =>
          SceneUpdateFragment.empty
      }
    )
}

object LevelScene {
  val name: SceneName = SceneName("demo")
}
