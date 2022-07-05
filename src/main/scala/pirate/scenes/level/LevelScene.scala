package pirate.scenes.level

import indigo._
import indigo.scenes._

import pirate.scenes.level.subsystems.CloudsAutomata
import pirate.scenes.level.subsystems.CloudsSubSystem
import pirate.scenes.level.LevelView
import pirate.core.{StartupData, Model, ViewModel}
import pirate.core.Constants.CharacterName
import pirate.scenes.level.model.Platform
import pirate.scenes.level.model.LevelModel
import pirate.scenes.level.model.LevelModel.Screen
import pirate.scenes.level.viewmodel.LevelViewModel
import pirate.scenes.level.viewmodel.CharacterViewState
import pirate.scenes.level.model.ItvCharacter
import indigoextras.geometry.Vertex

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
                Screen.Zero -> List(ItvCharacter.otherItvCharacter(CharacterName.Dougie, Vertex(7d, 0d))),
                Screen.One  -> List(ItvCharacter.otherItvCharacter(CharacterName.Maya, Vertex(10d, 0d))),
                Screen.Two  -> List(ItvCharacter.otherItvCharacter(CharacterName.Shah, Vertex(8d, 0d)))
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
          LevelView.draw(context.gameTime, m, vm, context.startUpData.spritesByName, context.startUpData.levelDataStore)

        case _ =>
          SceneUpdateFragment.empty
      }
    )
}

object LevelScene {
  val name: SceneName = SceneName("demo")
}
