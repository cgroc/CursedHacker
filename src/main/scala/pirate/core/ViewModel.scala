package pirate.core

import indigo._

import pirate.scenes.level.viewmodel.LevelViewModel

// Does literally nothing, but I can't be bothered to unthread it
final case class ViewModel(level: LevelViewModel)
object ViewModel {
  def initial: ViewModel =
    ViewModel(LevelViewModel.NotReady)
}
