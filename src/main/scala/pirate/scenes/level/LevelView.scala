package pirate.scenes.level

import indigo.*
import pirate.core.{Assets, Constants, LevelDataStore}
import indigoextras.geometry.{BoundingBox, Vertex}
import pirate.core.Constants.CharacterName
import pirate.core.Quotes
import pirate.scenes.level.model.{CharacterState, ItvCharacter, LevelModel}
import pirate.scenes.level.viewmodel.LevelViewModel
import pirate.scenes.level.viewmodel.CharacterViewState
import pirate.core.ScreenData

object LevelView {

  // If we're being classic, should be like a green and black checked box or something. I miss WC3 world editor
  def errorSprite: Sprite[Material.ImageEffects] = ???

  def draw(
      gameTime: GameTime,
      model: LevelModel.Ready,
      viewModel: LevelViewModel.Ready,
      spritesByName: CharacterName => Sprite[Material.ImageEffects],
      levelDataStore: Option[LevelDataStore],
      backgroundPerScreen: LevelModel.Screen => ScreenData
  ): SceneUpdateFragment =
    Level.draw(levelDataStore, backgroundPerScreen, model.currentScreen) |+|
      (if (Constants.Debug.drawTerrainBoxes)
         SceneUpdateFragment.empty.addLayer(
           Layer(
             model.platform.navMesh.map { box =>
               Shape.Box(
                 Constants.MagicNumbers.modelBoxScaledToView(box),
                 Fill.None,
                 Stroke(1, RGBA.Green)
               )
             }
           )
         )
       else SceneUpdateFragment.empty) |+|
      (model.dave.character +: model.currentCharacters)
        .map { character =>
          CharacterDrawer.draw(
            gameTime,
            character,
            viewModel.characterStates.getOrElse(character.name, CharacterViewState.initial),
            spritesByName(character.name),
            debug = Constants.Debug.drawCharacterBoxes
          )
        }
        .reduceOption(_ |+| _)
        .getOrElse(SceneUpdateFragment.empty)

  object Level {

    def draw(
        levelDataStore: Option[LevelDataStore],
        screenDatas: LevelModel.Screen => ScreenData,
        currentScreen: LevelModel.Screen
    ): SceneUpdateFragment =
      levelDataStore
        .map { assets =>
          SceneUpdateFragment.empty
            .addLayer(
              Layer(
                BindingKey("background"),
                List(Graphic(Rectangle(0, 0, 640, 360), 50, screenDatas(currentScreen).background)) ++
                  (if (currentScreen == LevelModel.Screen.End) Nil
                   else drawWater(assets.waterReflections))
              )
            )
            .addLayer(
              Layer(BindingKey("game"), drawForeground(assets, currentScreen))
            )
            .withAudio(
              SceneAudio(
                SceneAudioSource(
                  BindingKey(Assets.Sounds.shanty.toString),
                  PlaybackPattern.SingleTrackLoop(
                    Track(Assets.Sounds.shanty, Volume(0.5))
                  )
                )
              )
            )
        }
        .getOrElse(SceneUpdateFragment.empty)

    def drawWater(waterReflections: Sprite[Material.Bitmap]): List[SceneNode] =
      List(
        waterReflections.play(),
        waterReflections.moveBy(150, 30).play(),
        waterReflections.moveBy(-100, 60).play()
      )

    def drawForeground(assets: LevelDataStore, currentScreen: LevelModel.Screen): List[SceneNode] = {
      val isLastScreen  = currentScreen == LevelModel.Screen.End
      val isFirstScreen = currentScreen == LevelModel.Screen.Start
      List(
        Option.unless(isLastScreen)(assets.flag.play()),
        Some(assets.itv.play()),
        Option.unless(isLastScreen)(Assets.Trees.tallTrunkGraphic.moveTo(420, 204)),
        Option.unless(isLastScreen)(Assets.Trees.leftLeaningTrunkGraphic.moveTo(100, 254)),
        Option.unless(isLastScreen)(Assets.Trees.rightLeaningTrunkGraphic.moveTo(25, 166)),
        Option.unless(isLastScreen)(assets.backTallPalm.moveTo(420, 194).changeCycle(CycleLabel("P Back")).play()),
        Option.unless(isLastScreen)(assets.palm.moveTo(397, 172).play()),
        Option.unless(isLastScreen)(assets.palm.moveTo(77, 219).play()),
        Option.unless(isLastScreen)(assets.palm.moveTo(37, 120).play()),
        Option.unless(isLastScreen)(Assets.Static.chestGraphic.moveTo(380, 256)),
        if (isFirstScreen) Some(assets.terrainNoLeftDoor) else Some(assets.terrain)
      ).flatten
    }
  }

  object CharacterDrawer {

    def draw(
        gameTime: GameTime,
        character: ItvCharacter,
        characterViewState: CharacterViewState,
        sprite: Sprite[Material.ImageEffects],
        debug: Boolean
    ): SceneUpdateFragment =
      SceneUpdateFragment.empty
        .addLayer(
          Layer(
            BindingKey("game"),
            respawnEffect(
              gameTime,
              character.lastRespawn,
              updatedCharacter(
                character,
                characterViewState,
                sprite,
                (v: Vertex) => (v * Constants.MagicNumbers.tileSize).toPoint
              )
            )
          )
        )
        .addLayer(
          if (debug)
            Layer(
              Shape.Box(
                Constants.MagicNumbers.modelBoxScaledToView(character.boundingBox),
                Fill.None,
                Stroke(1, RGBA.Red)
              )
            )
          else Layer.empty
        )
        .addLayer(
          if (
            character.lastSpoke != Seconds.zero && (gameTime.running - character.lastSpoke).toInt < Constants.MagicNumbers.speechDurationSeconds
          )
            Layer(
              Text(
                s"${character.name.toString}:\n ${Quotes.get(character.name)}",
                Constants.MagicNumbers.modelBoxScaledToView(character.boundingBox).center.x,
                Constants.MagicNumbers.modelBoxScaledToView(character.boundingBox).center.y,
                1,
                Assets.Fonts.fontKey,
                Assets.Fonts.fontMaterial
              ).alignCenter
            )
          else Layer.empty
        )

    val respawnFlashSignal: Seconds => Signal[(Boolean, Boolean)] =
      lastRespawn => Signal(_ < lastRespawn + Seconds(1.2)) |*| Signal.Pulse(Seconds(0.1))

    val captainWithAlpha
        : Sprite[Material.ImageEffects] => SignalFunction[(Boolean, Boolean), Sprite[Material.ImageEffects]] =
      captain =>
        SignalFunction {
          case (false, _) =>
            captain

          case (true, true) =>
            captain
              .modifyMaterial(_.withAlpha(1))

          case (true, false) =>
            captain
              .modifyMaterial(_.withAlpha(0))
        }

    def respawnEffect(
        gameTime: GameTime,
        lastRespawn: Seconds,
        captain: Sprite[Material.ImageEffects]
    ): Sprite[Material.ImageEffects] =
      (respawnFlashSignal(lastRespawn) |> captainWithAlpha(captain)).at(gameTime.running)

    def updatedCharacter(
        character: ItvCharacter,
        characterViewState: CharacterViewState,
        sprite: Sprite[Material.ImageEffects],
        toScreenSpace: Vertex => Point
    ): Sprite[Material.ImageEffects] =
      sprite
        .moveTo(toScreenSpace(character.position))
        .flipHorizontal(true)
        .changeCycle(CycleLabel("Idle"))
        .play()

//      pirate.state match {
//        case CharacterState.Idle if characterViewState.facingRight =>
//          captain
//            .moveTo(toScreenSpace(pirate.position))
//            .changeCycle(CycleLabel("Idle"))
//            .play()
//
//        case CharacterState.Idle =>
//          captain
//            .moveTo(toScreenSpace(pirate.position))
//            .flipHorizontal(true)
//            .moveBy(-20, 0)
//            .changeCycle(CycleLabel("Idle"))
//            .play()
//
//        case CharacterState.MoveLeft =>
//          captain
//            .moveTo(toScreenSpace(pirate.position))
//            .flipHorizontal(true)
//            .moveBy(-20, 0)
//            .changeCycle(CycleLabel("Run"))
//            .play()
//
//        case CharacterState.MoveRight =>
//          captain
//            .moveTo(toScreenSpace(pirate.position))
//            .changeCycle(CycleLabel("Run"))
//            .play()
//
//        case CharacterState.FallingRight =>
//          captain
//            .moveTo(toScreenSpace(pirate.position))
//            .changeCycle(CycleLabel("Fall"))
//            .play()
//
//        case CharacterState.FallingLeft =>
//          captain
//            .moveTo(toScreenSpace(pirate.position))
//            .flipHorizontal(true)
//            .moveBy(-20, 0)
//            .changeCycle(CycleLabel("Fall"))
//            .play()
//
//        case CharacterState.JumpingRight =>
//          captain
//            .moveTo(toScreenSpace(pirate.position))
//            .changeCycle(CycleLabel("Jump"))
//            .play()
//
//        case CharacterState.JumpingLeft =>
//          captain
//            .moveTo(toScreenSpace(pirate.position))
//            .flipHorizontal(true)
//            .moveBy(-20, 0)
//            .changeCycle(CycleLabel("Jump"))
//            .play()
//      }
  }
}
