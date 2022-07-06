package pirate.scenes.level.model

import indigo.*
import indigoextras.geometry.{BoundingBox, LineSegment, Vertex}
import indigoextras.geometry.BoundingBox.{contract, toLineSegments}
import pirate.core.Assets
import pirate.core.Constants.CharacterName
import pirate.scenes.level.model.ItvCharacter.Action
import pirate.scenes.level.model.LevelModel.ScreenChange

final case class PlayerCharacter(character: ItvCharacter) {
  def update(
      gameTime: GameTime,
      commands: Set[Action],
      platform: Platform,
      otherCharacters: List[ItvCharacter]
  ): Outcome[(ScreenChange, PlayerCharacter)] =
    character.update(gameTime, commands, platform, otherCharacters, None).map { char =>
      if (char.boundingBox.x < (0 - char.boundingBox.width))
        val xTransform = platform.columnCount.toDouble
        val yRound     = char.boundingBox.y.round - char.boundingBox.y
        val moved      = char.boundingBox.moveBy(xTransform, yRound)
        IndigoLogger.info(
          s"SCREEN CHANGE: PREVIOUS (${char.boundingBox} to $moved)"
        )
        ScreenChange.Previous -> PlayerCharacter(char.copy(boundingBox = moved))
      else if (char.boundingBox.x > (platform.columnCount))
        val xTransform = -platform.columnCount.toDouble
        val yRound     = char.boundingBox.y.round - char.boundingBox.y
        val moved      = char.boundingBox.moveBy(xTransform, yRound)
        IndigoLogger.info(
          s"SCREEN CHANGE: NEXT (${char.boundingBox} to $moved)"
        )
        ScreenChange.Next -> PlayerCharacter(char.copy(boundingBox = moved))
      else ScreenChange.Remain -> PlayerCharacter(char)
    }
}

/** Based off the original pirate. This thing is collidable, and can fall, and may or may not be controlled by the
  * input.
  */
final case class ItvCharacter(
    boundingBox: BoundingBox,
    state: CharacterState,
    lastRespawn: Seconds,
    lastSpoke: Seconds,
    lastYSpeed: Double,
    useInput: Boolean,
    name: CharacterName,
    respawnPoint: Vertex
) {

  /** Change from the demo - this is the top right corner of Pirate Dave, NOT the centre-bottom
    */
  val position: Vertex = boundingBox.position

  val center: Vertex = boundingBox.center

  def update(
      gameTime: GameTime,
      commands: Set[Action],
      platform: Platform,
      otherCharacters: List[ItvCharacter],
      interactingCharacter: Option[CharacterName]
  ): Outcome[ItvCharacter] = {
    import indigo.IndigoLogger._
    import ItvCharacter.Action._

    val collisionBounds = platform.copy(navMesh = platform.navMesh ::: otherCharacters.map(_.boundingBox))

    val actions = if (useInput) commands else Set.empty

    val xMove: Double = {
      val dir           = if (actions.movesRight) 1.0d else if (actions.movesLeft) -1.0d else 0.0d
      val speed         = if (state.inMidAir) 2d else 3d
      val inputX        = dir * speed
      val movedXAttempt = boundingBox.moveBy(inputX * gameTime.delta.toDouble, 0)
      if (collisionBounds.hitTest(movedXAttempt).isDefined) inputX * -0.1d else inputX
    }

    val jump           = actions.jumps && !state.inMidAir
    val ySpeed: Double = ItvCharacter.decideNextSpeedY(lastYSpeed, jump)

    val proposedBounds = boundingBox.moveBy(Vertex(xMove, ySpeed) * gameTime.delta.toDouble)
    val nextBounds     = ItvCharacter.adjustOnCollision(collisionBounds, proposedBounds)

    val nextState = ItvCharacter.nextStateFromLocationDiff(state, boundingBox.position, nextBounds.position)

    // Respawn if the pirate is below the bottom of the map.
    if (nextBounds.y > platform.rowCount.toDouble + 1)
      consoleLog(s"Respawn! $nextBounds")
      Outcome(
        copy(
          boundingBox = nextBounds.moveTo(respawnPoint),
          state = nextState,
          lastRespawn = gameTime.running,
          lastYSpeed = 0
        )
      )
        .addGlobalEvents(PlaySound(Assets.Sounds.respawnSound, Volume.Max))
    else {
      val maybeJumpSound =
        if (!state.inMidAir && nextState.isJumping)
          List(PlaySound(Assets.Sounds.jumpSound, Volume.Max))
        else Nil

      // consoleLog(s"States: $state - $nextState")
      Outcome(
        copy(
          boundingBox = nextBounds,
          state = nextState,
          lastRespawn = lastRespawn,
          lastSpoke = if (interactingCharacter.contains(name)) gameTime.running else lastSpoke,
          lastYSpeed = (nextBounds.y - boundingBox.y) / gameTime.delta.toDouble
        )
      )
        .addGlobalEvents(maybeJumpSound)
    }
  }
}

object ItvCharacter {

  enum Action:
    case Jump      extends Action
    case MoveRight extends Action
    case MoveLeft  extends Action
    case Interact  extends Action

  object Action {
    extension (actions: Set[Action]) {
      def jumps: Boolean      = actions.contains(Action.Jump)
      def movesRight: Boolean = actions.contains(Action.MoveRight) && !actions.contains(Action.MoveLeft)
      def movesLeft: Boolean  = actions.contains(Action.MoveLeft) && !actions.contains(Action.MoveRight)
      def interacts: Boolean  = actions.contains(Action.Interact)
    }
  }

  val inputMappings: InputMapping[Set[Action]] =
    InputMapping(
      Combo.withKeyInputs(Key.LEFT_ARROW, Key.UP_ARROW)  -> Set(Action.MoveLeft, Action.Jump),
      Combo.withKeyInputs(Key.LEFT_ARROW)                -> Set(Action.MoveLeft),
      Combo.withKeyInputs(Key.RIGHT_ARROW, Key.UP_ARROW) -> Set(Action.MoveRight, Action.Jump),
      Combo.withKeyInputs(Key.RIGHT_ARROW)               -> Set(Action.MoveRight),
      Combo.withKeyInputs(Key.UP_ARROW)                  -> Set(Action.Jump),
      Combo.withKeyInputs(Key.SPACE)                     -> Set(Action.Interact)
    )

  // The model space is 1 unit per tile, a tile is 32 x 32.
  // I am deciding that all our sprites are a square the same size as a tile.
  // This gives us easy maths, because the BouncyDave sprite is 224x224, so we can scale between sprites and tiles by x7,
  // and between graphics and model by x32
  val size = Vertex(0.8d, 0.9d)

  def initialDave: PlayerCharacter = PlayerCharacter(
    ItvCharacter(
      BoundingBox(Vertex(1.2d, 0.0d), size),
      CharacterState.FallingRight,
      Seconds.zero,
      Seconds.zero,
      0,
      useInput = true,
      CharacterName.Dave,
      Vertex(1.0, 0.0)
    )
  )

  /** Starting position is in model terms, so should be < (20, 17)
    */
  def otherItvCharacter(name: CharacterName, startingPosition: Vertex): ItvCharacter =
    ItvCharacter(
      BoundingBox(startingPosition, size),
      CharacterState.FallingRight,
      Seconds.zero,
      Seconds.zero,
      0,
      useInput = false,
      name,
      startingPosition
    )

  given CanEqual[Option[BoundingBox], Option[BoundingBox]] = CanEqual.derived

  /*
  Very important: only considers y.
  So the pirate only falls or doesn't fall, no bumping into things
   */
  def adjustOnCollision(platform: Platform, proposedBounds: BoundingBox): BoundingBox =
//    import indigo.IndigoLogger._
//    consoleLog(
//      s"Checking collision with proposedBounds of h: ${proposedBounds.height}, w: ${proposedBounds.width}, p: ${proposedBounds.position}"
//    )
    platform.hitTest(proposedBounds) match {
      case Some(value) =>
        proposedBounds.moveTo(proposedBounds.position.withY(value.y - proposedBounds.height))

      case None =>
        proposedBounds
    }

  val gravityIncrement: Double = 0.4d
  val jumpSpeed: Double        = -8.0d
  val terminalVelocity: Double = 8.0d
  val maxVerticalSpeed: Double = jumpSpeed

  def decideNextSpeedY(
      previousYSpeed: Double,
      jump: Boolean
  ): Double =
    if (jump) jumpSpeed else Math.max(Math.min(previousYSpeed + gravityIncrement, terminalVelocity), maxVerticalSpeed)

  def nextStateFromLocationDiff(
      previousState: CharacterState,
      oldLocation: Vertex,
      newLocation: Vertex
  ): CharacterState = {
    val locationDiff = newLocation - oldLocation

    if (locationDiff.y > -0.001 && locationDiff.y < 0.001)
      nextStanding(locationDiff.x)
    else if (newLocation.y > oldLocation.y)
      nextFalling(previousState)(locationDiff.x)
    else
      nextJumping(previousState)(locationDiff.x)
  }

  private def nextStateFromDiffX(
      movingLeft: CharacterState,
      movingRight: CharacterState,
      otherwise: CharacterState
  ): Double => CharacterState =
    xDiff =>
      if (xDiff < -0.01) movingLeft
      else if (xDiff > 0.01) movingRight
      else otherwise

  lazy val nextStanding: Double => CharacterState =
    nextStateFromDiffX(
      CharacterState.MoveLeft,
      CharacterState.MoveRight,
      CharacterState.Idle
    )

  def nextFalling(previousState: CharacterState): Double => CharacterState =
    nextStateFromDiffX(
      CharacterState.FallingLeft,
      CharacterState.FallingRight,
      previousState match {
        case CharacterState.FallingLeft | CharacterState.JumpingLeft =>
          CharacterState.FallingLeft

        case CharacterState.FallingRight | CharacterState.JumpingRight =>
          CharacterState.FallingRight

        case _ =>
          CharacterState.FallingRight
      }
    )

  def nextJumping(previousState: CharacterState): Double => CharacterState =
    nextStateFromDiffX(
      CharacterState.JumpingLeft,
      CharacterState.JumpingRight,
      previousState match {
        case l @ CharacterState.JumpingLeft  => l
        case r @ CharacterState.JumpingRight => r
        case _                               => CharacterState.JumpingRight
      }
    )

}
