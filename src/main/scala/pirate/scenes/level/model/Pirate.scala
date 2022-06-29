package pirate.scenes.level.model

import indigo._
import indigoextras.geometry.BoundingBox
import indigoextras.geometry.Vertex
import pirate.core.Assets

final case class Pirate(
    boundingBox: BoundingBox,
    state: PirateState,
    lastRespawn: Seconds,
    ySpeed: Double
) {

  /** Change from the demo - this is the top right corner of Pirate Dave, NOT the centre-bottom
    */
  val position: Vertex = boundingBox.position

  val center: Vertex = boundingBox.center

  def update(gameTime: GameTime, inputState: InputState, platform: Platform): Outcome[Pirate] = {
    import indigo.IndigoLogger._
    val inputForce = inputState.mapInputs(Pirate.inputMappings(state.isFalling), Vector2.zero)

    val (nextBounds, collision) =
      Pirate.adjustOnCollision(
        platform,
        boundingBox.moveBy(
          Vertex(inputForce.x, ySpeed) * gameTime.delta.toDouble
        )
      )

    val ySpeedNext: Double =
      Pirate.decideNextSpeedY(state.inMidAir, boundingBox.y, nextBounds.y, ySpeed, inputForce.y)

    val nextState =
      Pirate.nextStateFromForceDiff(
        state,
        collision,
        boundingBox.position.toVector2,
        nextBounds.position.toVector2
      )

    // Respawn if the pirate is below the bottom of the map.
    if (nextBounds.y > platform.rowCount.toDouble + 1)
      consoleLog(s"Respawn! $nextBounds")
      Outcome(Pirate(nextBounds.moveTo(Pirate.RespawnPoint), nextState, gameTime.running, ySpeedNext))
        .addGlobalEvents(PlaySound(Assets.Sounds.respawnSound, Volume.Max))
    else {
      val maybeJumpSound =
        if (!state.inMidAir && nextState.isJumping)
          List(PlaySound(Assets.Sounds.jumpSound, Volume.Max))
        else Nil

      consoleLog(s"States: $state - $nextState")
      Outcome(Pirate(nextBounds, nextState, lastRespawn, ySpeedNext))
        .addGlobalEvents(maybeJumpSound)
    }
  }
}

object Pirate {

  // Where does the captain start in model terms?
  // Right in the middle, and off the top of the screen
  // by 2 units (tiles).
  val RespawnPoint = Vertex(0.0, 0.0)

  def initial: Pirate = {

    val startPosition = Vertex(0.0, 0.0)

    // The model space is 1 unit per tile, a tile is 32 x 32.
    // I am deciding that BouncyDave is a square the same size as a tile.
    // This gives us easy maths, because the BouncyDave sprite is 224x224, so we can scale between sprites and tiles by x7,
    // and between graphics and model by x32
    val size = Vertex(1d, 1d)

    Pirate(
      BoundingBox(startPosition, size),
      PirateState.FallingRight,
      Seconds.zero,
      0
    )
  }

  val inputMappings: Boolean => InputMapping[Vector2] = isFalling => {
    val xSpeed: Double = if (isFalling) 2.0d else 3.0d
    val ySpeed: Double = if (isFalling) 0.0d else -8.0d

    InputMapping(
      Combo.withKeyInputs(Key.LEFT_ARROW, Key.UP_ARROW)  -> Vector2(-xSpeed, ySpeed),
      Combo.withKeyInputs(Key.LEFT_ARROW, Key.SPACE)     -> Vector2(-xSpeed, ySpeed),
      Combo.withKeyInputs(Key.LEFT_ARROW)                -> Vector2(-xSpeed, 0.0d),
      Combo.withKeyInputs(Key.RIGHT_ARROW, Key.UP_ARROW) -> Vector2(xSpeed, ySpeed),
      Combo.withKeyInputs(Key.RIGHT_ARROW, Key.SPACE)    -> Vector2(xSpeed, ySpeed),
      Combo.withKeyInputs(Key.RIGHT_ARROW)               -> Vector2(xSpeed, 0.0d),
      Combo.withKeyInputs(Key.UP_ARROW)                  -> Vector2(0.0d, ySpeed),
      Combo.withKeyInputs(Key.SPACE)                     -> Vector2(0.0d, ySpeed),
      Combo.withGamepadInputs(
        GamepadInput.LEFT_ANALOG(_ < -0.5, _ => true, false),
        GamepadInput.Cross
      )                                                                             -> Vector2(-xSpeed, ySpeed),
      Combo.withGamepadInputs(GamepadInput.LEFT_ANALOG(_ < -0.5, _ => true, false)) -> Vector2(-xSpeed, 0.0d),
      Combo.withGamepadInputs(
        GamepadInput.LEFT_ANALOG(_ > 0.5, _ => true, false),
        GamepadInput.Cross
      )                                                                            -> Vector2(xSpeed, ySpeed),
      Combo.withGamepadInputs(GamepadInput.LEFT_ANALOG(_ > 0.5, _ => true, false)) -> Vector2(xSpeed, 0.0d),
      Combo.withGamepadInputs(GamepadInput.Cross)                                  -> Vector2(0.0d, ySpeed)
    )
  }

  given CanEqual[Option[BoundingBox], Option[BoundingBox]] = CanEqual.derived

  /*
  Very important: only considers y.
  So the pirate only falls or doesn't fall, no bumping into things
   */
  def adjustOnCollision(platform: Platform, proposedBounds: BoundingBox): (BoundingBox, Boolean) = {

    import indigo.IndigoLogger._
    // consoleLog(
    //  s"Checking collision with proposedBounds of h: ${proposedBounds.height}, w: ${proposedBounds.width}, p: ${proposedBounds.position}"
    // )
    platform.hitTest(proposedBounds) match {
      case Some(value) =>
        (
          proposedBounds
            .moveTo(proposedBounds.position.withY(value.y - proposedBounds.height)),
          true
        )

      case None =>
        (proposedBounds, false)
    }
  }

  val gravityIncrement: Double = 0.4d

  def decideNextSpeedY(
      inMidAir: Boolean,
      previousY: Double,
      nextY: Double,
      ySpeed: Double,
      inputY: Double
  ): Double =
    if (Math.abs(nextY - previousY) < 0.0001 && !inMidAir)
      gravityIncrement + inputY
    else if (ySpeed + gravityIncrement <= 8.0d)
      ySpeed + gravityIncrement
    else
      8.0d

  def nextStateFromForceDiff(
      previousState: PirateState,
      collisionOccurred: Boolean,
      oldForce: Vector2,
      newForce: Vector2
  ): PirateState = {
    val forceDiff = newForce - oldForce

    if (forceDiff.y > -0.001 && forceDiff.y < 0.001 && collisionOccurred)
      nextStanding(forceDiff.x)
    else if (newForce.y > oldForce.y)
      nextFalling(previousState)(forceDiff.x)
    else
      nextJumping(previousState)(forceDiff.x)

  }

  private def nextStateFromDiffX(
      movingLeft: PirateState,
      movingRight: PirateState,
      otherwise: PirateState
  ): Double => PirateState =
    xDiff =>
      if (xDiff < -0.01) movingLeft
      else if (xDiff > 0.01) movingRight
      else otherwise

  lazy val nextStanding: Double => PirateState =
    nextStateFromDiffX(
      PirateState.MoveLeft,
      PirateState.MoveRight,
      PirateState.Idle
    )

  def nextFalling(previousState: PirateState): Double => PirateState =
    nextStateFromDiffX(
      PirateState.FallingLeft,
      PirateState.FallingRight,
      previousState match {
        case PirateState.FallingLeft | PirateState.JumpingLeft =>
          PirateState.FallingLeft

        case PirateState.FallingRight | PirateState.JumpingRight =>
          PirateState.FallingRight

        case _ =>
          PirateState.FallingRight
      }
    )

  def nextJumping(previousState: PirateState): Double => PirateState =
    nextStateFromDiffX(
      PirateState.JumpingLeft,
      PirateState.JumpingRight,
      previousState match {
        case l @ PirateState.JumpingLeft  => l
        case r @ PirateState.JumpingRight => r
        case _                            => PirateState.JumpingRight
      }
    )

}
