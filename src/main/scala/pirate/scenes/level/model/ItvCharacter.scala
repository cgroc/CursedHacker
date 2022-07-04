package pirate.scenes.level.model

import indigo.*
import indigoextras.geometry.{BoundingBox, LineSegment, Vertex}
import indigoextras.geometry.BoundingBox.toLineSegments
import pirate.core.Assets
import pirate.core.Constants.CharacterName

/** Based off the original pirate. This thing is collidable, and can fall, and may or may not be controlled by the
  * input.
  */
final case class ItvCharacter(
    boundingBox: BoundingBox,
    state: CharacterState,
    lastRespawn: Seconds,
    yDelta: Double,
    useInput: Boolean,
    name: CharacterName,
    respawnPoint: Vertex
) {

  /** Change from the demo - this is the top right corner of Pirate Dave, NOT the centre-bottom
    */
  val position: Vertex = boundingBox.position

  val center: Vertex = boundingBox.center

  /** At the moment this goes:
    *   - calculate a desired movement vector based on the user input and whether he's falling
    *   - attempt to move him by the X of this movement vector (because you can only control the X movement directly)
    *     and current Y speed
    *   - if he's inside something, move to be above the thing
    *   - some logic to figure out the y speed, which includes gravity, the previous position, god knows
    *   - calculate the next state (idle, falling, walking, jumping) based off the change in position
    *
    * How about instead:
    *   - do x and y separately.
    *   - x is simple - you try and move, if you'll end up inside something, you aren't allowed, so no x movement
    *   - y is more complicated - if you're not in midair, and you jump, you get an instant y boost
    *   - then we calculate the next y speed, which has some fun gravity effect
    */
  def update(gameTime: GameTime, inputState: InputState, platform: Platform): Outcome[ItvCharacter] = {
    import indigo.IndigoLogger._
    import ItvCharacter.Action._

    val actions = if (useInput) inputState.mapInputs(ItvCharacter.inputMappings, Set.empty) else Set.empty

    val inputX: Double = {
      val dir   = if (actions.movesRight) 1.0d else if (actions.movesLeft) -1.0d else 0.0d
      val speed = if (state.inMidAir) 2d else 3d
      dir * speed
    }

    val movedXAttempt = boundingBox.moveBy(inputX * gameTime.delta.toDouble, 0)
    val xMove         = if (platform.hitTest(movedXAttempt).isDefined) 0d else inputX

    val jump           = actions.jumps && !state.inMidAir
    val ySpeed: Double = ItvCharacter.decideNextSpeedY(previousYSpeed = yDelta / gameTime.delta.toDouble, jump)

    val proposedBounds = boundingBox.moveBy(Vertex(xMove, ySpeed) * gameTime.delta.toDouble)
    val nextBounds     = ItvCharacter.adjustOnCollision(platform, proposedBounds)

    val nextState = ItvCharacter.nextStateFromLocationDiff(state, boundingBox.position, nextBounds.position)

    // Respawn if the pirate is below the bottom of the map.
    if (nextBounds.y > platform.rowCount.toDouble + 1)
      consoleLog(s"Respawn! $nextBounds")
      Outcome(
        copy(
          boundingBox = nextBounds.moveTo(respawnPoint),
          state = nextState,
          lastRespawn = gameTime.running,
          yDelta = 0
        )
      )
        .addGlobalEvents(PlaySound(Assets.Sounds.respawnSound, Volume.Max))
    else {
      val maybeJumpSound =
        if (!state.inMidAir && nextState.isJumping)
          List(PlaySound(Assets.Sounds.jumpSound, Volume.Max))
        else Nil

      consoleLog(s"States: $state - $nextState")
      Outcome(
        copy(
          boundingBox = nextBounds,
          state = nextState,
          lastRespawn = lastRespawn,
          yDelta = nextBounds.y - boundingBox.y
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

  object Action {
    extension (actions: Set[Action]) {
      def jumps: Boolean      = actions.contains(Action.Jump)
      def movesRight: Boolean = actions.contains(Action.MoveRight) && !actions.contains(Action.MoveLeft)
      def movesLeft: Boolean  = actions.contains(Action.MoveLeft) && !actions.contains(Action.MoveRight)
    }
  }

  val inputMappings: InputMapping[Set[Action]] =
    InputMapping(
      Combo.withKeyInputs(Key.LEFT_ARROW, Key.UP_ARROW)  -> Set(Action.MoveLeft, Action.Jump),
      Combo.withKeyInputs(Key.LEFT_ARROW, Key.SPACE)     -> Set(Action.MoveLeft, Action.Jump),
      Combo.withKeyInputs(Key.LEFT_ARROW)                -> Set(Action.MoveLeft),
      Combo.withKeyInputs(Key.RIGHT_ARROW, Key.UP_ARROW) -> Set(Action.MoveRight, Action.Jump),
      Combo.withKeyInputs(Key.RIGHT_ARROW, Key.SPACE)    -> Set(Action.MoveRight, Action.Jump),
      Combo.withKeyInputs(Key.RIGHT_ARROW)               -> Set(Action.MoveRight),
      Combo.withKeyInputs(Key.UP_ARROW)                  -> Set(Action.Jump),
      Combo.withKeyInputs(Key.SPACE)                     -> Set(Action.Jump)
    )

  // The model space is 1 unit per tile, a tile is 32 x 32.
  // I am deciding that all our sprites are a square the same size as a tile.
  // This gives us easy maths, because the BouncyDave sprite is 224x224, so we can scale between sprites and tiles by x7,
  // and between graphics and model by x32
  val size = Vertex(1d, 1d)

  def initialDave: ItvCharacter =
    ItvCharacter(
      BoundingBox(Vertex(1.0d, 0.0d), size),
      CharacterState.FallingRight,
      Seconds.zero,
      0,
      useInput = true,
      CharacterName.Dave,
      Vertex(1.0, 0.0)
    )

  /** Starting position is in model terms, so should be < (20, 17)
    */
  def otherItvCharacter(name: CharacterName, startingPosition: Vertex): ItvCharacter =
    ItvCharacter(
      BoundingBox(startingPosition, size),
      CharacterState.FallingRight,
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
  def adjustOnCollision(platform: Platform, proposedBounds: BoundingBox): BoundingBox = {

    import indigo.IndigoLogger._
    consoleLog(
      s"Checking collision with proposedBounds of h: ${proposedBounds.height}, w: ${proposedBounds.width}, p: ${proposedBounds.position}"
    )
    platform.hitTest(proposedBounds) match {
      case Some(value) =>
        proposedBounds.moveTo(proposedBounds.position.withY(value.y - proposedBounds.height))

      case None =>
        proposedBounds
    }
  }

  /** For each thing you collide with, work out the vector to move you OUT of that thing. This will almost certainly
    * break down in hilarious ways if you have multiple corners inside.
    *
    * In fact if I allow attempted X movement it already breaks down and throws you out the top of the world, but hey
    * Calls itself recursively so that you can't get stuck in walls (in theory)
    */
  def mattsDraftCollision(platform: Platform, proposedBounds: BoundingBox): BoundingBox = {
    import indigo.IndigoLogger._
    def desc(boundingBox: BoundingBox): String =
      boundingBox.corners.map(p => s"(${p.x},${p.y})").mkString("(", ",", ")")

    // consoleLog(s"Checking collision with proposedBounds of: ${desc(proposedBounds)}")

    extension (line: LineSegment) {
      def customClosestPointOnLine(to: Vertex): Option[Vertex] = {

        /** I think I've found a bug in Indigo: LineSegment.closestPointOnLine uses Vertex#clamp But this doesn't
          * account for the fact that min.x might be greater than max.x, even while min.y might be less than max.y. This
          * causes cases where: testLine.closestPointOnLine(testPoint) != testLine.invert.closestPointOnLine(testPoint)
          * I'm not sure if it's wrong for Vertex#clamp to not check the max and mins, or LineSegment#closestPointOnLine
          * to call it without checking
          */
        def customClamp(vertex: Vertex, min: Vertex, max: Vertex): Vertex = {
          import vertex.{x, y}
          val minX = Math.min(min.x, max.x)
          val miny = Math.min(min.y, max.y)
          val maxX = Math.max(min.x, max.x)
          val maxY = Math.max(min.y, max.y)
          Vertex(
            x = Math.min(maxX, Math.max(minX, x)),
            y = Math.min(maxY, Math.max(miny, y))
          )
        }
        import line._
        val a   = end.y - start.y
        val b   = start.x - end.x
        val c1  = a * start.x + b * start.y
        val c2  = -b * to.x + a * to.y
        val det = a * a - -b * b
        if det != 0.0 then
          Some(
            customClamp(
              Vertex(
                x = (a * c1 - b * c2) / det,
                y = (a * c2 - -b * c1) / det
              ),
              start,
              end
            )
          )
        else None
      }
    }

    platform
      .hitTest(proposedBounds) match
      case Some(obj) =>
        consoleLog(s"Collided with ${desc(obj)}")
        val newBounds = proposedBounds.corners
          .find(obj.contains) // find YOUR corner which is overlapping
          .map(corner =>
            corner -> {
              val nearestEdge = obj.toLineSegments
                .filterNot(_.customClosestPointOnLine(corner).map(_.distanceTo(corner).abs).contains(0.0d))
                .minBy(_.customClosestPointOnLine(corner).map(_.distanceTo(corner).abs))
              consoleLog(s"Colliding corner was $corner")
              consoleLog(s"Nearest edge was $nearestEdge")
              val nearestPoint = nearestEdge.customClosestPointOnLine(corner)
              consoleLog(s"Nearest point was $nearestPoint")
              nearestPoint.getOrElse(corner)
            } // find the nearest corner to your overlapping corner
          )
          .map { case (me, edgePoint) => (edgePoint - me).toVector2 } // find the difference between them
          .foldLeft(proposedBounds) { (box, vector) => // move it out
            val r = box.moveBy(vector)
            consoleLog(s"Moved ${desc(box)} by $vector to ${desc(r)}")
            r
          }
        adjustOnCollision(platform, newBounds)
      case None => proposedBounds
  }

  val gravityIncrement: Double = 0.4d
  val jumpSpeed: Double        = -8.0d

  def decideNextSpeedY(
      previousYSpeed: Double,
      jump: Boolean
  ): Double =
    if (jump) jumpSpeed else Math.min(previousYSpeed + gravityIncrement, 8.0d)

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
