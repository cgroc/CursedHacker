package pirate.scenes.level.model

/*
An ADT of all the states the pirate can find himself in.
 */
sealed trait CharacterState {

  val isFalling: Boolean =
    this match {
      case CharacterState.FallingLeft  => true
      case CharacterState.FallingRight => true
      case _                           => false
    }

  val isJumping: Boolean =
    this match {
      case CharacterState.JumpingLeft  => true
      case CharacterState.JumpingRight => true
      case _                           => false
    }

  val inMidAir: Boolean =
    isFalling || isJumping

}
object CharacterState {
  case object Idle         extends CharacterState
  case object MoveLeft     extends CharacterState
  case object MoveRight    extends CharacterState
  case object FallingLeft  extends CharacterState
  case object FallingRight extends CharacterState
  case object JumpingLeft  extends CharacterState
  case object JumpingRight extends CharacterState
}
