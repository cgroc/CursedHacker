package pirate.core

import indigo.{Point, Rectangle}
import indigoextras.geometry.BoundingBox
import indigoextras.geometry.Vertex

object Constants {
  inline val frameRate = 60
  enum CharacterName:
    case Dan    extends CharacterName
    case Dave   extends CharacterName
    case Dougie extends CharacterName
    case Maya   extends CharacterName
    case Pere   extends CharacterName
    case Shah   extends CharacterName
    case Lee    extends CharacterName
    case Gokce  extends CharacterName
    case Rob    extends CharacterName
    case Josh   extends CharacterName
    case Becky  extends CharacterName
    case Miles  extends CharacterName
    case Anne   extends CharacterName
    case Luke   extends CharacterName
    case Ali    extends CharacterName

  object Debug {
    // Do you want a red box highlighting the bounding box of each character
    inline val drawCharacterBoxes = false
    inline val drawTerrainBoxes   = false
  }

  object MagicNumbers {

    inline val speechDurationSeconds = 5

    inline val tilesAcrossScreen = 20

    inline val aespriteSize = 224d
    inline val tileSize     = 32d
    inline val modelSize    = 1d

    /** The terrain tile size is 32 pixels by 32 pixels BouncyDave has a tile size of 224 pixels by 224 pixels Therefore
      * BouncyDave needs scaling by 1/7, because we need to turn its 224 pixel into 32 pixels
      */
    inline def bouncyDaveScaleFactor: Double = tileSize / aespriteSize

    /** The terrain tile size is 32 pixels by 32 pixels Our terrain bounding boxes are 1x1 pixels So to translate
      * between the model and the view, we need to x32
      */
    inline def modelToViewScaleFactor: Double = tileSize / modelSize

    inline def modelBoxScaledToView(box: BoundingBox): Rectangle = (box * modelToViewScaleFactor).toRectangle

    inline def modelPointScaledToView(point: Vertex): Point = (point * modelToViewScaleFactor).toPoint

    inline val itvxSize = 320d

    /** Roughest maths ever - ITVX image is split into letters about 273 each. and 364 high. I am calling this a 320
      * square, for the maths. So it's 10x too big compared to a single terrain tile
      */
    inline def itvxScaleFactor: Double = tileSize / itvxSize
  }
}
