package pirate.core

import indigo.{Point, Rectangle}
import indigoextras.geometry.BoundingBox
import indigoextras.geometry.Vertex

object MagicNumbers {
  inline val aespriteSize = 224d
  inline val tileSize     = 32d
  inline val modelSize    = 1d

  /** The terrain tile size is 32 pixels by 32 pixels BouncyDave has a tile size of 224 pixels by 224 pixels Therefore
    * BouncyDave needs scaling by 1/7, because we need to turn its 224 pixel into 32 pixels
    */
  inline def bouncyDaveScaleFactor: Double = tileSize / aespriteSize

  /** The terrain tile size is 32 pixels by 32 pixels Our terrain bounding boxes are 1x1 pixels So to translate between
    * the model and the view, we need to x32
    */
  inline def modelToViewScaleFactor: Double = tileSize / modelSize

  inline def modelBoxScaledToView(box: BoundingBox): Rectangle = (box * modelToViewScaleFactor).toRectangle
  inline def modelPointScaledToView(point: Vertex): Point      = (point * modelToViewScaleFactor).toPoint
}
