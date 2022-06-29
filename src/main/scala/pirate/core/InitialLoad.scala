package pirate.core

import indigo.*
import indigo.json.Json
import indigo.shared.formats.TiledGridMap
import pirate.core.MagicNumbers

/*
In a nutshell, the setup function here takes the boot data (screen dimensions),
the asset collection, and a dice object, and produces "start up data", which is
totally user defined and you can do that however you like, you just need to return
a success or failure object.

What's really important to understand is that this function is run _more than once!_

The first time it runs, we only have available the assets we told indigo we needed
for the loading screen. We find this out by simply checking which assets are available
at the moment.

The second run is triggered by the completion of a dynamic asset load - you see the
progress of which on the loading screen. This can theoretically happen as many times
as you decide to load assets. So it's only on the second run that we do all the work
in `makeAdditionalAssets`.
 */
object InitialLoad {

  def setup(
      screenDimensions: Rectangle,
      assetCollection: AssetCollection,
      dice: Dice
  ): Outcome[Startup[StartupData]] =
    Outcome(
      (for {
        captain  <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.ref, Depth(2))
        maybeLds <- levelDataStore(screenDimensions, assetCollection, dice)
      } yield makeStartupData(captain, maybeLds)) match {
        case Left(message) =>
          IndigoLogger.consoleLog(s"ERROR STARTING $message")
          Startup.Failure(message)

        case Right(success) =>
          IndigoLogger.consoleLog(s"HAPPY STARTING $success")
          success
      }
    )

  def levelDataStore(
      screenDimensions: Rectangle,
      assetCollection: AssetCollection,
      dice: Dice
  ): Either[String, Option[(LevelDataStore, List[Animation])]] = {

    val loader: (AssetName, AssetName, Depth) => Either[String, SpriteAndAnimations] = (a, b, c) => {
      val res = loadAnimation(assetCollection, dice)(a, b, c)
      res match
        case Left(err) => IndigoLogger.error(s"Error loading $a: $err")
        case Right(_)  => IndigoLogger.info(s"Have loaded $a")
      res
    }

    // If these assets haven't been loaded yet, we're not going to try and process anything.
    if (
      assetCollection.findTextDataByName(Assets.Helm.jsonRef).isDefined &&
      assetCollection.findTextDataByName(Assets.Trees.jsonRef).isDefined &&
      assetCollection.findTextDataByName(Assets.Water.jsonRef).isDefined &&
      assetCollection.findTextDataByName(Assets.Flag.jsonRef).isDefined &&
      assetCollection.findTextDataByName(Assets.Static.terrainJsonRef).isDefined &&
      assetCollection.findTextDataByName(Assets.Itv.jsonRef).isDefined
    ) {

      val tileMapper: Int => TileType = {
        case 0 => TileType.Empty
        case _ => TileType.Solid
      }

      // Here we read the Tiled level description and manufacture a tuple of:
      // (a `TiledGridMap` of data, and a renderable verison of the map)
      val terrainData: Option[(TiledGridMap[TileType], Group)] =
        for {
          json         <- assetCollection.findTextDataByName(Assets.Static.terrainJsonRef)
          tileMap      <- Json.tiledMapFromJson(json)
          terrainGroup <- tileMap.toGroup(Assets.Static.terrainRef)
          grid         <- tileMap.toGrid(tileMapper)
        } yield (grid -> terrainGroup.withDepth(Depth(4)))

      for {
        helm        <- loader(Assets.Helm.jsonRef, Assets.Helm.ref, Depth(9))
        palm        <- loader(Assets.Trees.jsonRef, Assets.Trees.ref, Depth(1))
        reflections <- loader(Assets.Water.jsonRef, Assets.Water.ref, Depth(20))
        flag        <- loader(Assets.Flag.jsonRef, Assets.Flag.ref, Depth(10))
        terrain     <- terrainData.toRight("Failed to load terrain")
        itv         <- loader(Assets.Itv.jsonRef, Assets.Itv.ref, Depth(5))
      } yield Some(
        makeAdditionalAssets(
          screenDimensions,
          helm,
          palm,
          reflections,
          flag,
          terrain._1,
          terrain._2,
          itv
        )
      )
    } else Right(None)
  }

  // Helper function that loads Aseprite animations.
  def loadAnimation(
      assetCollection: AssetCollection,
      dice: Dice
  )(jsonRef: AssetName, name: AssetName, depth: Depth): Either[String, SpriteAndAnimations] = {
    val res = for {
      json                <- assetCollection.findTextDataByName(jsonRef)
      aseprite            <- Json.asepriteFromJson(json)
      spriteAndAnimations <- aseprite.toSpriteAndAnimations(dice, name)
    } yield spriteAndAnimations.copy(sprite = spriteAndAnimations.sprite.withDepth(depth))

    res match {
      case Some(spriteAndAnimations) =>
        Right(spriteAndAnimations)

      case None =>
        Left("Failed to load " + name)
    }
  }

  def makeAdditionalAssets(
      screenDimensions: Rectangle,
      helm: SpriteAndAnimations,
      palm: SpriteAndAnimations,
      waterReflections: SpriteAndAnimations,
      flag: SpriteAndAnimations,
      terrainMap: TiledGridMap[TileType],
      terrain: Group,
      itv: SpriteAndAnimations
  ): (LevelDataStore, List[Animation]) =
    (
      LevelDataStore(
        waterReflections.sprite
          .withRef(85, 0)
          .moveTo(screenDimensions.horizontalCenter, screenDimensions.verticalCenter + 5),
        flag.sprite.withRef(22, 105).moveTo(200, 288),
        itv.sprite
          .moveTo(MagicNumbers.tileSize.toInt * 17, MagicNumbers.tileSize.toInt * 4)
          .scaleBy(MagicNumbers.itvxScaleFactor, MagicNumbers.itvxScaleFactor),
        palm.sprite,
        terrainMap,
        terrain
      ),
      List(waterReflections.animations, flag.animations, helm.animations, palm.animations, itv.animations)
    )

  def makeStartupData(
      captain: SpriteAndAnimations,
      levelDataStore: Option[(LevelDataStore, List[Animation])]
  ): Startup.Success[StartupData] =
    Startup
      .Success(
        StartupData(
          captain.sprite
            .modifyMaterial(m => Material.ImageEffects(m.diffuse))
            .scaleBy(MagicNumbers.bouncyDaveScaleFactor, MagicNumbers.bouncyDaveScaleFactor),
          levelDataStore.map(_._1)
        )
      )
      .addAnimations(captain.animations)
      .addAnimations(levelDataStore.map(_._2).getOrElse(Nil))

}

final case class StartupData(
    captain: Sprite[Material.ImageEffects],
    levelDataStore: Option[LevelDataStore]
)
final case class LevelDataStore(
    waterReflections: Sprite[Material.Bitmap],
    flag: Sprite[Material.Bitmap],
    itv: Sprite[Material.Bitmap],
    palm: Sprite[Material.Bitmap],
    terrainMap: TiledGridMap[TileType],
    terrain: Group
) {
  val backTallPalm: Sprite[Material.Bitmap] =
    palm
      .withBindingKey(BindingKey("Back Tall Palm"))
      .withDepth(Depth(10))
}

enum TileType derives CanEqual:
  case Empty, Solid
