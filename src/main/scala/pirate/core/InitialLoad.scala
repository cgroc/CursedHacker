package pirate.core

import indigo.*
import indigo.json.Json
import indigo.shared.formats.TiledGridMap
import pirate.core.Constants
import pirate.core.Constants.CharacterName
import pirate.scenes.level.model.LevelModel

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
        dave     <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.ref, Depth(2))
        dougie   <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.dougieRef, Depth(2))
        maya     <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.mayaRef, Depth(2))
        pere     <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.pereRef, Depth(2))
        shah     <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.shahRef, Depth(2))
        lee      <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.leeRef, Depth(2))
        dan      <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.danRef, Depth(2))
        miles    <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.milesRef, Depth(2))
        rob      <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.robRef, Depth(2))
        josh     <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.joshRef, Depth(2))
        gokce    <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.gokceRef, Depth(2))
        becky    <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.beckyRef, Depth(2))
        anne     <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.anneRef, Depth(2))
        luke     <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.lukeRef, Depth(2))
        ali      <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.aliRef, Depth(2))
        bella    <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.bellaRef, Depth(2))
        james    <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.jamesRef, Depth(2))
        tobias   <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.tobiasRef, Depth(2))
        chloe    <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.chloeRef, Depth(2))
        chris    <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.chrisRef, Depth(2))
        adam     <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.adamRef, Depth(2))
        waltons  <- loadAnimation(assetCollection, dice)(Assets.Captain.jsonRef, Assets.Captain.waltonsRef, Depth(2))
        maybeLds <- levelDataStore(screenDimensions, assetCollection, dice)
      } yield makeStartupData(
        {
          case Constants.CharacterName.Dave    => dave
          case Constants.CharacterName.Dougie  => dougie
          case Constants.CharacterName.Maya    => maya
          case Constants.CharacterName.Shah    => shah
          case Constants.CharacterName.Pere    => pere
          case Constants.CharacterName.Lee     => lee
          case Constants.CharacterName.Dan     => dan
          case Constants.CharacterName.Miles   => miles
          case Constants.CharacterName.Rob     => rob
          case Constants.CharacterName.Josh    => josh
          case Constants.CharacterName.Gokce   => gokce
          case Constants.CharacterName.Becky   => becky
          case Constants.CharacterName.Anne    => anne
          case Constants.CharacterName.Luke    => luke
          case Constants.CharacterName.Ali     => ali
          case Constants.CharacterName.Bella   => bella
          case Constants.CharacterName.James   => james
          case Constants.CharacterName.Tobias  => tobias
          case Constants.CharacterName.Chloe   => chloe
          case Constants.CharacterName.Chris   => chris
          case Constants.CharacterName.Adam    => adam
          case Constants.CharacterName.Waltons => waltons
        },
        maybeLds
      )) match {
        case Left(message) =>
          Startup.Failure(message)

        case Right(success) =>
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
      def terrainData(removeLeftDoor: Boolean): Option[(TiledGridMap[TileType], Group)] =
        for {
          json <- assetCollection.findTextDataByName(Assets.Static.terrainJsonRef)
          tileMap <- Json.tiledMapFromJson(json).map { tileMap =>
            if (removeLeftDoor)
              tileMap.copy(
                layers = tileMap.layers.map(layer =>
                  layer.copy(data = layer.data.zipWithIndex.collect { case (tile, idx) =>
                    if (idx == 80 || idx == 60) 20 else tile // get rid of the gap on the left hand side
                  })
                )
              )
            else tileMap
          }
          terrainGroup <- tileMap.toGroup(Assets.Static.terrainRef)
          grid         <- tileMap.toGrid(tileMapper)
        } yield grid -> terrainGroup.withDepth(Depth(4))

      for {
        helm              <- loader(Assets.Helm.jsonRef, Assets.Helm.ref, Depth(9))
        palm              <- loader(Assets.Trees.jsonRef, Assets.Trees.ref, Depth(1))
        reflections       <- loader(Assets.Water.jsonRef, Assets.Water.ref, Depth(20))
        flag              <- loader(Assets.Flag.jsonRef, Assets.Flag.ref, Depth(10))
        normalTerrain     <- terrainData(false).toRight("Failed to load terrain")
        terrainNoLeftDoor <- terrainData(true).toRight("Failed to load terrain")
        itv               <- loader(Assets.Itv.jsonRef, Assets.Itv.ref, Depth(5))
      } yield Some(
        makeAdditionalAssets(
          screenDimensions,
          helm,
          palm,
          reflections,
          flag,
          normalTerrain._1,
          normalTerrain._2,
          terrainNoLeftDoor._1,
          terrainNoLeftDoor._2,
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
      terrainMapNoLeftDoor: TiledGridMap[TileType],
      terrainNoLeftDoor: Group,
      itv: SpriteAndAnimations
  ): (LevelDataStore, List[Animation]) =
    (
      LevelDataStore(
        waterReflections.sprite
          .withRef(85, 0)
          .moveTo(screenDimensions.horizontalCenter, screenDimensions.verticalCenter + 5),
        flag.sprite.withRef(22, 105).moveTo(200, 256),
        itv.sprite
          .moveTo(Constants.MagicNumbers.tileSize.toInt * 17, Constants.MagicNumbers.tileSize.toInt * 4)
          .scaleBy(Constants.MagicNumbers.itvxScaleFactor, Constants.MagicNumbers.itvxScaleFactor),
        palm.sprite,
        terrainMap,
        terrain,
        terrainMapNoLeftDoor,
        terrainNoLeftDoor
      ),
      List(waterReflections.animations, flag.animations, helm.animations, palm.animations, itv.animations)
    )

  def makeStartupData(
      spritesByName: CharacterName => SpriteAndAnimations,
      levelDataStore: Option[(LevelDataStore, List[Animation])]
  ): Startup.Success[StartupData] =
    Startup
      .Success(
        StartupData(
          spritesByName = spritesByName andThen (
            _.sprite
              .modifyMaterial(m => Material.ImageEffects(m.diffuse))
              .scaleBy(
                Constants.MagicNumbers.bouncyDaveScaleFactor,
                Constants.MagicNumbers.bouncyDaveScaleFactor
              )
          ),
          levelDataStore = levelDataStore.map(_._1),
          screenData = {
            case LevelModel.Screen.Start => ScreenData(Material.Bitmap(Assets.Static.backgroundRef))
            case LevelModel.Screen.One   => ScreenData(Material.Bitmap(Assets.Static.background2Ref))
            case LevelModel.Screen.Two   => ScreenData(Material.Bitmap(Assets.Static.background3Ref))
            case LevelModel.Screen.Three => ScreenData(Material.Bitmap(Assets.Static.background4Ref))
            case LevelModel.Screen.Four  => ScreenData(Material.Bitmap(Assets.Static.background5Ref))
            case LevelModel.Screen.End   => ScreenData(Material.Bitmap(Assets.Static.finalBackgroundRef))
          }
        )
      )
      .addAnimations(CharacterName.values.map(spritesByName).toList.map(_.animations))
      .addAnimations(levelDataStore.map(_._2).getOrElse(Nil))

}

final case class StartupData(
    spritesByName: CharacterName => Sprite[Material.ImageEffects],
    levelDataStore: Option[LevelDataStore],
    screenData: LevelModel.Screen => ScreenData
)
final case class LevelDataStore(
    waterReflections: Sprite[Material.Bitmap],
    flag: Sprite[Material.Bitmap],
    itv: Sprite[Material.Bitmap],
    palm: Sprite[Material.Bitmap],
    terrainMap: TiledGridMap[TileType],
    terrain: Group,
    terrainMapNoLeftDoor: TiledGridMap[TileType],
    terrainNoLeftDoor: Group
) {
  val backTallPalm: Sprite[Material.Bitmap] =
    palm
      .withBindingKey(BindingKey("Back Tall Palm"))
      .withDepth(Depth(10))
}

enum TileType derives CanEqual:
  case Empty, Solid
