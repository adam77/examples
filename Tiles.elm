--
-- Tiles.elm
--
-- Functions to support rendering tiled maps using quad-tree decomposition.
--
-- https://wiki.openstreetmap.org/wiki/QuadTiles
--


module GeoAPI.Tiles exposing (divide, metresToPixels, latlng2tile, parentOf, rootTile, tileBounds, tile2latlng, tilesForBounds, tilesForView)

import GeoAPI.Types exposing (Tile, LatLng, ZoomLevel, Bounds)
import List.Extra exposing (andThen)


--
-- The root tile covers the whole globe.
--


rootTile : Tile
rootTile =
    Tile 0 0 0



--
-- Find longitude of a given tile.
--


tile2lng : Int -> Int -> Float
tile2lng xIndex zIndex =
    let
        x =
            toFloat xIndex

        z =
            toFloat zIndex
    in
        (x / (2 ^ z)) * 360 - 180



--
-- Find latitude of a given tile.
--


tile2lat : Int -> Int -> Float
tile2lat yIndex zIndex =
    let
        y =
            toFloat yIndex

        z =
            toFloat zIndex

        n =
            pi - 2 * pi * y / (2 ^ z)
    in
        180 / pi * atan (0.5 * ((e ^ n) - (e ^ -n)))



--
-- Find tile x coordinate for longitude and zoom level.
--


lng2tileX : Float -> ZoomLevel -> Int
lng2tileX lng zoom =
    let
        z =
            toFloat zoom
    in
        floor <| (lng + 180) / 360 * (2 ^ z)



--
-- Find tile y coordinate for latitude and zoom level.
--


lat2tileY : Float -> ZoomLevel -> Int
lat2tileY lat zoom =
    let
        z =
            toFloat zoom

        n =
            (lat * pi / 180)

        y =
            floor <| (1 - (logBase e (tan (n) + 1 / cos (n))) / pi) / 2 * (2 ^ z)
    in
        max 0 y



--
-- Find tile for position and zoom level.
--


latlng2tile : LatLng -> ZoomLevel -> Tile
latlng2tile { lat, lng } zoom =
    Tile (lng2tileX lng zoom) (lat2tileY lat zoom) zoom



--
-- Find position for tile (top-left).
--


tile2latlng : Tile -> LatLng
tile2latlng { x, y, z } =
    LatLng (tile2lat y z) (tile2lng x z)



--
-- Find bounds for a tile.
--


tileBounds : Tile -> Bounds
tileBounds { x, y, z } =
    let
        topLeft =
            tile2latlng <| Tile x y z

        bottomRight =
            tile2latlng <|
                Tile (x + 1) (y + 1) z
    in
        Bounds topLeft bottomRight



--
-- Find all tiles intersecting an area at a given zoom level.
--


tilesForBounds : Bounds -> ZoomLevel -> List Tile
tilesForBounds { topLeft, bottomRight } zoom =
    let
        { x, y } =
            latlng2tile topLeft zoom

        bottomRightTile =
            latlng2tile bottomRight zoom

        x2 =
            bottomRightTile.x

        y2 =
            bottomRightTile.y
    in
        if zoom == 0 then
            [ rootTile ]
        else
            List.range y y2
                |> andThen
                    (\y ->
                        List.range x x2
                            |> andThen (\x -> [ ( x, y ) ])
                    )
                |> List.map (\( x, y ) -> Tile x y zoom)



--
-- The tile covering this tile at the next zoom level up.
--


parentOf : Tile -> Tile
parentOf tile =
    { tile | x = tile.x // 2, y = tile.y // 2, z = zoomOut tile.z }



--
-- Quad divide a tile.
--


divide : Tile -> List Tile
divide tile =
    let
        ( x, y, z ) =
            ( tile.x * 2, tile.y * 2, tile.z + 1 )
    in
        [ Tile x y z
        , Tile (x + 1) y z
        , Tile x (y + 1) z
        , Tile (x + 1) (y + 1) z
        ]



--
-- Test if two bounds intersect.
--


intersects : Bounds -> Bounds -> Bool
intersects bounds bounds2 =
    let
        latIntersects =
            (bounds2.topLeft.lat >= bounds.bottomRight.lat) && (bounds2.bottomRight.lat <= bounds.topLeft.lat)

        lngIntersects =
            (bounds2.bottomRight.lng >= bounds.topLeft.lng) && (bounds2.topLeft.lng <= bounds.bottomRight.lng)
    in
        latIntersects && lngIntersects



--
-- Next zoom level 'up' (min 0).
--


zoomOut : ZoomLevel -> ZoomLevel
zoomOut current =
    if current == 0 then
        0
    else
        current - 1



--
-- Find all tiles covering an area.
-- Recursively quad divide any tiles that fail.
--


tilesForView : Bounds -> ZoomLevel -> (Tile -> Bool) -> List Tile
tilesForView view zoom failedToLoad =
    let
        -- start with tiles that cover the view and are 'relatively' large
        tiles =
            tilesForBounds view <| (zoomOut << zoomOut << zoomOut) zoom

        -- don't subdivide tiles smaller than this
        maxTileZ =
            zoom + 1

        successfullyLoaded tile =
            not (failedToLoad tile)

        stopDividing tile =
            (tile.z >= maxTileZ) || (successfullyLoaded tile)

        quadDivide tile =
            if stopDividing tile then
                [ tile ]
            else
                let
                    dividedTiles =
                        List.filter (tileBounds >> intersects view) (divide tile)
                in
                    List.concatMap quadDivide dividedTiles
    in
        List.concatMap quadDivide tiles



--
-- Convert metres to pixels at a given zoom level.
--


metresPerPixel : ZoomLevel -> LatLng -> Float
metresPerPixel zoomLevel viewportCentre =
    -- Implementing https://wiki.openstreetmap.org/wiki/Zoom_levels#Metres_per_pixel_math
    let
        circumferenceOfEarth =
            40075016.686

        latitudeInRadians =
            viewportCentre.lat * (pi / 180)
    in
        circumferenceOfEarth
            * (abs latitudeInRadians)
            / (2 ^ toFloat (zoomLevel + 8))


metresToPixels : ZoomLevel -> Float -> LatLng -> Float
metresToPixels zoomLevel metres viewportCentre =
    metres / (metresPerPixel zoomLevel viewportCentre)
