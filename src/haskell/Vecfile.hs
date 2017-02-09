import Text.Parsec hiding (spaces)
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (haskellDef)
import Data.Functor
import Diagrams.Prelude hiding (Color, Time, render)
import Diagrams.Backend.SVG
import Data.Colour.SRGB

-- Parser elements

lexer    = T.makeTokenParser haskellDef
lexeme   = T.lexeme lexer
spaces   = T.whiteSpace lexer
lexstr   = lexeme . string

identifier = T.identifier lexer
integer    = T.integer lexer
float      = T.float lexer
colon      = T.colon lexer
comma      = T.comma lexer

floating = do
  t <- optionMaybe (try float)
  case t of
    Just t  -> return t
    Nothing -> do
      i <- integer
      return $ (fromInteger i :: Double)

parens   = T.parens lexer   -- ()
braces   = T.braces lexer   -- {}
brackets = T.brackets lexer -- <>
squares  = T.squares lexer  -- []

-- Data structures

data VecFile = VecFile { m_version :: Double, m_scene :: Scene } deriving (Show)
data Scene = Scene { m_sceneObjects :: SceneObjects } deriving (Show)
data SceneObjects = SceneObjects { m_sceneObject :: SceneObject, m_backgroundColor :: Color, m_cells :: [Cell] } deriving (Show)
data SceneObject = SpacetimeVectorGraphicsComplex deriving (Show)
data Color = Color Double Double Double Double deriving (Show)
data Cell = InstantVertex { m_id :: Integer, m_color :: Color, m_time :: Time, m_pos :: Position, m_size :: Integer, m_tangentEdges :: () }
          | InstantEdge { m_id :: Integer, m_color :: Color, m_time :: Time, m_leftVertex :: Integer, m_rightVertex :: Integer, m_geometry :: Geometry }
          | InstantFace { m_id :: Integer, m_color :: Color, m_time :: Time, m_cycles :: [Cycle] } deriving (Show)
data Position = Position Double Double deriving (Show)
data Time = ExactFrame Integer deriving (Show)
data Geometry = LinearSpline { m_numVertices :: Integer, m_vertices :: [Vertex] } deriving (Show)
data Vertex = Vertex Double Double Double deriving (Show)
data Cycle = Cycle Integer [(Integer, Integer)]
           | Steiner Integer deriving (Show)

-- Parser definitions

p_vecfile = do
  spaces
  version <- p_version
  scene <- p_scene
  return $ VecFile version scene

p_version = lexstr "Version" >> colon >> floating

p_scene = do
  sceneobjects <- lexstr "Scene" >> colon >> (braces p_sceneobjects)
  return $ Scene sceneobjects

p_sceneobjects = do
  lexstr "SceneObjects" >> colon
  squares . braces $ do
    sceneobject <- p_sceneobject
    color <- p_backgroundcolor
    cells <- p_cells
    return $ SceneObjects sceneobject color cells

p_sceneobject = lexstr "SceneObject" >> colon >> lexstr "SpacetimeVectorGraphicsComplex" >> return SpacetimeVectorGraphicsComplex

p_backgroundcolor = lexstr "BackgroundColor" >> colon >> p_color

p_color = do
  r <- floating
  g <- floating
  b <- floating
  a <- floating
  return $ Color r g b a

p_cells = do
  lexstr "Cells" >> colon
  squares . many1 . braces $ do
    lexstr "Type" >> colon
    t <- identifier
    case t of
      "InstantVertex" -> do
        id    <- lexstr "ID" >> colon >> integer
        color <- lexstr "Color" >> colon >> p_color
        time  <- p_time
        pos   <- p_position
        size  <- lexstr "Size" >> colon >> integer
        lexstr "TangentEdges" >> colon >> squares spaces
        return $ InstantVertex id color time pos size ()
      "InstantEdge" -> do
        id    <- lexstr "ID" >> colon >> integer
        color <- lexstr "Color" >> colon >> p_color
        time  <- p_time
        left  <- lexstr "LeftVertex" >> colon >> integer
        right <- lexstr "RightVertex" >> colon >> integer
        geom  <- p_geometry
        return $ InstantEdge id color time left right geom
      "InstantFace" -> do
        id     <- lexstr "ID" >> colon >> integer
        color  <- lexstr "Color" >> colon >> p_color
        time   <- p_time
        cycles <- p_cycles
        return $ InstantFace id color time cycles

p_time = do
  t <- lexstr "Time" >> colon >> lexstr "ExactFrame" >> integer
  return $ ExactFrame t

p_position = do
  lexstr "Pos" >> colon
  parens p_tuple where
    p_tuple = do
      x <- floating
      y <- comma >> floating
      return $ Position x y

p_geometry = do
  lexstr "Geometry" >> colon
  braces $ do
    lexstr "Type" >> colon >> lexstr "LinearSpline"
    n <- lexstr "NumVertices" >> colon >> integer
    lexstr "Vertices" >> colon
    vs <- (squares . many1 . parens $ do
      x <- floating
      y <- comma >> floating
      w <- comma >> floating
      return $ Vertex x y w)
    return $ LinearSpline n vs

p_cycles = do
  lexstr "Cycles" >> colon
  squares $ sepBy1 p_cycle comma where
    p_cycle = do
      dir <- integer
      halfedges <- p_halfedges
      return $ Cycle dir halfedges

p_halfedges = do
  squares $ sepBy1 (parens p_tuple) comma where
    p_tuple = do
      id <- integer
      dir <- comma >> integer
      return (id, dir)

-- Algorithms

isVertex (InstantVertex _ _ _ _ _ _) = True
isVertex _ = False

isEdge (InstantEdge _ _ _ _ _ _) = True
isEdge _ = False

isFace (InstantFace _ _ _ _) = True
isFace _ = False

findVertex id [] = Nothing
findVertex id (x:xs) = if match id x then Just x else findVertex id xs where
  match id (InstantVertex tid _ _ _ _ _)
    | id == tid = True
    | otherwise = False
  match id _ = False

findEdge id [] = Nothing
findEdge id (x:xs) = if match id x then Just x else findEdge id xs where
  match id (InstantEdge tid _ _ _ _ _)
    | id == tid = True
    | otherwise = False
  match id _ = False

findFace id [] = Nothing
findFace id (x:xs) = if match id x then Just x else findFace id xs where
  match id (InstantFace tid _ _ _)
    | id == tid = True
    | otherwise = False
  match id _ = False

toColour (Color r g b a) = sRGB r g b
vertexToP2 (Vertex x y w) = p2 (x,y)
positionToP2 (Position x y) = p2 (x,y)
toWeight (Vertex x y w) = w
{-
extractFromHalfedges :: [Cell] -> [(Integer,Integer)] -> [[P2]]
extractFromHalfedges edges [] = []
extractFromHalfedges edges ((edge_id,edge_dir):xs) =
  case (findEdge edge_id edges) of
    Nothing -> (extractFromHalfedges edges xs)
    Just edge -> let dirf = if edge_dir == 1 then id else reverse
                     vs = dirf . m_vertices . m_geometry $ edge
                  in (map toP2 vs) : (extractFromHalfedges edges xs)

drawFace :: [Cell] -> [Cell] -> Cell -> Diagram B R2
drawFace edges vertices face =
  let bc = m_color $ face
      cs = m_cycles $ face
      mkVss [] = []
      mkVss ((Cycle cycle_dir halfedges):xs) = (concat . extractFromHalfedges edges $ halfedges) : (mkVss xs)
      vss = mkVss cs
   in foldl mappend mempty $ (map (reflectY . stroke . cubicSpline True) vss) # fc (toColour bc) # lw none

drawEdge :: [Cell] -> Cell -> Diagram B R2
drawEdge vertices edge =
  let vs = m_vertices . m_geometry $ edge
   in (reflectY . stroke . cubicSpline False $ map toP2 vs) # lwO 50

drawVertex :: Cell -> Diagram B R2
drawVertex vertex =
  let bc = m_color $ vertex
      sz = m_size $ vertex
   in circle (fromIntegral sz)

render r = do
  let bc = m_backgroundColor . m_sceneObjects . m_scene $ r
      cs = m_cells . m_sceneObjects . m_scene $ r
      vs = filter isVertex cs
      es = filter isEdge cs
      fs = filter isFace cs
  {-draw faces first-}
  putStrLn "Faces:" >> print fs
  let r1 = foldl mappend mempty $ map (drawFace es vs) fs
  {-draw edges second-}
  putStrLn "Edges:" >> print es
  let r2 = foldl mappend r1 $ map (drawEdge vs) es
  {-draw vertices third-}
  putStrLn "Vertices:" >> print vs
  let r3 = foldl mappend r2 $ map drawVertex vs
  {-draw background last-}
  putStrLn "Background:" >> print bc
  let result = r3 # bg (toColour bc)
  {-write output-}
  renderRasterific "main.png" (Width 200) 100 (r1 # (pad 1.5 . center))
-}
render2 r = do
  let bc = m_backgroundColor . m_sceneObjects . m_scene $ r
      cs = m_cells . m_sceneObjects . m_scene $ r
      result = (foldl mappend mempty (map (drawCell cs) cs)) # center # pad 1.2 # reflectY # bg (toColour bc)
   in renderSVG "main.svg" (Width 400) result

drawCell :: [Cell] -> Cell -> Diagram B R2
drawCell cells (InstantVertex vid color time pos size tangentEdges) =
  (circle ((fromIntegral size)/2) `at` (positionToP2 pos)) # strokeLocTrail # fc (toColour color) # lw none
drawCell cells (InstantEdge eid color time leftVertex rightVertex geometry) =
  let LinearSpline numVertices vertices = geometry
      positions = map vertexToP2 vertices
      weights = map toWeight vertices
      average_weight = sum weights / (fromIntegral $ length weights) {- TODO: weight-varying edge -}
   in (fromVertices positions # closeLine) # strokeLoop # lwG average_weight # lc (toColour color) # fc yellow
drawCell cells (InstantFace fid color time cycles) =
  let fromHalfedges [] = []
      fromHalfedges ((edge_id,edge_dir):xs) =
        case (findEdge edge_id cells) of
          Nothing -> fromHalfedges xs
          Just (InstantEdge _ _ _ l r g) -> let dirf = (if edge_dir == 1 then id else reverse)
                                                vertices = dirf . m_vertices $ g
                                                closed = (l == r)
                                             in (closed, map vertexToP2 vertices) : (fromHalfedges xs)
      fromCycles [] = []
      fromCycles ((Cycle cycle_dir halfedges):xs) =
        let vertices = concat $ map snd (fromHalfedges halfedges) {- TODO: proper edge concating -}
         in (fromVertices vertices # closeLine # strokeLoop) : (fromCycles xs)
   in (foldl mappend mempty $ fromCycles cycles) # fc (toColour color) # fillRule EvenOdd # lw none

-- Entrance

main = do
  c <- getContents
  case parse p_vecfile "error" c of
    Left e -> putStrLn "Error parsing input:" >> print e
    Right r -> render2 r

