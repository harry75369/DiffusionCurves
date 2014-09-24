import Text.Parsec hiding (spaces)
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (haskellDef)
import Data.Functor
import Diagrams.Prelude hiding (Color, Time, render)
import Diagrams.Backend.SVG

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

data VecFile = VecFile { version :: Double, scene :: Scene } deriving (Show)
data Scene = Scene { sceneObjects :: SceneObjects } deriving (Show)
data SceneObjects = SceneObjects { sceneObject :: SceneObject, backgroundColor :: Color, cells :: [Cell] } deriving (Show)
data SceneObject = SpacetimeVectorGraphicsComplex deriving (Show)
data Color = Color Double Double Double Double deriving (Show)
data Cell = InstantVertex { id :: Integer, color :: Color, time :: Time, pos :: Position, size :: Integer, tangentEdges :: () }
          | InstantEdge { id :: Integer, color :: Color, time :: Time, leftVertex :: Integer, rightVertex :: Integer, geometry :: Geometry }
          | InstantFace { id :: Integer, color :: Color, time :: Time, cycles :: [Cycle] } deriving (Show)
data Position = Position Double Double deriving (Show)
data Time = ExactFrame Integer deriving (Show)
data Geometry = LinearSpline { numVertices :: Integer, vertices :: [Vertex] } deriving (Show)
data Vertex = Vertex Double Double Double deriving (Show)
data Cycle = Cycle Integer [(Integer, Integer)] deriving (Show)

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

render r = do
  let bc = backgroundColor . sceneObjects . scene $ r
      cs = cells . sceneObjects . scene $ r
      vs = filter isVertex cs where
        isVertex (InstantVertex _ _ _ _ _ _) = True
        isVertex _ = False
      es = filter isEdge cs where
        isEdge (InstantEdge _ _ _ _ _ _) = True
        isEdge _ = False
      fs = filter isFace cs where
        isFace (InstantFace _ _ _ _) = True
        isFace _ = False
  {-print vs >> print es >> print fs-}
  case vertices . geometry <$> findEdge 0 es of
    Nothing -> return ()
    Just vs -> do
      let toP2 (Vertex x y w) = p2 (x,y)
      let s = reflectY . stroke . cubicSpline True $ map toP2 vs
      renderSVG "main.svg" (Width 400) s

-- Entrance

main = do
  c <- getContents
  case parse p_vecfile "error" c of
    Left e -> putStrLn "Error parsing input:" >> print e
    Right r -> render r

