import qualified Text.Parsec as P
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (haskellDef)

lexer    = T.makeTokenParser haskellDef
lexeme   = T.lexeme lexer
spaces   = T.whiteSpace lexer
string   = lexeme . P.string
many1    = P.many1
sepBy1   = P.sepBy1

identifier = T.identifier lexer
integer    = T.integer lexer
float      = T.float lexer
colon      = T.colon lexer
comma      = T.comma lexer

floating = do
  t <- P.optionMaybe (P.try float)
  case t of
    Just t  -> return t
    Nothing -> do
      i <- integer
      return $ (fromInteger i :: Double)

parens   = T.parens lexer   -- ()
braces   = T.braces lexer   -- {}
brackets = T.brackets lexer -- <>
squares  = T.squares lexer  -- []


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

p_vecfile = do
  spaces
  v <- p_version
  s <- p_scene
  return VecFile { version = v, scene = s }

p_version = string "Version" >> colon >> floating

p_scene = do
  s <- string "Scene" >> colon >> (braces p_sceneobjects)
  return Scene { sceneObjects = s }

p_sceneobjects = do
  string "SceneObjects" >> colon
  squares . braces $ do
    s <- p_sceneobject
    c <- p_backgroundcolor
    cs <- p_cells
    return SceneObjects { sceneObject = s, backgroundColor = c, cells = cs }

p_sceneobject = string "SceneObject" >> colon >> string "SpacetimeVectorGraphicsComplex" >> return SpacetimeVectorGraphicsComplex

p_backgroundcolor = string "BackgroundColor" >> colon >> p_color

p_color = do
  r <- floating
  g <- floating
  b <- floating
  a <- floating
  return $ Color r g b a

p_cells = do
  string "Cells" >> colon
  squares . many1 . braces $ do
    string "Type" >> colon
    t <- identifier
    case t of
      "InstantVertex" -> do
        string "ID" >> colon
        id <- integer
        string "Color" >> colon
        color <- p_color
        time <- p_time
        string "Pos" >> colon
        pos <- p_position
        string "Size" >> colon
        size <- integer
        string "TangentEdges" >> colon
        squares spaces
        return $ InstantVertex id color time pos size ()
      "InstantEdge" -> do
        string "ID" >> colon
        id <- integer
        string "Color" >> colon
        color <- p_color
        time <- p_time
        string "LeftVertex" >> colon
        left <- integer
        string "RightVertex" >> colon
        right <- integer
        geom <- p_geometry
        return $ InstantEdge id color time left right geom
      "InstantFace" -> do
        string "ID" >> colon
        id <- integer
        string "Color" >> colon
        color <- p_color
        time <- p_time
        cycles <- p_cycles
        return $ InstantFace id color time cycles

p_time = do
  t <- string "Time" >> colon >> string "ExactFrame" >> integer
  return $ ExactFrame t

p_position = do
  parens p_tuple where
    p_tuple = do
      x <- floating
      y <- comma >> floating
      return $ Position x y

p_geometry = do
  string "Geometry" >> colon
  braces $ do
    string "Type" >> colon >> string "LinearSpline"
    string "NumVertices" >> colon
    n <- integer
    string "Vertices" >> colon
    vs <- (squares . many1 . parens $ do
      x <- floating
      y <- comma >> floating
      w <- comma >> floating
      return $ Vertex x y w)
    return $ LinearSpline n vs

p_cycles = do
  string "Cycles" >> colon
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

text = "\
\---------- Vec File ----------\n\
\Version : 1.0\n\
\Scene : \n\
\{\n\
\    SceneObjects : \n\
\    [\n\
\        {\n\
\            SceneObject : SpacetimeVectorGraphicsComplex\n\
\            BackgroundColor : 1 1 1 1\n\
\            Cells : \n\
\            [\n\
\                {\n\
\                    Type : InstantVertex\n\
\                    ID : 0\n\
\                    Color : 0 0 1 1\n\
\                    Time : ExactFrame 0\n\
\                    Pos : (143,89)\n\
\                    Size : 17\n\
\                    TangentEdges :  [ ]\n\
\                }\n\
\                {\n\
\                    Type : InstantEdge\n\
\                    ID : 28\n\
\                    Color : 0 0 0 1\n\
\                    Time : ExactFrame 0\n\
\                    LeftVertex : 25\n\
\                    RightVertex : 27\n\
\                    Geometry : \n\
\                    {\n\
\                        Type : LinearSpline\n\
\                        NumVertices : 53\n\
\                        Vertices : [ (430,399,10) (430.074,402.018,10) (430.131,405.037,10) (430.16,408.057,10) (430.166,411.076,10) (430.155,414.095,10) (430.123,417.115,10) (430.08,420.134,10) (430.025,423.152,10) (429.964,426.171,10) (429.899,429.19,10) (429.836,432.209,10) (429.78,435.228,10) (429.736,438.247,10) (429.707,441.266,10) (429.698,444.285,10) (429.713,447.305,10) (429.755,450.324,10) (429.825,453.342,10) (429.923,456.36,10) (430.054,459.377,10) (430.22,462.391,10) (430.423,465.404,10) (430.663,468.414,10) (430.939,471.42,10) (431.251,474.423,10) (431.597,477.423,10) (431.978,480.418,10) (432.39,483.409,10) (432.832,486.395,10) (433.297,489.379,10) (433.782,492.358,10) (434.28,495.336,10) (434.787,498.312,10) (435.298,501.288,10) (435.808,504.264,10) (436.313,507.24,10) (436.811,510.218,10) (437.297,513.198,10) (437.77,516.18,10) (438.223,519.165,10) (438.654,522.153,10) (439.056,525.145,10) (439.426,528.142,10) (439.759,531.143,10) (440.054,534.147,10) (440.307,537.156,10) (440.517,540.168,10) (440.684,543.183,10) (440.809,546.2,10) (440.9,549.218,10) (440.967,552.236,10) (441,554,10) ]\n\
\                    }\n\
\                }\n\
\                {\n\
\                    Type : InstantFace\n\
\                    ID : 36\n\
\                    Color : 0 0.666667 0 1\n\
\                    Time : ExactFrame 0\n\
\                    Cycles : [ -1 [ (13,0) , (12,1) ] ]\n\
\                }\n\
\            ]\n\
\        }\n\
\    ]\n\
\}\n\
\"
