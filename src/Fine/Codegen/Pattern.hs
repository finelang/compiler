module Fine.Codegen.Pattern (extractCondsAndBinds) where

import Data.List.NonEmpty2 (toList)
import Data.Maybe (mapMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Fine.Codegen.Lit (genLitCode)
import Fine.Syntax.Abstract (Pattern (..))
import Fine.Syntax.Common (Id (Id), Lit (Str))

data PathEnd
  = Equals Lit
  | As Id

data PathPiece
  = PropTo Text
  | IndexTo Int

data PatternPath
  = End PathEnd
  | Continue PathPiece PatternPath

indexedPaths :: [Pattern] -> [PatternPath]
indexedPaths patts =
  concat $
    zipWith
      (\patt ix -> map (Continue $ IndexTo ix) (fromPattern patt))
      patts
      [(0 :: Int) ..]

fromPattern :: Pattern -> [PatternPath]
fromPattern (LiteralP lit _) = [End $ Equals lit]
fromPattern (DataP (Id name _) patts _) =
  let fromTag = Continue (PropTo "$tag") (End $ Equals $ Str name)
   in fromTag : indexedPaths patts
fromPattern (RecordP props _) =
  foldMap
    (\(Id name _, patt) -> map (Continue $ PropTo name) (fromPattern patt))
    props
fromPattern (TupleP patts _) = indexedPaths (toList patts)
fromPattern (Capture var) = [End $ As var]
fromPattern (DiscardP _) = []

fromPatternPath :: PatternPath -> ([PathPiece], PathEnd)
fromPatternPath pattPath = go pattPath []
  where
    go (End ct) pieces = (reverse pieces, ct)
    go (Continue piece pp) pieces = go pp (piece : pieces)

genPathPieceCode :: PathPiece -> Text
genPathPieceCode (PropTo name) = [i|.#{name}|]
genPathPieceCode (IndexTo ix) = [i|[#{ix}]|]

genPathCode :: Text -> [PathPiece] -> Text
genPathCode name pieces = T.concat (name : map genPathPieceCode pieces)

data CodeType t
  = Cond t
  | Bind t

justCond :: CodeType t -> Maybe t
justCond (Cond x) = Just x
justCond _ = Nothing

justBind :: CodeType t -> Maybe t
justBind (Bind x) = Just x
justBind _ = Nothing

genCode :: Text -> [PathPiece] -> PathEnd -> CodeType Text
genCode name pieces ct =
  let pathCode = genPathCode name pieces
   in case ct of
        Equals lit -> Cond [i|#{pathCode} === #{genLitCode lit}|]
        As var -> Bind [i|const #{var} = #{pathCode}|]

extractCondsAndBinds :: Text -> Pattern -> ([Text], [Text])
extractCondsAndBinds name patt =
  let paths = fromPattern patt
      pairs = map fromPatternPath paths
      condsAndBinds = map (uncurry $ genCode name) pairs
   in (mapMaybe justCond condsAndBinds, mapMaybe justBind condsAndBinds)
