module Fine.Codegen.Pattern (extractCondsAndBinds) where

import Data.List.NonEmpty (toList)
import Data.Maybe (mapMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Fine.Codegen.Lit (genLitCode)
import Fine.Syntax.Abstract (Pattern (..), PropsPattern (PropsPattern))
import Fine.Syntax.Common (Lit (Str), Var (Var))

data PathEnd
  = Equals Lit
  | As Var
  | AsExcept Var [Var]

data PathPiece
  = PropTo Text
  | IndexTo Int

data PatternPath
  = End PathEnd
  | Continue PathPiece PatternPath

fromPropsPattern :: PropsPattern -> [PatternPath]
fromPropsPattern (PropsPattern named optSpread) =
  let fromNamed =
        concat $
          map
            (\(Var name _, patt) -> map (Continue $ PropTo name) (fromPattern patt))
            named
      fromSpread = case optSpread of
        Nothing -> []
        Just var -> [End $ AsExcept var (map fst named)]
   in fromSpread ++ fromNamed

fromPattern :: Pattern -> [PatternPath]
fromPattern (LiteralPatt lit _) = [End $ Equals lit]
fromPattern (ObjPatt props _) = fromPropsPattern props
fromPattern (VariantPatt (Var name _) props _) =
  let fromTag = Continue (PropTo "$tag") (End $ Equals $ Str name)
   in fromTag : fromPropsPattern props
fromPattern (TuplePatt patts _) =
  concat $
    zipWith
      (\patt ix -> map (Continue $ IndexTo ix) (fromPattern patt))
      (toList patts)
      [(0 :: Int) ..]
fromPattern (Capture var) = [End $ As var]

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
        (Equals lit) -> Cond [i|#{pathCode} === #{genLitCode lit}|]
        (As var) -> Bind [i|const #{var} = #{pathCode}|]
        (AsExcept var excluded) ->
          let excluded' = T.intercalate ", " (map (\prop -> [i|"#{prop}"|]) excluded)
           in Bind [i|const #{var} = fine$withoutProps(#{pathCode}, [#{excluded'}])|]

extractCondsAndBinds :: Text -> Pattern -> ([Text], [Text])
extractCondsAndBinds name patt =
  let paths = fromPattern patt
      pairs = map fromPatternPath paths
      condsAndBinds = map (uncurry $ genCode name) pairs
   in (mapMaybe justCond condsAndBinds, mapMaybe justBind condsAndBinds)
