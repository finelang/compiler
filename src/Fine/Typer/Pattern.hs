module Fine.Typer.Pattern () where

-- import Control.Monad (forM)
-- import Control.Monad.Trans.Reader (Reader, asks, local)
-- import Data.Semigroup (Max (Max, getMax))
-- import Data.String.Interpolate (i)
-- import Fine.Syntax (
--   Lit (Bool, Unit),
--   LitT (..),
--   Pattern (..),
--   Phase (Typed),
--   Range (NoRange),
--   Type (..),
--  )
-- import GHC.Err.Extra (errorTODO, errorUNREACHABLE)

-- type Depth = Int

-- maxDepth :: [Pattern] -> Depth
-- maxDepth = getMax . foldMap (Max . depth)

-- depth :: Pattern -> Depth
-- depth (LiteralP _ _) = 0
-- depth (DataP _ _ patts) = 1 + maxDepth patts
-- depth (RecordP _ props) = 1 + maxDepth (map snd props)
-- depth (TupleP _ fst' snd' rest) = 1 + maxDepth (fst' : snd' : rest)
-- depth (ListP _ patts) = 1 + maxDepth patts
-- depth (Capture _) = 0
-- depth (Discard _) = 0

-- data Ctx = Ctx
--   { maximumDepth :: Depth,
--     currentDepth :: Depth
--   }

-- deeper :: Reader Ctx a -> Reader Ctx a
-- deeper = local $ \ctx@Ctx{currentDepth} -> ctx{currentDepth = currentDepth + 1}

-- deepEnough :: Reader Ctx Bool
-- deepEnough = (==) <$> asks maximumDepth <*> asks currentDepth

-- arbitraryTestCase :: Pattern
-- arbitraryTestCase = Discard NoRange

-- litTestCases :: LitT -> [Pattern]
-- litTestCases IntT = [arbitraryTestCase]
-- litTestCases FloatT = [arbitraryTestCase]
-- litTestCases BoolT = [LiteralP NoRange (Bool True), LiteralP NoRange (Bool False)]
-- litTestCases StrT = [arbitraryTestCase]
-- litTestCases UnitT = [LiteralP NoRange Unit]

-- testCases :: Type Typed -> Reader Ctx [Pattern]
-- testCases matchedType = do
--   de <- deepEnough
--   if de
--     then pure [arbitraryTestCase]
--     else deeper $ case matchedType of
--       LiteralT _ litType -> pure $ litTestCases litType
--       VoidT _ -> pure []
--       TupleT _ fst' snd' rest -> do
--         fstTestCases <- testCases fst'
--         sndTestCases <- testCases snd'
--         restTestCases <- combinations <$> mapM testCases rest
--         pure $ TupleP NoRange <$> fstTestCases <*> sndTestCases <*> restTestCases
--       ListT _ _ ->
--         -- since lists can have any size, the user shoud always have
--         -- a case to match any list
--         pure [arbitraryTestCase]
--       RecordT _ propTypes -> do
--         propTestCases <- fmap combinations $ forM propTypes $ \(prop, type') -> do
--           cases <- testCases type'
--           pure $ map ((,) prop) cases
--         pure $ RecordP NoRange <$> propTestCases
--       FunT _ _ _ ->
--         -- the only way to match a function is by discarding it
--         -- or capturing it in a var, no case is useful
--         pure [arbitraryTestCase]
--       Forall _ _ type' -> testCases type'
--       DataT _ _ _ -> errorTODO
--       _ ->
--         errorUNREACHABLE
--           [i|The matched type '#{matchedType}' should be a fully evaluated type of an expression.|]

-- combinations :: [[a]] -> [[a]]
-- combinations = go [[]]
--  where
--   go yss [] = map reverse yss
--   go yss (xs : xss) = go ((:) <$> xs <*> yss) xss
