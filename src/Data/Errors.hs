module Data.Errors where

data Errors e w = Errors [e] [w]

instance Semigroup (Errors e w) where
  (<>) :: Errors e w -> Errors e w -> Errors e w
  Errors errs wrns <> Errors errs' wrns' = Errors (errs ++ errs') (wrns ++ wrns')

instance Monoid (Errors e w) where
  mempty :: Errors e w
  mempty = Errors [] []

error' :: e -> Errors e w
error' err = Errors [err] []

warning :: w -> Errors e w
warning wrn = Errors [] [wrn]
