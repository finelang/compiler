module GHC.Err.Extra where

import GHC.Stack (HasCallStack)

errorTODO :: (HasCallStack) => a
errorTODO = error "Not Implemented"

errorUNREACHABLE :: (HasCallStack) => String -> a
errorUNREACHABLE message = error $ "This section of code should be unreachable. " ++ message
