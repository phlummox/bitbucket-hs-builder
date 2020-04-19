
module Network.Bitbucket.Types
  where

import Data.Text as T

type UserId = Text
type Password = Text
data Auth = Auth UserId Password

