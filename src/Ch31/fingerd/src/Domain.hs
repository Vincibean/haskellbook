module Domain where

import           Data.Text                      ( Text )
import           Database.SQLite.Simple         ( field
                                                , FromRow(..)
                                                , ToRow(..)
                                                )
import           Database.SQLite.Simple.Types   ( Null )

data NewUser = NewUser
  { newUsername      :: Text
  , newShell         :: Text
  , newHomeDirectory :: Text
  , newRealName      :: Text
  , newphone         :: Text
  }
  deriving (Eq, Show)

data User = User
  { userId        :: Integer
  , username      :: Text
  , shell         :: Text
  , homeDirectory :: Text
  , realName      :: Text
  , phone         :: Text
  }
  deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)
