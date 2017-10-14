module Database.Datastore
  ( DATASTORE
  , AuthCreds
  , Client
  , makeClient
  , delete
  , save
  , get
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat as Aff
import Control.Monad.Eff (kind Effect)
import Data.Foreign (Foreign, isUndefined)
import Data.Function.Pipe ((|>))
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Prelude (Unit)


data Client =
  Client


type Result =
  { name :: String
  , kind :: String
  , data :: Foreign
  , path :: Array String
  }


type AuthCreds =
  { projectId :: String
  }


foreign import data DATASTORE :: Effect


foreign import makeClient :: AuthCreds -> Client


foreign import _get
  :: forall eff
  . Client
  -> Array String
  -> Aff.EffFnAff (datastore :: DATASTORE | eff) Result


foreign import _save
  :: forall eff
  . Client
  -> Array String
  -> Foreign
  -> Aff.EffFnAff (datastore :: DATASTORE | eff) Unit


foreign import _delete
  :: forall eff
  . Client
  -> Array String
  -> Aff.EffFnAff (datastore :: DATASTORE | eff) Unit


get
  :: forall eff
  . Client
  -> Array String
  -> Aff (datastore :: DATASTORE | eff) (Maybe Result)
get client key =
  _get client key
    |> Aff.fromEffFnAff
    |> map handleNotFound


handleNotFound :: Result -> Maybe Result
handleNotFound r =
  if r."data" |> isUndefined then
    Nothing
  else
    Just r


save
  :: forall eff
  . Client
  -> Array String
  -> Foreign
  -> Aff (datastore :: DATASTORE | eff) Unit
save client key value =
  value
    |> _save client key
    |> Aff.fromEffFnAff


delete
  :: forall eff
  . Client
  -> Array String
  -> Aff (datastore :: DATASTORE | eff) Unit
delete client key =
  key
    |> _delete client
    |> Aff.fromEffFnAff
