module Database.Datastore
  ( DATASTORE
  , AuthCreds
  , Client
  , Id(..)
  , Kind(..)
  , KeyPair(..)
  , KeyPath(..)
  , makeClient
  , delete
  , save'
  , get
  ) where

import Prelude (class Eq, class Show, Unit, (<>))

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat as Aff
import Control.Monad.Eff (kind Effect)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foldable as Foldable
import Data.Foreign (Foreign)
import Data.Foreign as Foreign
import Data.Function.Pipe ((|>))
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))


newtype Kind =
  Kind String

derive newtype instance entityNameShow :: Show Kind
derive newtype instance entityNameEq :: Eq Kind


data Id
  = Id Int
  | Name String

derive instance genericId :: Generic Id _
derive instance eqId :: Eq Id
instance showId :: Show Id where show = genericShow


data Client
  = Client


type KeyPair =
  Tuple Kind Id


type KeyPath =
  Array KeyPair


type Result =
  { id :: Id
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
  -> Array Foreign
  -> Aff.EffFnAff (datastore :: DATASTORE | eff) Result


foreign import _save
  :: forall eff
  . Client
  -> Array Foreign
  -> Foreign
  -> Aff.EffFnAff (datastore :: DATASTORE | eff) Unit


foreign import _delete
  :: forall eff
  . Client
  -> Array Foreign
  -> Aff.EffFnAff (datastore :: DATASTORE | eff) Unit


get
  :: forall eff
  . Client
  -> KeyPath
  -> Aff (datastore :: DATASTORE | eff) (Maybe Result)
get client key =
  key
    |> flattenPath
    |> _get client
    |> Aff.fromEffFnAff
    |> map handleNotFound


save'
  :: forall eff
  . Client
  -> KeyPath
  -> Kind
  -> Id
  -> Foreign
  -> Aff (datastore :: DATASTORE | eff) Unit
save' client ancestors kind id data_ =
  let
    keyPair =
      [Foreign.toForeign kind, expandId id]

    key =
      ancestors
        |> flattenPath
        |> (_ <> keyPair)
  in
    data_
      |> _save client key
      |> Aff.fromEffFnAff


delete
  :: forall eff
  . Client
  -> KeyPath
  -> Aff (datastore :: DATASTORE | eff) Unit
delete client key =
  key
  |> flattenPath
  |> _delete client
  |> Aff.fromEffFnAff


flattenPath :: KeyPath -> Array Foreign
flattenPath path =
  let
    expand (Tuple kind id) =
       [Foreign.toForeign kind, expandId id]
  in
    path
      |> map expand
      |> Foldable.fold


expandId :: Id -> Foreign
expandId id =
  case id of
    Name n ->
      Foreign.toForeign n

    Id i ->
      Foreign.toForeign i


handleNotFound :: Result -> Maybe Result
handleNotFound r =
  if r."data" |> Foreign.isUndefined then
    Nothing
  else
    Just r
