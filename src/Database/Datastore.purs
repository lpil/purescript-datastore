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
    {-- TODO --}
    {-- , insert --}
    , upsert
    {-- TODO --}
    {-- , update --}
    , get
    ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat as Aff
import Control.Monad.Eff (kind Effect)
import Data.Foreign (Foreign)
import Data.Foreign as Foreign
import Data.Array as Array
import Data.Function.Pipe ((|>))
import Data.Functor (map)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, Unit, (<>))


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


type RawResult =
  { id :: Foreign
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
  -> Aff.EffFnAff (datastore :: DATASTORE | eff) RawResult


foreign import _save
  :: forall eff
   . String
  -> Client
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
    |> map processRawResult


upsert
  :: forall eff
   . Client
  -> KeyPath
  -> Kind
  -> Id
  -> Foreign
  -> Aff (datastore :: DATASTORE | eff) Unit
upsert =
  save "upsert"


insert
  :: forall eff
   . Client
  -> KeyPath
  -> Kind
  -> Id
  -> Foreign
  -> Aff (datastore :: DATASTORE | eff) Unit
insert =
  save "insert"


update
  :: forall eff
   . Client
  -> KeyPath
  -> Kind
  -> Id
  -> Foreign
  -> Aff (datastore :: DATASTORE | eff) Unit
update =
  save "update"


save
  :: forall eff
   . String
  -> Client
  -> KeyPath
  -> Kind
  -> Id
  -> Foreign
  -> Aff (datastore :: DATASTORE | eff) Unit
save method client ancestors kind id data_ =
  let
    keyPair =
      [Foreign.toForeign kind, expandId id]

    key =
      ancestors
        |> flattenPath
        |> (_ <> keyPair)
  in
    data_
      |> _save method client key
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
      |> Array.concat


expandId :: Id -> Foreign
expandId id =
  case id of
    Name n ->
      Foreign.toForeign n

    Id i ->
      Foreign.toForeign i


processRawResult :: RawResult -> Maybe Result
processRawResult r =
  if Foreign.isUndefined r."data" then
    Nothing
  else
    Just { id: foreignToId r.id
         , kind: r.kind
         , data: r.data
         , path: r.path
         }


foreignToId :: Foreign -> Id
foreignToId f =
  case Foreign.typeOf f of
    "number" ->
      Id (Foreign.unsafeFromForeign f)

    _ ->
      Name (Foreign.unsafeFromForeign f)
