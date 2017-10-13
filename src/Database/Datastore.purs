module Database.Datastore
  ( DATASTORE
  , AuthCreds
  , Client
  , Key
  , makeClient
  , makeKindKey
  , makeKey
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


data Key =
  Key


type AuthCreds =
  { projectId :: String
  }


foreign import data DATASTORE :: Effect


foreign import makeClient :: AuthCreds -> Client


foreign import makeKindKey
  :: Client
  -> String
  -> Key


foreign import makeKey
  :: Client
  -> String
  -> String
  -> Key


foreign import _get
  :: forall eff
  . Client
  -> Key
  -> Aff.EffFnAff (datastore :: DATASTORE | eff) Foreign


foreign import _save
  :: forall eff
  . Client
  -> Key
  -> Foreign
  -> Aff.EffFnAff (datastore :: DATASTORE | eff) Unit



get
  :: forall eff
  . Client
  -> Key
  -> Aff (datastore :: DATASTORE | eff) (Maybe Foreign)
get client key =
  _get client key
    |> Aff.fromEffFnAff
    |> map handleUndefined


save
  :: forall eff
  . Client
  -> Key
  -> Foreign
  -> Aff (datastore :: DATASTORE | eff) Unit
save client key value =
  value
    |> _save client key
    |> Aff.fromEffFnAff


handleUndefined :: Foreign -> Maybe Foreign
handleUndefined e =
  if isUndefined e then
    Nothing
  else
    Just e

{-
var key = datastoreClient.key(['Product', 'Computer']);

datastoreClient.get(key, function(err, entity) {
  console.log(err || entity);
});

// Save data to Datastore.
var blogPostData = {
  title: 'How to make the perfect homemade pasta',
  author: 'Andrew Chilton',
  isDraft: true
};

var blogPostKey = datastoreClient.key('BlogPost');

datastoreClient.save({
  key: blogPostKey,
  data: blogPostData
}, function(err) {
  // `blogPostKey` has been updated with an ID so you can do more operations
  // with it, such as an update.
  blogPostData.isDraft = false;

  datastoreClient.save({
    key: blogPostKey,
    data: blogPostData
  }, function(err) {
    if (!err) {
      // The blog post is now published!
    }
  });
});
-}
