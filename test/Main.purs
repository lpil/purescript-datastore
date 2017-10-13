module Test.Main where

import Database.Datastore

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except as Except
import Data.Foreign as Foreign
import Data.Function.Pipe ((|>))
import Data.Maybe as Maybe
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign.Generic as FGeneric
import Data.Foreign.Generic.Types (Options)
import Test.Unit (TestSuite, test, failure)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


main :: forall e
  . Eff ( console :: CONSOLE
        , testOutput :: TESTOUTPUT
        , datastore :: DATASTORE
        , avar :: AVAR
        | e
        ) Unit
main =
  runTest suites


newtype Thing =
  Thing { score :: Int }

derive instance genericThing :: Generic Thing _
derive instance eqThing :: Eq Thing
instance showThing :: Show Thing where show = genericShow


foreign import trace :: forall e. e -> e


client :: Client
client =
  makeClient { projectId: "test-project-id" }


opts :: Options
opts =
  FGeneric.defaultOptions { unwrapSingleConstructors = true }


suites :: forall e. TestSuite (datastore :: DATASTORE | e)
suites = do
  test "get non-existing Entity" do
    let key = makeKey client "Person" "Amy"
    result <- get client key
    result
      |> Maybe.isNothing
      |> Assert.assert "non-existant Entity get should eq Nothing"

  test "save and get an Entity" do
    let key = makeKey client "Person" "Simmy"
    let thing = Thing { score: 15 }
    thing
      |> Foreign.toForeign
      |> save client key

    result <- get client key
    case result of
      Nothing ->
        failure "get returned Nothing"

      Just value ->
        value
          |> FGeneric.genericDecode opts
          |> Except.runExcept
          |> Assert.equal (Right thing)
