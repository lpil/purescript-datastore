module Test.Main where

import Prelude (class Eq, class Show, Unit, unit, bind, discard, pure)

import Control.Monad.Eff (Eff)
import Control.Bind (bindFlipped)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except as Except
import Control.Monad.Aff as Aff

import Data.Foreign as Foreign
import Data.Function.Pipe ((|>))
import Data.Maybe as Maybe
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign.Generic as FGeneric
import Data.Foreign.Generic.Types (Options)

import Test.Unit (TestSuite, suite, test, failure)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import Database.Datastore


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
  suite "Databse.Datastore" do
    suite "get" do
      test "get non-existing Entity" do
        let key = [Tuple (Kind "Person") (Name "Jane")]
        delete client key
        result <- get client key
        result
          |> Maybe.isNothing
          |> Assert.assert "non-existant Entity get should eq Nothing"

    suite "upsert" do
      test "upsert and get a new named Entity" do
        let kind = Kind "Person"
        let name = Name "Simmy"
        let key = [Tuple kind name]
        delete client key

        let thing = Thing { score: 15 }
        thing
          |> Foreign.toForeign
          |> upsert client [] kind name

        result <- get client key
        case result of
          Nothing ->
            failure "get returned Nothing"

          Just value -> do
            value
              |> _.kind
              |> Assert.equal "Person"

            value
              |> _.id
              |> Assert.equal (Name "Simmy")

            value
              |> _.path
              |> Assert.equal [Tuple (Kind "Person") (Name "Simmy")]

            value
              |> _.data
              |> FGeneric.genericDecode opts
              |> Except.runExcept
              |> Assert.equal (Right thing)

      test "upsert and get a new numbered Entity" do
        let kind = Kind "Person"
        let id = Id 50
        let key = [Tuple kind id]
        delete client key

        let thing = Thing { score: 15 }
        thing
          |> Foreign.toForeign
          |> upsert client [] kind id

        result <- get client key
        case result of
          Nothing ->
            failure "get returned Nothing"

          Just value -> do
            value
              |> _.kind
              |> Assert.equal "Person"

            value
              |> _.id
              |> Assert.equal (Id 50)

            value
              |> _.path
              |> Assert.equal [Tuple (Kind "Person") (Id 50)]

            value
              |> _.data
              |> FGeneric.genericDecode opts
              |> Except.runExcept
              |> Assert.equal (Right thing)

      test "overwriting and existing record" do
        let kind = Kind "Person"
        let id = Id 50
        let key = [Tuple kind id]
        delete client key

        let thing1 = Thing { score: 1 }
        thing1
          |> Foreign.toForeign
          |> upsert client [] kind id

        let thing2 = Thing { score: 2 }
        thing2
          |> Foreign.toForeign
          |> upsert client [] kind id

        result <- get client key
        case result of
          Nothing ->
            failure "get returned Nothing"

          Just value -> do
            value
              |> _.kind
              |> Assert.equal "Person"

            value
              |> _.id
              |> Assert.equal (Id 50)

            value
              |> _.path
              |> Assert.equal [Tuple (Kind "Person") (Id 50)]

            value
              |> _.data
              |> FGeneric.genericDecode opts
              |> Except.runExcept
              |> Assert.equal (Right thing2)

    suite "insert" do
      test "insert and get a new named Entity" do
        let kind = Kind "Person"
        let name = Name "Simmy"
        let key = [Tuple kind name]
        delete client key

        let thing = Thing { score: 15 }
        thing
          |> Foreign.toForeign
          |> insert client [] kind name

        result <- get client key
        case result of
          Nothing ->
            failure "get returned Nothing"

          Just value -> do
            value
              |> _.kind
              |> Assert.equal "Person"

            value
              |> _.id
              |> Assert.equal (Name "Simmy")

            value
              |> _.path
              |> Assert.equal [Tuple (Kind "Person") (Name "Simmy")]

            value
              |> _.data
              |> FGeneric.genericDecode opts
              |> Except.runExcept
              |> Assert.equal (Right thing)

      test "insert and get a new numbered Entity" do
        let kind = Kind "Person"
        let id = Id 50
        let key = [Tuple kind id]
        delete client key

        let thing = Thing { score: 15 }
        thing
          |> Foreign.toForeign
          |> insert client [] kind id

        result <- get client key
        case result of
          Nothing ->
            failure "get returned Nothing"

          Just value -> do
            value
              |> _.kind
              |> Assert.equal "Person"

            value
              |> _.id
              |> Assert.equal (Id 50)

            value
              |> _.path
              |> Assert.equal [Tuple (Kind "Person") (Id 50)]

            value
              |> _.data
              |> FGeneric.genericDecode opts
              |> Except.runExcept
              |> Assert.equal (Right thing)

      test "cannot insert over existing Entity" do
        let kind = Kind "Person"
        let id = Id 50
        let key = [Tuple kind id]
        delete client key

        Thing { score: 1 }
          |> Foreign.toForeign
          |> insert client [] kind id

        Thing { score: 2 }
          |> Foreign.toForeign
          |> insert client [] kind id
          |> Aff.attempt
          |> bindFlipped case _ of
            Right _ ->
              failure "Should not have inserted over existing Entity"

            Left msg ->
              pure unit

    suite "update" do
      test "cannot update a non-existing Entity" do
        let kind = Kind "Person"
        let id = Id 88
        let key = [Tuple kind id]
        delete client key

        Thing { score: 1 }
          |> Foreign.toForeign
          |> update client [] kind id
          |> Aff.attempt
          |> bindFlipped case _ of
            Right _ ->
              failure "Should not have updated a non-existing Entity"

            Left msg ->
              pure unit

      test "update an existing Entity" do
        let kind = Kind "Person"
        let id = Id 50
        let key = [Tuple kind id]
        delete client key

        Thing { score: 15 }
          |> Foreign.toForeign
          |> insert client [] kind id

        let thing = Thing { score: 1 }
        thing
          |> Foreign.toForeign
          |> update client [] kind id

        result <- get client key
        case result of
          Nothing ->
            failure "get returned Nothing"

          Just value -> do
            value
              |> _.kind
              |> Assert.equal "Person"

            value
              |> _.id
              |> Assert.equal (Id 50)

            value
              |> _.path
              |> Assert.equal [Tuple (Kind "Person") (Id 50)]

            value
              |> _.data
              |> FGeneric.genericDecode opts
              |> Except.runExcept
              |> Assert.equal (Right thing)

    suite "delete" do
      test "deletion" do
        let kind = Kind "Person"
        let name = Name "Tina"
        let key = [Tuple kind name]
        let thing = Thing { score: 2 }
        thing
          |> Foreign.toForeign
          |> upsert client [] kind name

        result1 <- get client key
        result1
          |> Maybe.isJust
          |> Assert.assert "Thing should be persisted"

        delete client key

        result2 <- get client key
        result2
          |> Maybe.isNothing
          |> Assert.assert "Thing should not be persisted"
