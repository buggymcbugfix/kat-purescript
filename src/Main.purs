module Main where

import Control.Applicative (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Except.Trans (lift, runExceptT)
import Data.Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as D
import Prelude
import Web.File.File (File)
import Web.File.File as File
import Web.File.FileList as FileList
import Web.HTML.HTMLAudioElement as AudioElement
import Web.HTML.HTMLInputElement as InputElement
import Web.HTML.HTMLMediaElement (ended, play)

-- import Data.Foldable (elem, foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (class GenericShow, genericShow)


data VowelGroup = A | E | I | O | U

vowelGroups :: Array VowelGroup
vowelGroups = [A,E,I,O,U]

derive instance eqVowelGroup :: Eq VowelGroup
derive instance genericVowelGroup :: Generic VowelGroup _
instance showVowelGroup :: Show VowelGroup where show = genericShow

type English = String

type Stimulus =
  { spelling :: English
  , pronunciation :: String  -- wav file location
  }

type Challenge =
  { correctAnswer :: Stimulus
  , incorrectAnswers :: NEA.NonEmptyArray Stimulus
  , vowelGroup :: VowelGroup    -- the vowel we are testing against
  }

data Result = Correct English | Incorrect English

type State =
  { results :: Array { vowelGroup :: VowelGroup, result :: Result }
  , current :: Challenge
  , answerGiven :: Maybe Result
  , todo :: Array Challenge
  , finished :: Boolean
  }

data Query a
  = SelectAnswer English a
  | Replay a
  | GoToNext a

ui :: NEA.NonEmptyArray Challenge -> H.Component HH.HTML Query Unit Void Aff
ui challenges = H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

  where
    initialState :: forall a . a -> State
    initialState = const
      { results: []
      , current: NEA.head challenges
      , answerGiven: Nothing
      , todo: NEA.tail challenges
      , finished: false
      }

    render :: State -> H.ComponentHTML Query
    render state = if state.finished
        then HH.h1_ [HH.text "End."]
        else HH.div
          [ HP.class_ $ wrap "container" ]
          [ HH.div
            [ HP.class_ $ wrap "root" ]
            [ replayButton, audio, choices, message ]
          ]
      where
        replayButton = HH.button
          [ HE.onClick $ HE.input_ Replay
          , HP.class_ $ wrap "btn btn-success" ]
          [ HH.label_ [ HH.text "replay" ] ]

        audio = HH.audio -- TODO: hidden attribute?
            [ HP.ref $ wrap "audio"
            , HP.src $ state.current.correctAnswer.pronunciation
            , HP.controls false
            , HP.autoplay true
            ]
            []

        choices = HH.div
            [ HP.class_ $ wrap "list-group" ]
            (state.current.correctAnswer : (NEA.toArray state.current.incorrectAnswers) -- TODO: shuffle!
              <#>
                \ans -> HH.button
                  (case state.answerGiven of
                    Nothing ->
                      [ HE.onClick $ HE.input_ $ SelectAnswer ans.spelling
                      , HP.class_ $ wrap "list-group-item list-group-item-action"
                      ]
                    Just (Correct answerGiven) ->
                      [ HE.onClick $ HE.input_ Replay
                      , if ans.spelling == answerGiven
                          then HP.class_ $ wrap "list-group-item list-group-item-action-success disabled"
                          else HP.class_ $ wrap "list-group-item disabled"
                      ]
                    Just (Incorrect answerGiven) ->
                      [ HE.onClick $ HE.input_ Replay
                      , if ans.spelling == answerGiven
                          then HP.class_ $ wrap "list-group-item list-group-item-action-danger disabled"
                          else HP.class_ $ wrap "list-group-item disabled"
                      ]
                  )
                  [ HH.label_ [ HH.text ans.spelling ] ])


        message = case state.answerGiven of
            Nothing -> HH.div [] []
            Just (Correct _) ->
              HH.div
                [HP.class_ $ wrap "card border-success mb-3"]
                [HH.div
                  [HP.class_ $ wrap "card-body text-success"]
                  [HH.h2
                    [HP.class_ $ wrap "card-title"]
                    [HH.text "Well done!"]
                  ,HH.button
                    [HE.onClick $ HE.input_ GoToNext
                    , HP.class_ $ wrap "btn btn-success" ]
                    [HH.label_ [ HH.text "Next" ]]
                  ]
                ]
            Just (Incorrect _) ->
              HH.div
                [HP.class_ $ wrap "card border-danger mb-3"]
                [HH.div
                  [HP.class_ $ wrap "card-body text-danger"]
                  [HH.h2
                    [HP.class_ $ wrap "card-title"]
                    [HH.text "Oh noes!"]
                  ,HH.button
                    [HE.onClick $ HE.input_ GoToNext
                    , HP.class_ $ wrap "btn btn-danger" ]
                    [HH.label_ [ HH.text "Next" ]]
                  ]
                ]

    eval :: Query ~> H.ComponentDSL State Query Void Aff
    eval (Replay next) = do
      audio <- H.getHTMLElementRef $ wrap "audio"
      case AudioElement.toHTMLMediaElement <$> (AudioElement.fromHTMLElement =<< audio) of
        Just el -> do
          H.liftEffect $ play el
          -- ended <- H.liftEffect ended
          -- H.liftEffect $ when ended (play el)
        Nothing -> H.liftAff $ log "No audio ref found"
      pure next

    eval (SelectAnswer ans next) = do
      _ <- H.modify \st ->
        let result = if ans == st.current.correctAnswer.spelling then Correct ans else Incorrect ans
        in st { answerGiven = Just result
              , results = {result, vowelGroup: st.current.vowelGroup} : st.results}
      pure next

    eval (GoToNext next) = do
      _ <- H.modify \st ->
        case uncons st.todo of
          Just { head: x, tail: xs } -> st { current = x, todo = xs, answerGiven = Nothing}
          Nothing -> st { finished = true }
      pure next

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  io <- D.runUI (ui challenges) unit body

  log "Running"

challenges :: NEA.NonEmptyArray Challenge
challenges = x <> x
  where
    x = NEA.singleton
      { correctAnswer: { spelling: "A", pronunciation: "assets/audio/kaɪl-kyle-Chloe-1.wav" }
      , incorrectAnswers:
        NEA.singleton { spelling: "B", pronunciation: "" }
        <> NEA.singleton { spelling: "C", pronunciation: "" }
      , vowelGroup: U
      }
