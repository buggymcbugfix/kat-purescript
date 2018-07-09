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


import Partial.Unsafe

newtype ObjectURL = ObjectURL String
derive instance newtypeFilePath :: Newtype ObjectURL _

foreign import createObjectURL :: File -> Effect ObjectURL
foreign import revokeObjectURL :: ObjectURL -> Effect Unit

-- data Vowel
--   = VowelTim
--   | VowelTeam
--   | VowelTime
--   | VowelTame
--   | VowelTen
--   | VowelTurn
--   | VowelTote
--   | VowelTot
--   | VowelTaught
--   | VowelPool
--   | VowelPull
--   | VowelTan
--   | VowelTarn
--   | VowelTonne
--
-- derive instance eqVowel :: Eq Vowel
-- derive instance genericVowel :: Generic Vowel _
-- instance showVowel :: Show Vowel where show = genericShow
--
-- associations =
--   [ { vowel: VowelTim,    ipa: "ɪ" }
--   , { vowel: VowelTeam,   ipa: "iː" }
--   , { vowel: VowelTime,   ipa: "aɪ" }
--   , { vowel: VowelTame,   ipa: "eɪ" }
--   , { vowel: VowelTen,    ipa: "e" }
--   , { vowel: VowelTurn,   ipa: "ɜː" }
--   , { vowel: VowelTote,   ipa: "ɔː" }
--   , { vowel: VowelTot,    ipa: "əʊ" }
--   , { vowel: VowelTaught, ipa: "ɒ" }
--   , { vowel: VowelPool,   ipa: "u:" }
--   , { vowel: VowelPull,   ipa: "ʊ" }
--   , { vowel: VowelTan,    ipa: "æ" }
--   , { vowel: VowelTarn,   ipa: "ɑː" }
--   , { vowel: VowelTonne,  ipa: "ʌ" }
--   ]
--
-- toIPA :: Vowel -> String
-- toIPA v = foldr step "" associations
--   where
--     step {vowel: v', ipa: i} acc = if v == v' then i else acc
--
--
-- fromIPA :: String -> Maybe Vowel
-- fromIPA i = foldr step Nothing associations
--   where
--     step {vowel: v, ipa: i'} acc = if i == i' then Just v else acc
--
--
-- getRelated :: Vowel -> Array Vowel
-- getRelated v = foldr step [] groups
--   where
--     step gr acc = if v `elem` gr then gr {-delete v gr-} else acc
--
-- groups :: Array (Array Vowel)
-- groups = [group1, group2, group3, group4, group5]
--   where
--     group1 = [VowelTim, VowelTeam, VowelTime, VowelTame]
--     group2 = [VowelTen, VowelTurn]
--     group3 = [VowelTote, VowelTot, VowelTaught]
--     group4 = [VowelPool, VowelPull]
--     group5 = [VowelTan, VowelTarn, VowelTonne]

data VowelGroup = A | E | I | O | U

vowelGroups :: Array VowelGroup
vowelGroups = [A,E,I,O,U]

derive instance eqVowelGroup :: Eq VowelGroup
derive instance genericVowelGroup :: Generic VowelGroup _
instance showVowelGroup :: Show VowelGroup where show = genericShow

type English = String

type Stimulus =
  { spelling :: English
  -- , pronunciation :: ObjectURL  -- wav file location
  }

type Challenge =
  { correctAnswer :: Stimulus
  , incorrectAnswers :: NEA.NonEmptyArray Stimulus
  , vowelGroup :: VowelGroup    -- the vowel we are testing against
  }

data Result = Correct | Incorrect

type State =
  { results :: Array { vowelGroup :: VowelGroup, result :: Result }
  , current :: Challenge
  , answerGiven :: Maybe Result
  , todo :: Array Challenge
  , finished :: Boolean
  }

data Query a
  = SelectAnswer English a
  -- | Replay a
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
            [ replayButton {-, audio -} , choices, message ]
          ]
      where
        replayButton = HH.button
          [  {- HE.onClick $ HE.input_ Replay
          ,-} HP.class_ $ wrap "btn btn-success" ]
          [ HH.label_ [ HH.text "replay" ] ]

        -- audio = HH.div_
        --   [ HH.audio -- TODO: hidden attribute?
        --     [ HP.ref $ wrap "audio"
        --     , HP.src $ state.current.url
        --     , HP.controls false
        --     , HP.autoplay true
        --     ]
        --     []
        --   ]

        choices = HH.div
            [ HP.class_ $ wrap "list-group" ]
            (state.current.correctAnswer : (NEA.toArray state.current.incorrectAnswers) -- TODO: shuffle!
              <#>
                \ans -> HH.button
                  [ HE.onClick $ HE.input_ $ SelectAnswer ans.spelling
                  , HP.class_ $ wrap "list-group-item list-group-item-warning list-group-item-action"
                  ]
                  [ HH.label_ [ HH.text ans.spelling ] ])


        message = case state.answerGiven of
            Nothing -> HH.div [] []
            Just Correct ->
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
            Just Incorrect ->
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

    -- log' = H.liftAff <<< log

    eval :: Query ~> H.ComponentDSL State Query Void Aff
    -- eval (Replay next) = do
    --   audio <- H.getHTMLElementRef $ wrap "audio"
    --   case AudioElement.toHTMLMediaElement <$> (AudioElement.fromHTMLElement =<< audio) of
    --     Just el -> do
    --       ended <- H.liftEffect ended
    --       H.liftEffect $ when ended (play el)
    --     Nothing -> log' "No audio ref found"
    --   pure next

    eval (SelectAnswer ans next) = do
      _ <- H.modify \st ->
        let result = if ans == st.current.correctAnswer.spelling then Correct else Incorrect
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
    x = NEA.singleton { correctAnswer: { spelling: "A" }, incorrectAnswers: (NEA.singleton { spelling: "B" } <> NEA.singleton { spelling: "C" }), vowelGroup: U }
