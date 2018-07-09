module Temp where

data Vowel
  = VowelTim
  | VowelTeam
  | VowelTime
  | VowelTame
  | VowelTen
  | VowelTurn
  | VowelTote
  | VowelTot
  | VowelTaught
  | VowelPool
  | VowelPull
  | VowelTan
  | VowelTarn
  | VowelTonne

assoc =
  [ { vowel: VowelTim,    ipa: "ɪ" }
  , { vowel: VowelTeam,   ipa: "iː" }
  , { vowel: VowelTime,   ipa: "aɪ" }
  , { vowel: VowelTame,   ipa: "eɪ" }
  , { vowel: VowelTen,    ipa: "e" }
  , { vowel: VowelTurn,   ipa: "ɜː" }
  , { vowel: VowelTote,   ipa: "ɔː" }
  , { vowel: VowelTot,    ipa: "əʊ" }
  , { vowel: VowelTaught, ipa: "ɒ" }
  , { vowel: VowelPool,   ipa: "u:" }
  , { vowel: VowelPull,   ipa: "ʊ" }
  , { vowel: VowelTan,    ipa: "æ" }
  , { vowel: VowelTarn,   ipa: "ɑː" }
  , { vowel: VowelTonne,  ipa: "ʌ" }
  ]

