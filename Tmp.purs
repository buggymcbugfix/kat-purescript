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

derive instance eqVowel :: Eq Vowel
derive instance genericVowel :: Generic Vowel _
instance showVowel :: Show Vowel where show = genericShow

associations =
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

toIPA :: Vowel -> String
toIPA v = foldr step "" associations
  where
    step {vowel: v', ipa: i} acc = if v == v' then i else acc


fromIPA :: String -> Maybe Vowel
fromIPA i = foldr step Nothing associations
  where
    step {vowel: v, ipa: i'} acc = if i == i' then Just v else acc


getRelated :: Vowel -> Array Vowel
getRelated v = foldr step [] groups
  where
    step gr acc = if v `elem` gr then gr {-delete v gr-} else acc

groups :: Array (Array Vowel)
groups = [group1, group2, group3, group4, group5]
  where
    group1 = [VowelTim, VowelTeam, VowelTime, VowelTame]
    group2 = [VowelTen, VowelTurn]
    group3 = [VowelTote, VowelTot, VowelTaught]
    group4 = [VowelPool, VowelPull]
    group5 = [VowelTan, VowelTarn, VowelTonne]