module Dsgen.SetGen
    (
    -- * Types
    Emphasis(..),
    SetGenOptions(..)
    ) where

import Paths_dsgen
import Dsgen.Cards

{- | Set generation emphasis. A set generated with an "emphasized" expansion
will contain at least 4 cards from the emphasized expansion. -}
data Emphasis = NoEmphasis
              | DominionEmphasis
              | IntrigueEmphasis
              | SeasideEmphasis
              | AlchemyEmphasis
              | ProsperityEmphasis
              | CornucopiaEmphasis
              | HinterlandsEmphasis
              | DarkAgesEmphasis
              | GuildsEmphasis

newtype Filter = [Card] -> [Card]

{- | Contains options for customizing the generation of Kingdom card sets -}
data SetGenOptions = SetGenOptions {
    setGenSources :: [CardSource],
    setGenEmphasis :: Emphasis,
    setGenFilters :: [Filter],
    setGenRules :: [Rule],
    setGenRandomColony :: Bool,
    setGenPlatinumRule :: PlatinumRule,
    setGenSheltersRule :: SheltersRule
}

