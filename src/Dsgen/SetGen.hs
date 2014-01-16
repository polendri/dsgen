module Dsgen.SetGen where

import Paths_dsgen
import Dsgen.Cards

data Emphasis = NoEmphasis
              | RandomEmphasis
              | DominionEmphasis
              | IntrigueEmphasis
              | SeasideEmphasis
              | AlchemyEmphasis
              | ProsperityEmphasis
              | CornucopiaEmphasis
              | HinterlandsEmphasis
              | DarkAgesEmphasis
              | GuildsEmphasis

data SetGenOptions = SetGenOptions {
    setGenSources :: [CardSource],
    setGenEmphasis :: Emphasis,
    setGenFilters :: [Filter],
    setGenRules :: [Rule],
    setGenRandomColony :: Bool,
    setGenPlatinumRule :: PlatinumRule,
    setGenSheltersRule :: SheltersRule
}
