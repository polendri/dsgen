module Dsgen.SetSelect
    (
    -- * Types
    Emphasis(..),
    Filter,
    ComplexityFilterOption(..),
    Rule,
    ReactionRuleOption(..),
    TrasherRuleOption(..),
    Addition,
    ColPlatAdditionOption(..),
    SheltersAdditionOption(..),
    SetSelectOptions(..),
    SetSelectResult(..),
    SetSelectError,

    -- * Filters
    actionFilter,
    complexityFilter,

    -- * Rules
    costVarietyRule,
    interactivityRule,
    reactionRule,
    trasherRule,
    mkEmphasisRule,

    -- * Additions
    colPlatAddition,
    sheltersAddition,

    -- * Set selection functions
    selectSet
    ) where

import Dsgen.SetSelect.Internals

