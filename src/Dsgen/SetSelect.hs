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
    ColonyAdditionOption(..),
    PlatinumAdditionOption(..),
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
    colonyAddition,
    platinumAddition,
    sheltersAddition,

    -- * Set selection functions
    selectSet
    ) where

import Dsgen.SetSelect.Internals

