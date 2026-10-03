{-# LANGUAGE OverloadedStrings #-}

module People.SelectionReason (SelectionReason (..), toText) where

import Data.Text (Text)
import qualified Data.Text as T
import GenerationId (GenerationId)
import People.DomainInstances ()


-- Explains a prompt block's role or provenance when included, or why it was
-- omitted. These are application selection decisions, not model reasoning.
-- Keep them typed until rendering prompt headings or saved inspection text.
data SelectionReason
    = PersonIdentity
    | ConversationParticipants
    | RecentConversation
    | ActiveGoal
    | CuratedContext
    | ModelReflection GenerationId
    | RetiredMemory
    | KeywordsDidNotMatch
    | ContextBudgetExceeded
    deriving (Eq, Show)


toText :: SelectionReason -> Text
toText selectionReason =
    case selectionReason of
        PersonIdentity ->
            "person"

        ConversationParticipants ->
            "conversation"

        RecentConversation ->
            "recent conversation"

        ActiveGoal ->
            "active goal"

        CuratedContext ->
            "curated context"

        ModelReflection generation ->
            "model reflection from generation "
                <> T.pack (show generation)
                <> "; may be mistaken"

        RetiredMemory ->
            "retired"

        KeywordsDidNotMatch ->
            "keywords did not match"

        ContextBudgetExceeded ->
            "excluded by context byte budget"
