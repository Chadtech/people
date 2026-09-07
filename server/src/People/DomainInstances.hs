{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module People.DomainInstances () where

import ConversationId (ConversationId (..))
import ConversationTitle (ConversationTitle (..))
import Data.String (IsString)
import GenerationError (GenerationError (..))
import GenerationId (GenerationId (..))
import GoalDescription (GoalDescription (..))
import GoalId (GoalId (..))
import IntervalSeconds (IntervalSeconds (..))
import LeaseSeconds (LeaseSeconds (..))
import MemoryContent (MemoryContent (..))
import MemoryId (MemoryId (..))
import MemoryKeywords (MemoryKeywords (..))
import MessageContent (MessageContent (..))
import MessageId (MessageId (..))
import Note (Note (..))
import PersonId (PersonId (..))
import PromptSnapshot (PromptSnapshot (..))
import Revision (Revision (..))
import TurnCount (TurnCount (..))


deriving stock instance Eq PersonId
deriving stock instance Ord PersonId


instance Show PersonId where
    show (PersonId value) = show value


deriving stock instance Eq GoalId
deriving stock instance Ord GoalId


instance Show GoalId where
    show (GoalId value) = show value


deriving stock instance Eq MemoryId
deriving stock instance Ord MemoryId


instance Show MemoryId where
    show (MemoryId value) = show value


deriving stock instance Eq ConversationId
deriving stock instance Ord ConversationId


instance Show ConversationId where
    show (ConversationId value) = show value


deriving stock instance Eq MessageId
deriving stock instance Ord MessageId


instance Show MessageId where
    show (MessageId value) = show value


deriving stock instance Eq GenerationId
deriving stock instance Ord GenerationId


instance Show GenerationId where
    show (GenerationId value) = show value


deriving stock instance Eq Note
deriving stock instance Ord Note
deriving stock instance Show Note
deriving newtype instance IsString Note


deriving stock instance Eq MessageContent
deriving stock instance Ord MessageContent
deriving stock instance Show MessageContent
deriving newtype instance IsString MessageContent


deriving stock instance Eq PromptSnapshot
deriving stock instance Ord PromptSnapshot
deriving stock instance Show PromptSnapshot
deriving newtype instance IsString PromptSnapshot


deriving stock instance Eq GenerationError
deriving stock instance Ord GenerationError
deriving stock instance Show GenerationError
deriving newtype instance IsString GenerationError


deriving stock instance Eq ConversationTitle
deriving stock instance Ord ConversationTitle
deriving stock instance Show ConversationTitle
deriving newtype instance IsString ConversationTitle


deriving stock instance Eq GoalDescription
deriving stock instance Ord GoalDescription
deriving stock instance Show GoalDescription
deriving newtype instance IsString GoalDescription


deriving stock instance Eq MemoryContent
deriving stock instance Ord MemoryContent
deriving stock instance Show MemoryContent
deriving newtype instance IsString MemoryContent


deriving stock instance Eq MemoryKeywords
deriving stock instance Ord MemoryKeywords
deriving stock instance Show MemoryKeywords
deriving newtype instance IsString MemoryKeywords


deriving stock instance Eq Revision


deriving stock instance Ord Revision


deriving stock instance Show Revision


deriving newtype instance Num Revision


deriving stock instance Eq TurnCount


deriving stock instance Ord TurnCount


deriving stock instance Show TurnCount


deriving newtype instance Num TurnCount


deriving stock instance Eq IntervalSeconds


deriving stock instance Ord IntervalSeconds


deriving stock instance Show IntervalSeconds


deriving newtype instance Num IntervalSeconds


deriving stock instance Eq LeaseSeconds


deriving stock instance Ord LeaseSeconds


deriving stock instance Show LeaseSeconds


deriving newtype instance Num LeaseSeconds
