module Lib1
    ( examples, Command(..), Dumpable(..)
    ) where

data Dumpable = Examples
  deriving Show

data PrepositionTo = To deriving Show

data ChoiceOption = ChoiceOption
  { text :: String
  , next :: Story
  }
  deriving Show

data ChapterElement =
  Title String
  | Paragraph String
  | Dialogue String String
  | Monologue String
  | Picture {url :: String, altText :: String}
  | Choice [ChoiceOption]
  deriving Show

data Story =
  Story [ChapterElement]
  | Add ChapterElement PrepositionTo Story
  | MergeStories Story Story
  | EmptyStory
  deriving Show

-- This is a "root" ADT representing your grammar,
-- Please expand this ADT as needed
data Command = Dump Dumpable
  | Create Story
  | Publish Story String PrepositionTo String
  deriving Show

examples :: [Command]
examples = [
    Dump Examples,
    Create (Story
      [
        Title "Intro",
        Paragraph "You wake up in a small village, the sun rising over the hills."
      ]
    ),
    Create (Story
      [ Dialogue "Villager" "Welcome to our village!",
        Monologue "I hope today will be an adventure.",
        Picture { url = "http://example.com/village.png", altText = "Village view" }
      ]
    ),
    Publish (Story [Title "Intro"]) "Amazing Book" To "My Blog",
    Create (Story
      [
        Paragraph "You come to a fork in the road.",
        Choice [
          ChoiceOption "Take the left path" (Story
            [ Paragraph "The forest is dark and quiet.",
              Choice [
                ChoiceOption "Go back to crossroads" (Story
                  [ Paragraph "You are back at the crossroads." ]
                )
              ]
            ]
          ),
          ChoiceOption "Take the right path" (Story
            [ Paragraph "A small village appears in the distance." ]
          )
        ]
      ]
    )
  ]
