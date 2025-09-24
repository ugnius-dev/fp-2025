# Storytelling DSL

## Description
Stories are made of chapters, which can be titles, paragraphs, dialogues, monologues, pictures, or choices that link to other stories. The DSL allows authors to create stories incrementally, add chapters, merge stories, and publish them.

## Main Entities
 1. Story: A collection of chapters or nested stories; can be empty or merged with other stories.
    1. Add: add a chapterElement to an existing story
    2. MergeStories: combine two stories
 2. ChapterElement: Represents the building blocks of a story:
    1. Title
    2. Paragraph
    3. Dialogue
    4. Monologue
    5. Picture: Image with URL and alt text
    6. Choice: A branching option pointing to another story (array)
 3. ChoiceOption: Represents a single choice with text and a target story
 4. Command:
    1. Create: create a new story
    2. Publish: publish a named story to a target

## Examples
```
-- 1. Create a simple story
Create (Story
  [
    Title "Intro",
    Paragraph "You wake up in a small village, the sun rising over the hills."
  ]
)

-- 3. Create a story with dialogue, monologue, and picture
Create (Story
  [
    Dialogue "Villager" "Welcome to our village!",
    Monologue "I hope today will be an adventure.",
    Picture { url = "http://example.com/village.png", altText = "Village view" }
  ]
)

-- 4. Recursive story: Choice points to another story
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

-- 5. Publish story
Publish (Story [Title "Intro"]) "Amazing Book" To "My Blog"

```

## Domain's BNF
```BNF
/* Command */
<command> ::= <dump> | <create> | <publish>
<dump> ::= "Dump" <wpo> "Examples"
<create> ::= "Create" <wpo> <story>
<publish> ::= "Publish" <wpo> <story> <wpo> <string> <wpo> "To" <wpo> <string>

/* Story */
<story> ::= <obl> (<initStory> | <addTo> | <mergeStories> | <emptyStory>) <obr>
<initStory> ::= "Story" <wpo> "[" <wp> <chapterElementList> <wp> "]"
<chapterElementList> ::= <chapterElement> | <chapterElement> <wp> "," <wp> <chapterElementList>
<addTo> ::= "Add" <wpo> <chapterElement> <wpo> "To" <wpo> <story>
<mergeStories> ::= "MergeStories" <wpo> <story> <wpo> <story>
<emptyStory> ::= "EmptyStory"

/* ChapterElement */
<chapterElement> ::= <oobl> (<title> | <paragraph> | <dialogue> | <monologue> | <picture> | <choice>) <oobr>
<title> ::= "Title" <wpo> <string>
<paragraph> ::= "Paragraph" <wpo> <string>
<dialogue> ::= "Dialogue" <wpo> <string> <wpo> <string>
<monologue> ::= "Monologue" <wpo> <string>
<picture> ::= "Picture" <wpo> "{" <wp> "url" <wpo> "=" <wpo> <string> <wp> "," <wp> "altText" <wpo> "=" <wpo> <string> <wp> "}"
<choice> ::= "Choice" <wpo> "[" <wp> <choiceList> <wp> "]"
<choiceList> ::= <choiceOption> | <choiceOption> <wp> "," <wp> <choiceList>

/* ChoiceOption */
<choiceOption> ::= "ChoiceOption" <wpo> <string> <wpo> <story>

/* Chars helpers */
<string> ::= "\"" <character>+ "\""
<character> ::= [A-Z] | [a-z] | [0-9] | "." | "_" | " " | "-"
/* open bracket left/right */
<obl> ::= <wp> "(" <wp>
<obr> ::= <wp> ")" <wp>
<oobl> ::= <wp> | <obl>
<oobr> ::= <wp> | <obr>
<wp> ::= " "*
<wpo> ::= " "+
```
