module App

open Elmish
open Elmish.React
open Feliz

type HackernewsItem = {
  id: int
  title: string
  url: string
  score : int
}

type State = {
  StoryItems: HackernewsItem list
}

type Msg = Msg of unit

let initialStoryItems = [
  {
    id = 1;
    title = "F#: An open-source, cross-platform functional programming language for .NET"
    url = "https://dotnet.microsoft.com/languages/fsharp"
    score = 9000
  };
  {
    id = 2;
    title = "PostgreSQL is a powerful, open source object-relational database system with over 30 years of active development"
    url = "https://www.postgresql.org/"
    score = 8650
  }
]

let init() =
  let initialState = { StoryItems = initialStoryItems }
  let initialCmd = Cmd.none
  initialState, initialCmd

let topStoriesEndpoint = "https://hacker-news.firebaseio.com/v0/topstories.json"
let storyItemEndpoint (itemId: int) = sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" itemId

let update (msg: Msg) (state: State) = state, Cmd.none

let renderError (errorMsg: string) =
  Html.h1 [
    prop.style [ style.color.red ]
    prop.text errorMsg
  ]

let div (classes: string list) (children: ReactElement list) =
  Html.div [
    prop.className classes
    prop.children children
  ]

let renderItem item =
  Html.div [
    prop.key item.id
    prop.className "box"
    prop.style [ style.marginTop 15; style.marginBottom 15 ]
    prop.children [
      Html.a [
        prop.style [ style.textDecoration.underline ]
        prop.target.blank
        prop.href item.url
        prop.text item.title
      ]
    ]
  ]

let spinner =
  Html.div [
    prop.style [ style.textAlign.center; style.marginTop 20 ]
    prop.children [
      Html.i [
        prop.className "fa fa-cog fa-spin fa-2x"
      ]
    ]
  ]

let renderItems = function
  | HasNotStartedYet -> Html.none
  | InProgress -> spinner
  | Resolved (Error errorMsg) -> renderError errorMsg
  | Resolved (Ok items) -> Html.fragment [ for item in items -> renderItem item ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      Html.h1 [
        prop.className "title"
        prop.text "Elmish Hackernews"
      ]

      Html.fragment [
        for item in state.StoryItems -> renderItem item
      ]
    ]
  ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run