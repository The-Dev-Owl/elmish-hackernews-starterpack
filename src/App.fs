module App

open Elmish
open Elmish.React
open Feliz
open Thoth.Json
open Fable.SimpleHttp

type HackernewsItem = {
  id: int
  title: string
  url: string
  score : int
}

let itemDecoder : Decoder<HackernewsItem> = 
  Decode.object (fun fields -> {
    id = fields.Required.At [ "id" ] Decode.int
    title = fields.Required.At [ "title" ] Decode.string
    url = fields.Required.At [ "url" ] Decode.string
    score = fields.Required.At [ "score" ] Decode.int
  })

let parseItem (json: string) = 
  Decode.fromString itemDecoder json


type State = {
  StoryItems: Deferred<Result<HackernewsItem list, string>>
}
(*
Initialized       -> HasNotStartedYet
Loading           -> InProgress
Loaded with Error -> Resolved(Error "msg")
Loaded with Data  -> Resolved(Ok list)
*)
type Msg = LoadStoryItems of AsyncOperationEvent<Result<HackernewsItem list, string>>

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

// Decoder<'t> 

let init() =
  let initialState = { StoryItems = HasNotStartedYet }
  let initialCmd = Cmd.ofMsg (LoadStoryItems Started)
  initialState, initialCmd

let topStoriesEndpoint = "https://hacker-news.firebaseio.com/v0/topstories.json"
let storyItemEndpoint (itemId: int) = sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" itemId

let (|HttpOk|HttpError|) status =
  match status with
  | 200 -> HttpOk
  | _ -> HttpError

let loadStoryItem (itemId: int) = async {
  let! (statusCode, responseText) = Http.get (storyItemEndpoint itemId)
  match statusCode with 
  | HttpOk -> 
      match parseItem responseText with 
      | Ok item -> return Some item
      | _ -> return None
  | HttpError -> 
      return None
}

let loadStoryItems = async {
  do! Async.Sleep 1000
  let! (statusCode, responseText) = Http.get topStoriesEndpoint
  match statusCode with 
  | HttpOk ->
      match Decode.fromString (Decode.list Decode.int) responseText with 
      | Ok ids ->
        let! storyItems =  
          ids
          |> List.truncate 50 
          |> List.map loadStoryItem 
          |> Async.Parallel
          |> Async.map (Array.choose id >> List.ofArray)

        return LoadStoryItems (Finished (Ok storyItems))
      
      | Error parseError -> 
          return LoadStoryItems (Finished (Error parseError))
  | HttpError -> 
      return LoadStoryItems (Finished (Error responseText))
}

let update (msg: Msg) (state: State) =
  match msg with 
  | LoadStoryItems Started ->
      { StoryItems = InProgress }, Cmd.fromAsync loadStoryItems
  
  | LoadStoryItems (Finished (Error error)) ->
      { StoryItems = Resolved (Error error) }, Cmd.none

  | LoadStoryItems (Finished (Ok items)) -> 
      { StoryItems = Resolved (Ok items) }, Cmd.none

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

let renderItems deferred = 
  match deferred with
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

      renderItems state.StoryItems
    ]
  ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run