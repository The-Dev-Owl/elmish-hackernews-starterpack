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

// get rid of tuple in error
type StoryItem = Deferred<Result<HackernewsItem, string>>

type Stories = Deferred<Result<Map<int, StoryItem>, string>>

let itemDecoder : Decoder<HackernewsItem> = 
  Decode.object (fun fields -> {
    id = fields.Required.At [ "id" ] Decode.int
    title = fields.Required.At [ "title" ] Decode.string
    url = fields.Required.At [ "url" ] Decode.string
    score = fields.Required.At [ "score" ] Decode.int
  })

let parseItem (json: string) = 
  Decode.fromString itemDecoder json

type StoryLimit = StoryLimit of int

type State = {
  StoryLimit: StoryLimit
  // not really fond of Deferred in Deferred, there is 
  // an opportunity for a better model!
  Stories:  Stories
}

// only two messages needed
type Msg =
  | LoadStoryIds of AsyncOperationEvent<Result<int list, string>>
  | LoadStoryItem of AsyncOperationEvent<Result<int * HackernewsItem, (int*string)>>

let init() =
                                      // getStoryLimit not needed
  let initialState = { StoryLimit = StoryLimit 10; Stories = HasNotStartedYet }
  let initialCmd = Cmd.ofMsg (LoadStoryIds Started)
  initialState, initialCmd

let topStoriesEndpoint = "https://hacker-news.firebaseio.com/v0/topstories.json"
let storyItemEndpoint (itemId: int) = sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" itemId

let (|HttpOk|HttpError|) status =
  match status with
  | 200 -> HttpOk
  | _ -> HttpError

let loadStoryItem (id: int) = async {
  let! (statusCode, responseText) = Http.get (storyItemEndpoint id)
  match statusCode with 
  | HttpOk -> 
      match parseItem responseText with 
      | Ok item -> 
          return LoadStoryItem(Finished(Ok (id, item)))

      | _ -> 
          printfn "Story error: %i" id
          return LoadStoryItem(Finished(Error (id, responseText)))

  | HttpError -> 
      return LoadStoryItem(Finished (Error (id, (sprintf "error on endpoint %i" statusCode))))
}

let loadStoryIds (StoryLimit storyLimit) = async {
  let! (statusCode, responseText) = Http.get topStoriesEndpoint
  match statusCode with 
  | HttpOk ->
      match Decode.fromString (Decode.list Decode.int) responseText with 
      | Ok ids -> 
          return LoadStoryIds(Finished(Ok (ids |> List.truncate storyLimit)))

      | Error parseError -> 
          return LoadStoryIds (Finished (Error parseError))
          
  | HttpError -> 
      return LoadStoryIds (Finished (Error responseText))
}

// new declarative function withStory. 
// Simplifies the update and no need for 
// seperate addStoryError or addStory
let withStory item state =
  match state.Stories with 
  | HasNotStartedYet | InProgress | Resolved (Error _) -> 
      state

  | Resolved (Ok stories) ->
      let id,value =
        match item with
        | Ok (id, item) ->
            id,Ok item

        | Error (id, error) ->
            id,Error error

      // use Map.x functions to be more F# idiomatic
      // Map.add replaces the value if available
      // no need for removing
      let stories =
        stories
        |> Map.add id (Resolved value)
        |> Ok
        |> Resolved            

      { state with Stories = stories }      

let update (msg: Msg) (state: State) =
  match msg with 
  | LoadStoryIds Started ->
      state, Cmd.fromAsync (loadStoryIds state.StoryLimit)

  | LoadStoryIds (Finished(Error error)) -> 
      {state with Stories = Resolved (Error error)}, Cmd.none

  | LoadStoryIds (Finished(Ok ids)) ->
      let stories  = 
        ids 
        |> List.map (fun i -> i, HasNotStartedYet) 
        |> Map.ofList
        |> Ok 
        |> Resolved

      // direct use of Cmd.fromAsync makes LoadItem Msg obsolete
      let cmds = 
        ids 
        |> List.map (loadStoryItem >> Cmd.fromAsync) 

      { state with Stories = stories }, Cmd.batch cmds

  // only one case for finished needed
  // better keep update function small and use declarative style code
  // you can later dig in to see what is actually happening
  | LoadStoryItem (Finished story) ->
      state |> withStory story, Cmd.none
        
  | LoadStoryItem Started -> state, Cmd.none

let renderItemError (id:int) (error:string) =
  Html.div [
    prop.key id
    prop.className "box"
    prop.style [ style.marginTop 15; style.marginBottom 15 ]
    prop.children [
      Html.h1 [
        prop.style [ style.color.red ]
        prop.text (sprintf "%s - Story Id: %i" error id)
      ]     
    ]
  ]

let div (classes: string list) (children: ReactElement list) =
  Html.div [
    prop.className classes
    prop.children children
  ]

let spinner =
  Html.div [
    prop.style [ style.textAlign.center ]
    prop.children [
      Html.i [
        prop.className "fa fa-cog fa-spin fa-2x"
      ]
    ]
  ]

let cellLink item =
  Html.a [
    prop.style [ style.textDecoration.underline ]
    prop.target.blank
    prop.href item.url
    prop.text item.title
  ]

let renderItemDom (id:int) (item: HackernewsItem option) =
    Html.div [
      prop.key id
      prop.className "box"
      prop.style [ style.marginTop 15; style.marginBottom 15 ]
      prop.children [
        match item with 
        | Some x -> cellLink x  
        | None -> spinner 
      ]
    ]
let renderItem id (item:StoryItem) =
  match item with
  | HasNotStartedYet -> renderItemDom id None
  | InProgress -> renderItemDom id None
               // id is already in function, no need to have it in tuple
  | Resolved (Error error) -> renderItemError id error
  | Resolved (Ok item) -> renderItemDom id (Some item)


let renderStories (deferred: Stories) = 
  match deferred with
  | HasNotStartedYet -> Html.none
  | InProgress -> spinner
  | Resolved (Error _) -> Html.none
  | Resolved (Ok items) -> 
      Html.fragment [ for KeyValue(k,v) in items -> renderItem k v ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      Html.h1 [
        prop.className "title"
        prop.text "Elmish Hackernews"
      ]

      renderStories state.Stories
    ]
  ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run