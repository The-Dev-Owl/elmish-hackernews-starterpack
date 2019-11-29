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
type StoryItem = Deferred<Result<HackernewsItem, (int*string)>>
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
  Stories:  Stories
}
type Msg =
  | LoadItem of int
  | LoadStoryIds of AsyncOperationEvent<Result<int list, string>>
  | LoadStoryItem of AsyncOperationEvent<Result<int * HackernewsItem, (int*string)>>

// Decoder<'t> 

let getStoryLimit limit = limit |> StoryLimit

let init() =
  let initialState = { StoryLimit = (getStoryLimit 10); Stories = HasNotStartedYet }
  let initialCmd = Cmd.ofMsg (LoadStoryIds Started)
  initialState, initialCmd

let topStoriesEndpoint = "https://hacker-news.firebaseio.com/v0/topstories.json"
let storyItemEndpoint (itemId: int) = sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" itemId

let (|HttpOk|HttpError|) status =
  match status with
  | 200 -> HttpOk
  | _ -> HttpError

let loadStoryItem (id: int) = async {

  let r = System.Random()
  do! Async.Sleep (r.Next(1000,5000))
  let! (statusCode, responseText) = Http.get (storyItemEndpoint id)
  match statusCode with 
  | HttpOk -> 
      match parseItem responseText with 
      | Ok item -> return LoadStoryItem(Finished(Ok (id, item)))
      | _ -> 
        printfn "Story error: %i" id
        return LoadStoryItem(Finished(Error (id, responseText)))

  | HttpError -> return LoadStoryItem(Finished (Error (id, (sprintf "error on endpoint %i" statusCode))))
}

let loadStoryIds storyLimit = async {
  do! Async.Sleep 1000
  let! (statusCode, responseText) = Http.get topStoriesEndpoint
  match statusCode with 
  | HttpOk ->
      match Decode.fromString (Decode.list Decode.int) responseText with 
      | Ok ids -> return LoadStoryIds(Finished(Ok (ids |> List.truncate storyLimit)))
      | Error parseError -> 
          return LoadStoryIds (Finished (Error parseError))
  | HttpError -> 
      return LoadStoryIds (Finished (Error responseText))
}

let addStoryError (stories: Stories) id error =
  match stories with
  | HasNotStartedYet -> stories
  | InProgress -> stories
  | Resolved x ->
    match x with
    | Error _ -> stories
    | Ok y -> Resolved(Ok (y.Remove(id).Add(id, Resolved(Error (id,error)))))

let addStory (stories: Stories) id (newStory:HackernewsItem) =
  // printfn "Added story: %A" newStory
  match stories with
  | HasNotStartedYet -> stories
  | InProgress -> stories
  | Resolved x ->
    match x with
    | Error y -> stories
    | Ok y -> Resolved( Ok (y.Remove(id).Add(id, Resolved(Ok newStory))))


let update (msg: Msg) (state: State) =
  match msg with 
  | LoadStoryIds Started ->
    let (StoryLimit storyLimit) = state.StoryLimit
    state, Cmd.fromAsync (loadStoryIds storyLimit)
  | LoadStoryIds (Finished(Error error)) -> {state with Stories = Resolved (Error error)}, Cmd.none
  | LoadStoryIds (Finished(Ok ids)) ->
      let z = ids |> List.map (fun i -> i, HasNotStartedYet) |> Map.ofList
      let y = ids |> List.map (LoadItem >> Cmd.ofMsg) |> Seq.ofList 
      printfn "Batch command: %A" y 
      { state with Stories = (Resolved(Ok z)) }, Cmd.batch y

  | LoadItem i -> state, Cmd.fromAsync (loadStoryItem i)
  | LoadStoryItem (Finished(Ok (id, item))) ->
    printfn "Finished command: %i" id
    { state with Stories = (addStory state.Stories id item) }, Cmd.none
        
  | LoadStoryItem (Finished (Error (id, error))) ->
    { state with Stories =(addStoryError state.Stories id error) }, Cmd.none

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
  | Resolved (Error (id, errorMsg)) -> renderItemError id errorMsg
  | Resolved (Ok item) -> renderItemDom id (Some item)


let renderStories (deferred: Stories) = 
  match deferred with
  | HasNotStartedYet -> Html.none
  | InProgress -> spinner
  | Resolved (Error errorMsg) -> Html.none
  | Resolved (Ok items) -> 
      Html.fragment [ for KeyValue(k,v) in items -> renderItem k v ]

let render (state: State) (dispatch: Msg -> unit) =
  // printfn "render: %A" state
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