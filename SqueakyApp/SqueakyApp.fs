/// Sample memory game
///
/// LiveUpdate on Android:
///    adb -d forward  tcp:9867 tcp:9867
///    C:\Apps\SqueakyApp\packages\Elmish.XamarinForms.LiveUpdate.0.17.0\tools\fscd.exe   --watch --webhook:http://localhost:9867/update

namespace SqueakyApp

open System
open Elmish.XamarinForms
open Elmish.XamarinForms.DynamicViews
open Xamarin.Forms

module App = 

    type Position = (int * int)

    /// The model of the application state
    type Model = 
      { 
        Count : int
        Revealed: Set<Position>
        FirstChoice: Position option
        Solution : Map<Position, int> 
        StartTime: DateTime
      }

    /// Messages that trigger updates to the application state
    type Msg = 
        | Increment 
        | Decrement 
        | Reveal of Position
        | Hide of Position * Position
        | Reset
        | NewGame
        | GameOver

    /// The random number generator
    let rnd = System.Random()

    /// Helper to dispatch a message after 'pause' milliseconds
    let timed pause msg = 
        async { do! Async.Sleep pause
                return msg }
        |> Cmd.ofAsyncMsg

    /// The initial model for a given size.
    let initModel count = 
         let randomNumbers = Seq.initInfinite (fun _ -> (rnd.Next(count),rnd.Next(count))) 
         let firstPairs = randomNumbers |> Seq.distinct |> Seq.take (count*count/2) |> Seq.toList
         let secondPairs =  Seq.allPairs [ 0 .. count-1 ] [ 0 .. count-1 ]  |> Seq.except firstPairs |> Seq.toList
         let swap (a,b) = (b,a)
         let solution = Map.ofList ((firstPairs |> List.indexed |> List.map swap) @ (secondPairs |> List.indexed |> List.map swap))
         { Count = count
           Revealed = Set.empty
           FirstChoice = None
           Solution = solution 
           StartTime = System.DateTime.Now 
         }
          
    /// The initial model
    let init () = (initModel 2, Cmd.none)

    /// Update the model given the message, optionally triggering a command
    let update msg model =
        match msg with
        | Increment -> initModel (model.Count + 2), Cmd.none

        | Decrement -> initModel (model.Count - 2), Cmd.none

        | Reveal pos -> 
            // Check if we already have a first choice
            match model.FirstChoice with 
            | None -> 
                { model with 
                    Revealed = model.Revealed.Add pos
                    FirstChoice = Some pos }, Cmd.none
            | Some fc -> 
                if model.Solution.[fc] = model.Solution.[pos] then  
                     let revealed = model.Revealed.Add pos
                     let isEndOfGame = (Set.count revealed = (model.Count * model.Count))
                     let cmd = if isEndOfGame then timed 100 GameOver else Cmd.none
                     { model with Revealed = revealed; FirstChoice=None }, cmd
                else
                     // Reveal the second choice, then hide the pair after 200 milliseconds
                     { model with Revealed = model.Revealed.Add pos; FirstChoice=None }, 
                     timed 200 (Hide (fc, pos))

        | Hide (pos1, pos2) -> 
             { model with Revealed = model.Revealed.Remove(pos1).Remove(pos2) }, Cmd.none

        | GameOver -> 
            let text = sprintf "You've won in %0.2f seconds!" (DateTime.op_Subtraction(DateTime.Now, model.StartTime)).TotalSeconds
            Application.Current.MainPage.DisplayAlert("Game over!", text, "OK") |> ignore
            model, Cmd.none
            
        | Reset -> initModel 2, Cmd.none

        | NewGame -> initModel model.Count, Cmd.none

    // A helper to make the view elements for a rows * cols grid
    let makeGrid rows cols (f : (int * int) -> ViewElement) = 
        View.Grid(rowdefs=[ for i in 0 .. rows - 1 -> box "auto" ], 
            coldefs = [ for i in 0 .. cols - 1 -> box "auto" ],
            children=[ for i in 0 .. rows - 1 do 
                            for j in 0 .. cols - 1   do 
                               yield (f (i,j)).GridRow(i).GridColumn(j) ],
            horizontalOptions = LayoutOptions.Center,
            verticalOptions= LayoutOptions.Center)
             
    // Grab the square images
    let imgs = 
        let imageAssembly = AppDomain.CurrentDomain.Load("SqueakyApp") 
        [| for i in 0 .. 7  -> 
             let imageResource = sprintf "SqueakyApp.sq%d.jpg" (i+1)
             ImageSource.FromResource(imageResource, imageAssembly) |]

    /// Generate the view elements for the model
    let view (model: Model) dispatch =
        View.ContentPage(
          content = View.StackLayout(padding = 20.0, verticalOptions = LayoutOptions.Center,
            children = [ 
                View.Label(text = sprintf "%d" model.Count, horizontalOptions = LayoutOptions.Center, fontSize = "Large")
                 
                View.Button(text = "Bigger", 
                            command = (fun () -> dispatch Increment), 
                            horizontalOptions = LayoutOptions.Center
                            , canExecute = (model.Count < 6)
                            )
                
                View.Button(text = "Smaller", 
                            command = (fun () -> dispatch Decrement), 
                            horizontalOptions = LayoutOptions.Center 
                            , canExecute = (model.Count > 2)
                            )

                View.Button(text = "Reset", 
                            command = (fun () -> dispatch Reset), 
                            horizontalOptions = LayoutOptions.Center)

                View.Button(text = "New Game", 
                            command = (fun () -> dispatch NewGame), 
                            horizontalOptions = LayoutOptions.Center)

                makeGrid model.Count model.Count (fun pos -> 
                        if model.Revealed.Contains pos then  
                            View.Image(source= imgs.[model.Solution.[pos]], 
                                       widthRequest=80.0, heightRequest=80.0, 
                                       aspect = Aspect.AspectFit)
                            //View.BoxView (color=Color.Gray, widthRequest=80.0, heightRequest=80.0)
                        else
                            View.BoxView(color=Color.Gray, widthRequest=80.0, heightRequest=80.0, 
                                 gestureRecognizers = [ 
                                     View.TapGestureRecognizer(command=(fun () -> dispatch (Reveal pos) )) ] )
                            )
            ]))

    /// Bind the init, update and view functions together
    let program = Program.mkProgram init update view


/// Make a Xamarin.Forms Application
type App () as app = 
    inherit Application ()

    let runner = 
        App.program
        |> Program.runWithDynamicView app

    do runner.EnableLiveUpdate()



    