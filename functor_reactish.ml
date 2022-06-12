(*
module Comp = struct
  type t =
    { key : string option
    ; children : component list
    }

  and component = InnerText of string
end

module type Component = sig
  type props

  val get_props : unit -> props
  val render : props -> Comp.t
end

module App = struct
  type props = { name : string }

  let get_props () = { name = "My App" }
  let render props = { Comp.key = Some "key"; children = [ InnerText props.name ] }
end

module type React = sig
  val render : unit -> unit
end

module Make_component (C : Component) : React = struct
  let render () =
    C.get_props ()
    |> C.render
    |> (fun c -> c.children)
    |> List.iter (function Comp.InnerText i -> Stdio.print_endline i)
  ;;
end

module Display = Make_component (App)

let () = Display.render ()
*)

module Redux = struct
  module Subscriber : sig
    type id
    type t = id

    val register : unit -> id
    val compare : id -> id -> int
    val id_to_string : id -> string
  end = struct
    type id = int
    type t = id

    let compare = Int.compare
    let subscriber_ids = ref 0

    let register () =
      incr subscriber_ids;
      !subscriber_ids
    ;;

    let id_to_string id = Int.to_string id
  end

  module type Behavior = sig
    type action
    type state

    val reducer : state -> action -> state
    val initial_state : state
    val to_string : state -> string
  end

  type ('state, 'action) reducer = 'state -> 'action -> 'state

  module type Store = sig
    type action
    type state
    type state_getter = unit -> state

    val get_state : state_getter
    val subscribe : (state -> unit) -> Subscriber.id
    val unsubscribe : Subscriber.id -> bool

    type dispatcher = operation -> unit
    and thunk = dispatch:dispatcher -> get_state:state_getter -> unit

    and operation =
      | Action of action
      | Thunk of thunk

    val dispatch : dispatcher
  end

  module Make_store (M : Behavior) :
    Store with type state = M.state and type action = M.action = struct
    type state = M.state
    type action = M.action
    type state_getter = unit -> state

    type dispatcher = operation -> unit
    and thunk = dispatch:dispatcher -> get_state:state_getter -> unit

    and operation =
      | Action of action
      | Thunk of thunk

    let state = ref M.initial_state
    let get_state () = !state

    module Subscriber_map = Map.Make (Subscriber)

    let subscribers = ref Subscriber_map.empty

    let subscribe t =
      let id = Subscriber.register () in
      subscribers := Subscriber_map.add id t !subscribers;
      id
    ;;

    let unsubscribe id =
      let subs = !subscribers in
      match Subscriber_map.find_opt id subs with
      | Some _ ->
        subscribers := Subscriber_map.remove id subs;
        true
      | None -> false
    ;;

    let rec reduce_with_thunks state = function
      | Action action ->
        Stdio.print_endline "an action";
        M.reducer state action
      | Thunk thunk ->
        Stdio.print_endline "a thunk";
        let () = thunk ~get_state ~dispatch in
        state

    and dispatch operation =
      Stdio.print_endline "Got Update";
      let prev_state = !state in
      Stdio.print_endline ("Got state " ^ M.to_string prev_state);
      let next_state = reduce_with_thunks prev_state operation in
      Stdio.print_endline ("Next state is state " ^ M.to_string next_state);
      state := next_state;
      Subscriber_map.iter (fun _ sub -> sub next_state) !subscribers
    ;;
  end

  (* Maybe with a GDAT? *)
  (*module type Split_store = sig*)
  (*end*)
end

type _ component =
  | String : string -> _ component
  | Component : 'props 'otherprops. 'props node -> 'otherprops component

and 'props node =
  { make_fn : 'props -> 'props component
  ; props : 'props
  ; children : 'props. 'props component list
  }

module Fragment = struct
  let make () = String ""
end

module Box = struct
  type props = { name : string }

  let make props =
    let new_name = props.name ^ " in box" in
    Component { make_fn = Fragment.make; props = (); children = [ String new_name ] }
  ;;
end

module App = struct
  let make () = Component { make_fn = Box.make; props = { name = "dick" }; children = [] }
end

let rec string_renderer : type a. a component -> string = function
  | String s -> s
  | Component { make_fn; props; children } ->
    children
    |> List.map string_renderer
    |> String.concat "|"
    |> fun children -> string_renderer (make_fn props) ^ "[children= " ^ children ^ "]"
;;

let () = App.make () |> string_renderer |> Stdio.print_endline

module DebugState = struct
  type state = bool

  type action =
    [ `On
    | `Off
    ]

  let initial_state = false

  let reducer state = function
    | `On -> true
    | `Off -> false
    | _ -> state
  ;;

  let to_string state = string_of_bool state
end

module UserState = struct
  type occupation =
    | Programmer
    | Lawyer

  type state =
    { name : string
    ; age : int
    ; occupation : occupation
    }

  type action =
    [ `UpdateName of string
    | `UpdateAge of int
    | `Reset of state
    ]

  let initial_state = { name = "Patrick"; age = 35; occupation = Lawyer }

  let reducer state = function
    | `UpdateAge age -> { state with age }
    | `UpdateName name -> { state with name }
    | `Reset state -> state
    | _ -> state
  ;;

  let string_of_occupation = function
    | Lawyer -> "Lawyer"
    | Programmer -> "Programmer"
  ;;

  let to_string state =
    Printf.sprintf
      "{ name = %s ; age = %d ; occupation = %s }\n"
      state.name
      state.age
      (string_of_occupation state.occupation)
  ;;
end

module State = struct
  type state =
    { user_state : UserState.state
    ; debug_state : DebugState.state
    }

  type action =
    [ UserState.action
    | DebugState.action
    ]

  let reducer state action =
    { user_state = UserState.reducer state.user_state action
    ; debug_state = DebugState.reducer state.debug_state action
    }
  ;;

  let initial_state =
    { user_state = UserState.initial_state; debug_state = DebugState.initial_state }
  ;;

  let to_string state =
    Printf.sprintf
      "{\n\tuser_state = %s\tdebug_state = %s\n}\n"
      (UserState.to_string state.user_state)
      (DebugState.to_string state.debug_state)
  ;;
end

module Store = Redux.Make_store (State)

let () =
  let _ =
    Store.subscribe (fun state -> state |> State.to_string |> Stdio.print_endline)
  in
  Stdio.print_endline "Starting";
  Store.get_state () |> State.to_string |> Stdio.print_endline;
  let lazy_age_update ~dispatch ~get_state:_ =
    ignore
      (Lwt.bind (Lwt_unix.sleep 2.) (fun _ ->
           Random.self_init ();
           if Random.float 1. > 0.5
           then (
             let () = dispatch (Store.Action (`UpdateAge 100)) in
             Lwt.return_unit)
           else (
             let () = dispatch (Store.Action (`UpdateAge 1)) in
             Lwt.return_unit)))
  in
  let _ = Store.dispatch (Thunk lazy_age_update) in
  let _ = Store.dispatch (Action (`UpdateName "fart")) in
  let () = Lwt_main.run (Lwt_unix.sleep 10.) in
  ()
;;
