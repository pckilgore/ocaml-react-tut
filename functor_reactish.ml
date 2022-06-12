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
  end = struct
    type id = int
    type t = id

    let compare = Int.compare
    let subscriber_ids = ref 0

    let register () =
      incr subscriber_ids;
      !subscriber_ids
    ;;
  end

  module type Behavior = sig
    type action
    type state

    val reducer : state -> action -> state
    val initial_state : state
  end

  module type Store = sig
    type action
    type state

    val get_state : unit -> state
    val subscribe : (state -> unit) -> Subscriber.id
    val unsubscribe : Subscriber.id -> bool
    val dispatch : action -> state
  end

  module Make_store (M : Behavior) :
    Store with type state = M.state and type action = M.action = struct
    type state = M.state
    type action = M.action

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

    let dispatch action =
      let state = !state in
      let next_state = M.reducer state action in
      Subscriber_map.iter (fun _ v -> v next_state) !subscribers;
      next_state
    ;;
  end
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
