(* [state] represents the state of an adventure. *)
(* You may define [state] to be whatever type you wish here. *)
open Yojson.Basic.Util
open Command

type description = {
  requires: string list;
  text: string;
}

type exit = {
  direction: string;
  room_id: string;
  keys: string list;
}

type room = {
  id: string;
  descriptions: description list;
  points: int;
  exits: exit list;
  treasure: string list;
}

type item = {
  id: string;
  description: string;
  points: int;
}

type location = {
  room: string;
  item: string;
}

type game = {
    rooms : room list;
    items : item list;
    start_room : string;
    start_inv: string list;
    start_locations: location list;
    win_message: string;
  }

let an_item j = {
  id = j |> member "id" |> to_string;
  description = j |> member "description" |> to_string;
  points = j |> member "points" |> to_int;
}

let a_location j = {
  room = j |> member "room" |> to_string;
  item = j |> member "item" |> to_string;
}

let a_description j = {
  requires = j |> member "requires" |> to_list |> List.map to_string;
  text = j |> member "text" |> to_string;
}

let an_exit j = {
  direction = j |> member "direction" |> to_string;
  room_id = j |> member "room_id" |> to_string;
  keys = j |> member "keys" |> to_list |> List.map to_string;
}

let a_room j = {
  id = j |> member "id" |> to_string;
  descriptions = j |> member "descriptions" |> to_list |> List.map a_description;
  points = j |> member "points" |> to_int;
  exits = j |> member "exits" |> to_list |> List.map an_exit;
  treasure = j |> member "treasure" |> to_list |> List.map to_string;
}

let game' j = {
  rooms = j |> member "rooms" |> to_list |> List.map a_room;
  items = j |> member "items" |> to_list |> List.map an_item;
  start_room = j |> member "start_room" |> to_string;
  start_inv = j |> member "start_inv" |> to_list |> List.map to_string;
  start_locations = j |> member "start_locations" |> to_list |> List.map a_location;
  win_message = j |> member "win_message" |> to_string;
}

let setup_game j =
  try game' j
  (*try init_state j*)
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)


type state = {
  current_room: room;
  inventory: item list;
  points: int;
  turns: int;
  rooms_visited: string list;
  item_locations: location list;
  game: game;
}

let rec find_room (roomList: room list) (fID:string) =
  match roomList with
  | [] -> failwith "Error: Room Not Found"
  | h::t -> if h.id = fID then h else (find_room t fID)

let rec find_item (itemList: item list) (iID:string) =
  match itemList with
  | [] -> failwith "Error: Item Not Found"
  | h::t -> if h.id = iID then h else (find_item t iID)

let rec delete_item_from_locs (locList: location list) (iString: string) =
  match locList with
  | [] -> locList
  | h::t -> if h.item = iString then delete_item_from_locs t iString
    else h :: (delete_item_from_locs t iString)

let rec make_inventory (itemList: item list) (inputList: string list) finalList =
  match itemList with
  |[] -> finalList
  | h::t -> if List.mem h.id inputList then make_inventory t inputList (h::finalList)
    else make_inventory t inputList finalList

let rec location_to_string (locationList: location list) stringByStringList =
  match locationList with
  | [] -> stringByStringList
  | h::t -> location_to_string t ((h.room, h.item) :: stringByStringList)

let rec inv_to_string (inventoryList: item list) stringList =
  match inventoryList with
  | [] -> stringList
  | h::t -> inv_to_string t (h.id :: stringList)

let rec item_in_treasure_room (roomList: room list) (i: location) =
  match roomList with
  | [] -> false
  | h::t -> if (List.mem i.item h.treasure && i.room = h.id) then true
    else item_in_treasure_room t i

let rec an_items_points (allItemsList: item list) (iString: string) =
  match allItemsList with
  | [] -> failwith "Error: Item Points Not Found"
  | h::t -> if iString = h.id then h.points else an_items_points t iString

let rec start_points j (roomList: room list) (itemList: location list) (totalPoints:int) =
  match itemList with
  | [] -> totalPoints
  | h::t -> if item_in_treasure_room roomList h then
      start_points j roomList t ((an_items_points (j |> member "items" |> to_list |> List.map an_item) h.item)
                               + totalPoints) else
      start_points j roomList t (totalPoints)

let init_state j = {
  current_room = find_room (j |> member "rooms" |> to_list |> List.map a_room)
      (j |> member "start_room" |> to_string);
  inventory = make_inventory (j |> member "items" |> to_list |> List.map an_item)
      (j |> member "start_inv" |> to_list |> List.map to_string) [];
  points =  start_points j (game' j).rooms
      (j |> member "start_locations" |> to_list |> List.map a_location)
      (find_room (j |> member "rooms" |> to_list |> List.map a_room)
                         (j |> member "start_room" |> to_string)).points;
  turns = 0;
  rooms_visited = (j |> member "start_room" |> to_string)::[];
  item_locations = j |> member "start_locations" |> to_list |> List.map a_location;
  game = game' j;
}

let rec sum_of_all_rooms (roomList: room list) (sum: int) =
  match roomList with
  | [] -> sum
  | h::t -> sum_of_all_rooms t (h.points + sum)

let rec sum_of_all_items (itemList: item list) (sum: int) =
  match itemList with
  | [] -> sum
  | h::t -> sum_of_all_items t (h.points + sum)

let win_score s =
  let sumOfRooms = sum_of_all_rooms s.game.rooms 0 in
  let sumOfItems = sum_of_all_items s.game.items 0 in
  sumOfRooms + sumOfItems

let score s = s.points

let turns s = s.turns

let current_room_id s = s.current_room.id

let inv s = inv_to_string s.inventory []

let visited s = s.rooms_visited

let locations s = location_to_string s.item_locations []

let make_state (croom: room) (inv: item list) (pts: int) (trns: int)
    (rvisited: string list) (ilocs: location list) (g: game) = {
  current_room = croom;
  inventory = inv;
  points = pts;
  turns = trns;
  rooms_visited = rvisited;
  item_locations = ilocs;
  game = g;
}

let rec valid_direction (exitList: exit list) (d: string) =
  match exitList with
  | [] -> false
  | h::t -> if h.direction = d then true else valid_direction t d

let rec valid_room_from_current (exitList: exit list) (rString: string) =
  match exitList with
  | [] -> false
  | h::t -> if h.room_id = rString then true else valid_room_from_current t rString

let rec get_room_from_exit (exitList: exit list) (d: string) =
  match exitList with
  | [] -> failwith "Error: Room not found in exit"
  | h::t -> if h.direction = d then h.room_id else get_room_from_exit t d

let rec get_exit_from_room (exitList: exit list) (rString: string) =
  match exitList with
  | [] -> failwith "Error: Exit not found from room"
  | h::t -> if h.room_id = rString then h else get_exit_from_room t rString

let rec key_not_in_inv (key:string) (inv: item list) =
  match inv with
  | [] -> true
  | h::t -> if h.id = key then false else key_not_in_inv key t

let rec key_not_in_locations (key:string) (locs: location list) (currentRoom: room) =
  match locs with
  | [] -> true
  | h::t -> if h.item = key && h.room = currentRoom.id then false else key_not_in_locations key t currentRoom

let rec requirements_fulfilled_for_room (keyList: string list) (rString: string) (inv: item list) (locs: location list) (currentRoom: room)=
  match keyList with
  | [] -> true
  | h::t -> if ((key_not_in_inv h inv) && (key_not_in_locations h locs currentRoom)) then
      false else (requirements_fulfilled_for_room t rString inv locs currentRoom)

let rec item_in_current_room (itemName: string) (r: room) (locList: location list) =
  match locList with
  | [] -> false
  | h::t -> if r.id = h.room && itemName = h.item then true
    else item_in_current_room itemName r t

let rec item_in_inventory (itemName: string) (inv: item list) =
  match inv with
  | [] -> false
  | h::t -> if h.id = itemName then true
    else item_in_inventory itemName t

let rec delete_item_from_inv (inv: item list) (iString: string) =
  match inv with
  | [] -> inv
  | h::t -> if h.id = iString then delete_item_from_inv t iString
    else h :: (delete_item_from_inv t iString)

let rec room_description_helper (s:state) (roomDescs: description list) =
  match roomDescs with
  | [] -> failwith "Error: Invalid json description format"
  | h::t -> if requirements_fulfilled_for_room h.requires s.current_room.id s.inventory s.item_locations s.current_room
    then h.text else room_description_helper s t

let room_description s =
  room_description_helper s s.current_room.descriptions

let rec items_in_room_helper (s:state) (itemLocations: location list) (finalItemList: string list) =
  match itemLocations with
  | [] -> if List.length finalItemList =0 then "No items in this location"::[]
                                                 else finalItemList
  | h::t -> if h.room = s.current_room.id then
      items_in_room_helper s t (h.item :: finalItemList) else
      items_in_room_helper s t finalItemList

let items_in_room s =
  items_in_room_helper s s.item_locations []

let win_message_string s =
  s.game.win_message

let do' c (st: state) =
  (
    if c.com = "go" && valid_direction st.current_room.exits c.word &&
      requirements_fulfilled_for_room (get_exit_from_room st.current_room.exits
                        (get_room_from_exit st.current_room.exits c.word)).keys
        (get_room_from_exit st.current_room.exits c.word) (st.inventory)
        (st.item_locations) (st.current_room)
   then
      let newRoomID = get_room_from_exit st.current_room.exits c.word in
      make_state (find_room st.game.rooms newRoomID) (st.inventory)
        (if List.mem newRoomID (st.rooms_visited) then st.points else
           st.points + (find_room st.game.rooms newRoomID).points) (st.turns + 1)
      (newRoomID::st.rooms_visited) (st.item_locations) (st.game)

   else if c.com = "go" && valid_room_from_current st.current_room.exits c.word &&
      requirements_fulfilled_for_room (get_exit_from_room st.current_room.exits
                        c.word).keys
        (c.word) (st.inventory)
        (st.item_locations) (st.current_room)
   then
      make_state (find_room st.game.rooms c.word) (st.inventory)
        (if List.mem c.word (visited st) then st.points else
           st.points + (find_room st.game.rooms c.word).points) (st.turns + 1)
        (c.word::st.rooms_visited) (st.item_locations) (st.game)

  else if c.com = "take" && item_in_current_room c.word st.current_room st.item_locations then
      make_state (st.current_room)
      ((find_item st.game.items c.word) :: st.inventory)
      (if (List.mem c.word st.current_room.treasure) then
         (st.points - an_items_points (st.game.items) (c.word)) else st.points) (st.turns+1) (st.rooms_visited)
      (delete_item_from_locs st.item_locations c.word)
      (st.game)

  else if c.com = "drop" && item_in_inventory c.word st.inventory then
    make_state (st.current_room)
      (delete_item_from_inv st.inventory c.word)
      (if (List.mem c.word st.current_room.treasure) then
         (st.points + an_items_points (st.game.items) (c.word)) else st.points) (st.turns+1) (st.rooms_visited)
      ({room = st.current_room.id; item = c.word;} :: st.item_locations)
      (st.game)

  else if c.com = "quit" then st
  else if c.com = "look" then st
  else if c.com = "inventory" || c.com = "inv" then st
  else if c.com = "score" then st
  else if c.com = "turns" then st
  else st
)
