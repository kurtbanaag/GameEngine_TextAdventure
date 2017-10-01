open OUnit2
open State
open Command

let j = Yojson.Basic.from_file "threerooms.json"
let j2 = Yojson.Basic.from_file "oneroom.json"
let gotj = Yojson.Basic.from_file "GameOfThronesAdventure.json"
let quitCommand = parse "quit"
let lookCommand = parse "look"
let invCommand = parse "inv"
let takeCommand = parse "take black hat"
let dropCommand = parse "drop white hat"
let scoreCommand = parse "score"
let turnsCommand = parse "turns"
let goCommand = parse "go room2"
let invalidCommand = parse "take room2"
let invalidCommand2 = parse "ergopo4pj4rpoj24 24pjr42pi4"




let tests =
[
  "1: max" >:: (fun _ -> assert_equal 11111 (j |> init_state |> win_score));
  "1: start_score" >:: (fun _ -> assert_equal 10001 (j |> init_state |> score));
  "1: start_turns" >:: (fun _ -> assert_equal 0 (j |> init_state |> turns));
  "1: start_room" >:: (fun _ -> assert_equal "room1" (j |> init_state |> current_room_id));
  "1: start_inv" >:: (fun _ -> assert_equal ["white hat"] (j |> init_state |> inv));
  "1: rooms_visited" >:: (fun _ -> assert_equal ["room1"] (j |> init_state |> visited));
  "1: start_locations" >:: (fun _ -> assert_equal [("room2", "key");
                                                ("room1", "red hat");
                                                ("room1", "black hat")]
                            (j |> init_state |> locations));

  "2: max" >:: (fun _ -> assert_equal 110 (j2 |> init_state |> win_score));
  "2: start_score" >:: (fun _ -> assert_equal 110 (j2 |> init_state |> score));
  "2: start_turns" >:: (fun _ -> assert_equal 0 (j2 |> init_state |> turns));
  "2: start_room" >:: (fun _ -> assert_equal "room1" (j2 |> init_state |> current_room_id));
  "2: start_inv" >:: (fun _ -> assert_equal [] (j2 |> init_state |> inv));
  "2: rooms_visited" >:: (fun _ -> assert_equal ["room1"] (j2 |> init_state |> visited));
  "2: start_locations" >:: (fun _ -> assert_equal [("room1", "item1")] (j2 |> init_state |> locations));

  "3: max" >:: (fun _ -> assert_equal 72650 (gotj |> init_state |> win_score));
  "3: start_score" >:: (fun _ -> assert_equal 0 (gotj |> init_state |> score));
  "3: start_turns" >:: (fun _ -> assert_equal 0 (gotj |> init_state |> turns));
  "3: start_room" >:: (fun _ -> assert_equal "winterfell" (gotj |> init_state |> current_room_id));
  "3: start_inv" >:: (fun _ -> assert_equal [] (gotj |> init_state |> inv));
  "3: rooms_visited" >:: (fun _ -> assert_equal ["winterfell"] (gotj |> init_state |> visited));
  "3: start_locations" >:: (fun _ ->
      assert_equal [("kings landing", "ned stark"); ("casterly rock", "food");
                    ("casterly rock", "army"); ("wendish town", "treasure map");
                    ("iron islands", "water"); ("iron islands", "theon grayjoy");
                    ("stony shore", "gold"); ("stony shore", "warriors");
                    ("castle black", "john snow"); ("winterfell", "whitewalker scroll");
                    ("winterfell", "sword")] (gotj |> init_state |> locations));

  "4: max" >:: (fun _ -> assert_equal 110 (j2 |> init_state |> do' quitCommand |> win_score));
  "4: start_score" >:: (fun _ -> assert_equal 110 (j2 |> init_state |> do' quitCommand |> score));
  "4: start_turns" >:: (fun _ -> assert_equal 0 (j2 |> init_state |> do' quitCommand |> turns));
  "4: start_room" >:: (fun _ -> assert_equal "room1" (j2 |> init_state |> do' quitCommand |> current_room_id));
  "4: start_inv" >:: (fun _ -> assert_equal [] (j2 |> init_state |> do' quitCommand |> inv));
  "4: rooms_visited" >:: (fun _ -> assert_equal ["room1"] (j2 |> init_state |> do' quitCommand |> visited));
  "4: start_locations" >:: (fun _ -> assert_equal [("room1", "item1")] (j2 |> init_state |> do' quitCommand |> locations));

]

let suite =
  "Adventure test suite"
  >::: tests

let _ = run_test_tt_main suite
