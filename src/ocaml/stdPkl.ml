type instream = in_channel
type outstream = out_channel
exception IOError of string    
let write_tag x s = PklInt.write_int x s
let read_tag s = PklInt.read_int s
let write_list f xs s =
  begin
    write_tag (List.length xs) s;
    List.iter (fun x -> (f x s)) xs
  end
let read_list f s = 
  let rec loop x acc =
      match x with
	0 -> acc
      |	m -> loop (x-1) ((f s)::acc) in
  let n = read_tag s in  List.rev (loop n [])

let write_option f x s = 
    match x with
    None -> write_tag 0 s
    | (Some x) ->
      begin
	write_tag 1 s;
	f x s
      end
let read_option f s =
      match (read_tag s) with
	0 -> None
      | 1 -> Some (f s)
      | _ -> raise (IOError "read_option")

let read_share rd ins =
      let t = (read_tag ins) in
      let rd_key l = 
	let buff = (String.create l) in
	begin
	  really_input ins buff 0 l ;
	  buff
	end
      in
      if t < 0 then
	Share.DEFv (rd_key (-t),rd ins)
      else if t > 0 then Share.USEv (rd_key t)
      else raise (IOError "read_share")

let write_share wr x outs =
  match x with
    Share.USEv n ->
      begin
	write_tag (String.length n) outs;
	output_string outs n
      end
  | Share.DEFv (n,v) ->
      begin
	write_tag (-(String.length n)) outs;
	output_string outs n;
	wr v outs
      end

