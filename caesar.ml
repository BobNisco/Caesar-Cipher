(* Bob Nisco
   Theory of Programming Languages
   Spring 2013 *)

fun shiftChar (ch : char, shftAmt : int) : char =
	case ch of #" " => #" "
	|  _ => chr (((ord (Char.toUpper ch) - 65 + shftAmt) mod 26) + 65);

fun encrypt (str : string, shftAmt : int) : string =
	String.implode(map (fn x => shiftChar (x, shftAmt)) (String.explode(str)));

fun decrypt (str : string, shftAmt : int) : string =
	encrypt (str, shftAmt * ~1);

fun solve (str, maxShftAmt) =
	map (fn x => print ("Caesar "^(Int.toString x)^": "^encrypt(str, x)^"\n")) (List.tabulate(maxShftAmt, fn x => x + 1))

(* Test cases *)
val str = "the quick brown fox jumps over the lazy dog";
val encStr = encrypt(str, 4);
decrypt("xli uymgo fvsar jsb nyqtih sziv xli pedc hsk", 4);
solve(str, 26);
