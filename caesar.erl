%% @author Bob Nisco
%% @title Caesar Cipher
%% Theory of Programming Languages
%% Spring 2013

-module(caesar).
-export([encrypt/2, decrypt/2, solve/2, main/0]).

shiftChar(Char, ShftAmt) when (Char /= " ") ->
	% A small function to give the ASCII value of a character
	ToAscii = fun([Character]) -> Character end,
	% Shift the char value by ShftAmt
	% Account for negatives by adding 26 then rem'ing again
	(((ToAscii(Char) - 65 + ShftAmt) rem (26) + 26) rem 26) + 65;
shiftChar(Char, _ShftAmt) ->
	% A default case when Char is a space
	Char.

encrypt(Str, ShftAmt) ->
	% Apply the shiftChar function with the uppercased character to the string
	Shifted = lists:map(fun(Char) -> shiftChar([string:to_upper(Char)], ShftAmt) end, Str),
	io:format("~s~n", [Shifted]).

decrypt(Str, ShftAmt) ->
	encrypt(Str, ShftAmt * -1).

solve(Str, MaxShftAmt) ->
	% Create a list of numbers ranging from 0 to MaxShftAmt
	ListOfNums = lists:seq(0, MaxShftAmt),
	% Iterate over the list of numbers with an anonymous functions
	% that calls decrypt for each number in the sequence.
	lists:foreach(fun(ShftAmt) ->
		io:fwrite("Caesar ~p: ", [ShftAmt]),
		decrypt(Str, ShftAmt * -1) end, ListOfNums).

main() ->
	Str = "the quick brown fox jumped over the lazy dog",
	encrypt(Str, 4),
	decrypt("xli uymgo fvsar jsb nyqtih sziv xli pedc hsk", 4),
	solve(Str, 26).
