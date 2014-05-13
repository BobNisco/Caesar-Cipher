{ Bob Nisco
  Theory of Programming Languages
  Spring 2013 }
program Caesar;
{ A variable for the string we'll be using }
var str: String;

function encrypt(str: String; shftAmt: Integer): String;
var
	i, temp: Integer;
	upperStr: String;
begin
	upperStr := UpCase(str);
	for i := 1 to length(upperStr) do
	begin
		{ Assign it to a temp integer because mod expects an integer }
		temp := (byte(upperStr[i]) - 65 + shftAmt) mod integer(26);
		if (temp < 0) then
			begin
				temp := temp + 26;
			end;
		temp := temp + 65;
		{ Reassign the current letter with the shifted letter }
		if (byte(upperStr[i]) = 32) then
			begin
				temp := 32;
			end;
		upperStr[i] := chr(temp);

	end;
	encrypt := upperStr;
end;

function decrypt(str: String; shftAmt: Integer): String;
begin
	decrypt := encrypt(str, shftAmt * -1);
end;

procedure solve(str: String; maxShftAmt: Integer);
var
	i: Integer;
begin
	for i := 0 to maxShftAmt do
	begin
		Writeln('Caesar ', i, ': ', encrypt(str, i));
	end;
end;

{ A quick test of the functions }
begin
	str := 'the quick brown fox jumped over the lazy dog';
	str := encrypt(str, 4);
	Writeln(str);
	str := decrypt(str, 4);
	Writeln(str);
	solve(str, 26);
end.
