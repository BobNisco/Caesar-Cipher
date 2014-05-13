	   >>SOURCE FORMAT IS FREE
*> Bob Nisco
*> Theory of Programming Languages
*> Spring 2013

IDENTIFICATION DIVISION.
PROGRAM-ID. CAESAR.
DATA DIVISION.
LOCAL-STORAGE SECTION.
01 Str     PIC X(99) VALUE "The quick brown fox jumped over the lazy dog".
01 ShftAmt PIC 99    VALUE 4.
PROCEDURE DIVISION.
	CALL 'ENCRYPT' USING
	BY CONTENT Str ShftAmt
	SET Str TO "XLI UYMGO FVSAR JSB NYQTIH SZIV XLI PEDC HSK"
	CALL 'DECRYPT' USING
	BY CONTENT Str ShftAmt
	SET Str TO "The quick brown fox jumped over the lazy dog"
	SET ShftAmt TO 26
	CALL 'SOLVE' USING
	BY CONTENT Str ShftAmt
	STOP RUN.


IDENTIFICATION DIVISION.
PROGRAM-ID. ENCRYPT.
DATA DIVISION.
LOCAL-STORAGE SECTION.
01 CurShft  PIC 99    VALUE 1.
01 StrLen   PIC 99.
01 OutStr   PIC X(99).
01 TempChar PIC 99.
01 CHR      PIC X.
01 ASC      REDEFINES CHR PIC 99 COMP-X.
01 TempStrLen PIC 99 VALUE 0.
LINKAGE SECTION.
01 Str      PIC X(99).
01 ShftAmt  PIC 99.
PROCEDURE DIVISION USING Str ShftAmt.
	MAIN.
		*> Uppercase the string
		SET Str TO FUNCTION UPPER-CASE(Str)
		*> If you just do length(Str), you'll get 99, as defined above
		*> Instead, we need to count the leading spaces in the reversed string
		*> and then subtract that from the length(Str)
		INSPECT FUNCTION REVERSE(Str) TALLYING StrLen FOR LEADING SPACES
		COMPUTE StrLen = LENGTH OF Str - StrLen
		*> Apparently, COBOL's strings start at position 1, not 0.
		ADD 1 to StrLen
		PERFORM SHIFT-CHARS UNTIL CurShft = StrLen
		DISPLAY "", OutStr
		EXIT PROGRAM.
	SHIFT-CHARS.
		SET TempStrLen TO CurShft
		SET TempChar TO FUNCTION ORD(Str(CurShft:1))
		IF TempStrLen IS = 0
			SET TempStrLen TO 1.
		IF TempChar IS = 33
			MOVE 32 TO ASC
		ELSE
			*> I would have subtracted 65, but ORD gives a value 1 greater than expected
			SUBTRACT 66 FROM TempChar
			ADD ShftAmt TO TempChar
			COMPUTE TempChar = FUNCTION MOD (TempChar, 26)
			ADD 65 TO TempChar
			MOVE TempChar TO ASC.
		STRING CHR DELIMITED BY SPACES
		INTO OutStr
		WITH POINTER TempStrLen
		END-STRING
		ADD 1 TO CurShft.


IDENTIFICATION DIVISION.
PROGRAM-ID. DECRYPT.
DATA DIVISION.
LINKAGE SECTION.
01 Str     PIC X(99).
01 ShftAmt PIC 99.
PROCEDURE DIVISION USING Str ShftAmt.
	SUBTRACT ShftAmt FROM 26 GIVING ShftAmt
	CALL 'ENCRYPT' USING
	BY CONTENT Str ShftAmt
	EXIT PROGRAM.


IDENTIFICATION DIVISION.
PROGRAM-ID. SOLVE.
DATA DIVISION.
LOCAL-STORAGE SECTION.
01 CurShft PIC 99 VALUE 0.
LINKAGE SECTION.
01 Str        PIC X(99).
01 MaxShftAmt PIC 99.
PROCEDURE DIVISION USING Str MaxShftAmt.
	MAIN.
		PERFORM SOLVE-LOOP UNTIL CurShft = MaxShftAmt + 1
		EXIT PROGRAM.
	SOLVE-LOOP.
		DISPLAY "Caesar ", CurShft, ": "
		CALL 'DECRYPT' USING
		BY CONTENT Str CurShft
		ADD 1 TO CurShft.
