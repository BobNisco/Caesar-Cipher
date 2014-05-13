! Bob Nisco
! Theory of Programming Languages
! Spring 2013

program Caesar
	implicit none

	! Declare some variables
	character(45) :: str = "The quick brown fox jumped over the lazy dog"
	integer, parameter :: shftAmt = 4

	! Make str all upper case
	call toUpper(str)
	call encrypt(str, shftAmt)
	write(*, *) str
	call decrypt(str, shftAmt)
	write(*, *) str
	call solve(str, 26)
	contains

	subroutine encrypt(str, shftAmt)
		character(*), intent(inout) :: str
		integer, intent(in) :: shftAmt
		integer :: i

		do i = 1, len(str)
			if (iachar(str(i:i)) == 32) then
				str(i:i) = achar(32)
			else
				str(i:i) = achar(modulo(iachar(str(i:i)) - 65 + shftAmt, 26) + 65)
			endif
		end do

	end subroutine encrypt

	subroutine decrypt(str, shftAmt)
		character(*), intent(inout) :: str
		integer, intent(in) :: shftAmt
		integer newShftAmt

		! Decrypt is really encrypt inverse
		newShftAmt = shftAmt * -1
		call encrypt(str, newShftAmt)
	end subroutine decrypt

	subroutine solve(str, maxShftAmt)
		character(*), intent(inout) :: str
		integer, intent(in) :: maxShftAmt
		character(len=len(str)) :: strCopy
		integer i

		do i = 0, maxShftAmt
			strCopy = str
			call encrypt(strCopy, i)
			write(*, *) "Caesar ", i, ": ", strCopy
		end do
	end subroutine

	subroutine toUpper(str)
		implicit none
		character(*), intent(inout) :: str
		integer i
		integer j

		do i = 1, len(str)
			j = iachar(str(i:i))
			if (j>= iachar("a") .and. j<=iachar("z")) then
				str(i:i) = achar(iachar(str(i:i)) - 32)
			else
				str(i:i) = str(i:i)
			end if
		end do
	end subroutine toUpper

end program Caesar
