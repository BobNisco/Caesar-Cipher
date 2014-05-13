/**
 * Caesar Cipher Scala Class and CaesarTest object for testing the Caesar class
 *
 * @author Bob Nisco
 * Theory of Programming Languages
 * Spring 2013
 */
class Caesar {

	// Caesar will have an attribute of str
	var str = ""

	/**
	 * @param shftAmt the amount of characters that str should be shifted
	 * @return an encryptd string based on the given shftAmt
	 */
	def encrypt(shftAmt: Int): String = {
		val newShftAmt = shftAmt % 26
		val charArray = this.str.toUpperCase().toCharArray()
		var ret = ""

		for (i<-charArray) {
			if (i.toInt == 32) {
				ret += " ";
			} else {
				var temp = (i.toInt - 65 + newShftAmt) % 26
				if (temp < 0) {
					temp += 26
				}
				ret += (temp + 65).toChar
			}
		}

		return ret
	}

	/**
	 * @param shftAmt the amount of characters that str should be shifted
	 * @return a decryptd string based on the given shftAmt
	 */
	def decrypt(shftAmt: Int): String = {
		return encrypt(shftAmt * - 1)
	}

	/**
	 * @param maxShftAmt the maximum amount of different combinations of the decrypt
	 *        function that should be iterated over.
	 * @return void, but it will print out the decrypt functions from 0 to maxShftAmt
	 */
	def solve(maxShftAmt: Int) {
		for (i <- 0 to maxShftAmt) {
			println("Caesar " + i + ": " + encrypt(i))
		}
	}
}

object CaesarTest {
	/**
	 * @param args an array of String arguments
	 */
	def main(args: Array[String]) {
		var caesar = new Caesar()
		caesar.str = "FATURE RAM IS OTILL I"
		caesar.str = caesar.encrypt(4)
		println(caesar.str)
		caesar.str = caesar.decrypt(4)
		println(caesar.str)
		println(caesar.solve(26))
	}
}
