/*
   day2 - add multiply halt
   day5 -

   rightmost two digits , say 1002 is 02

    how do we connect read / write - where does it go ?

   debugging - how easy is it to debug ?



*/

package main

import (
	//"errors"
	//"crypto/md5"
	//"encoding/hex"
	"fmt"
	"log"
	"os"
	//"sort"
	"strconv"
	"strings"
	//	"unicode/utf8"
	"math/rand/v2"
)

func parseNumbers(words []string) []int {
	var nums []int
	for _, line := range words {
		n, err := strconv.Atoi(line)
		if err != nil {
		} else {
			nums = append(nums, n)
		}
	}
	return nums
}

/*
state datatype
current instruction pointer value
the numbers - the code
steps - number of steps executed
status - ok true , bad false
*/
type State struct {
	Ip    int
	Nums  []int
	Steps int
	Ok    bool
}

func interpret(nums []int, reader func() int, writer func(int)) State {
	ip := 0
	op := 0
	steps := 0

	fmt.Printf("ip is set at %d\n", ip)
	fmt.Printf("there are %d numbers in input\n", len(nums))

	for (ip >= 0) && (ip < len(nums)) {
		fmt.Printf("ip = %d , raw op = %d \n", ip, nums[ip])
		op = parameter_opcode(nums[ip])
		if op == 99 { // halt
			fmt.Printf("halted\n")
			return State{Ip: ip, Nums: nums, Steps: steps, Ok: true}
		}

		if op == 1 { // add -- positional
			var arg1, arg2, arg3 int
			p1 := parameter_mode1(nums[ip])
			p2 := parameter_mode2(nums[ip])
			//p3 := parameter_mode3(nums[ip]) //redundant never immediate mode write
			// arg1
			if p1 == 1 { // 1 == immediate mode
				arg1 = nums[ip+1]
			} else {
				arg1 = nums[ip+1]
				arg1 = nums[arg1]
			}
			// arg2
			if p2 == 1 { // 1 == immediate mode
				arg2 = nums[ip+2]
			} else {
				arg2 = nums[ip+2]
				arg2 = nums[arg2]
			}
			arg3 = nums[ip+3]
			nums[arg3] = arg1 + arg2
			ip = ip + 4
			fmt.Printf("added %d + %d -> nums[%d]\n", arg1, arg2, arg3)
		} else if op == 2 { // mult - positional or immediate mode
			var arg1, arg2, arg3 int
			p1 := parameter_mode1(nums[ip])
			p2 := parameter_mode2(nums[ip])
			//p3 := parameter_mode3(nums[ip]) //redundant never immediate mode write
			// arg1
			if p1 == 1 { // 1 == immediate mode
				arg1 = nums[ip+1]
			} else {
				arg1 = nums[ip+1]
				arg1 = nums[arg1]
			}
			// arg2
			if p2 == 1 { // 1 == immediate mode
				arg2 = nums[ip+2]
			} else {
				arg2 = nums[ip+2]
				arg2 = nums[arg2]
			}
			arg3 = nums[ip+3]
			nums[arg3] = arg1 * arg2
			ip = ip + 4
			fmt.Printf("multiplied %d * %d -> nums[%d]\n", arg1, arg2, arg3)

		} else if op == 3 { // read a value -- and put read value into arg1 position
			var arg1 int
			arg1 = nums[ip+1]
			myread := reader()
			nums[arg1] = myread
			fmt.Printf("assumption read value %d into %d\n", myread, arg1)
			ip = ip + 2
		} else if op == 4 { // write -- where ?
			var arg1 int
			arg1 = nums[ip+1]
			arg1 = nums[arg1]
			fmt.Printf("writing value of %d\n", arg1) // require writer
			writer(arg1)
			ip = ip + 2
		} else if op == 5 { // jump if true
			var arg1, arg2 int
			p1 := parameter_mode1(nums[ip])
			p2 := parameter_mode2(nums[ip])
			// arg1
			if p1 == 1 { // 1 == immediate mode
				arg1 = nums[ip+1]
			} else {
				arg1 = nums[ip+1]
				arg1 = nums[arg1]
			}
			// arg2
			if p2 == 1 { // 1 == immediate mode
				arg2 = nums[ip+2]
			} else {
				arg2 = nums[ip+2]
				arg2 = nums[arg2]
			}
			//jump if true
			if arg1 == 0 {
				ip = ip + 3
				fmt.Printf("jmp if true : ignored jump - advanced 3 codes \n")
			} else {
				ip = arg2
				fmt.Printf("jmp if true : jumped to %d \n", ip)
			}
		} else if op == 6 { // jump if false
			var arg1, arg2 int
			p1 := parameter_mode1(nums[ip])
			p2 := parameter_mode2(nums[ip])
			// arg1
			if p1 == 1 { // 1 == immediate mode
				arg1 = nums[ip+1]
			} else {
				arg1 = nums[ip+1]
				arg1 = nums[arg1]
			}
			// arg2
			if p2 == 1 { // 1 == immediate mode
				arg2 = nums[ip+2]
			} else {
				arg2 = nums[ip+2]
				arg2 = nums[arg2]
			}
			//jump if true
			if arg1 == 0 {
				ip = arg2
				fmt.Printf("jmp if false : jumped to %d \n", ip)
			} else {
				ip = ip + 3
				fmt.Printf("jmp if false : ignored jump - advanced 3 codes\n")
			}
		} else if op == 7 { // less than
			var arg1, arg2, arg3 int
			p1 := parameter_mode1(nums[ip])
			p2 := parameter_mode2(nums[ip])
			// arg1
			if p1 == 1 { // 1 == immediate mode
				arg1 = nums[ip+1]
			} else {
				arg1 = nums[ip+1]
				arg1 = nums[arg1]
			}
			// arg2
			if p2 == 1 { // 1 == immediate mode
				arg2 = nums[ip+2]
			} else {
				arg2 = nums[ip+2]
				arg2 = nums[arg2]
			}

			arg3 = nums[ip+3]
			if arg1 < arg2 {
				nums[arg3] = 1
			} else {
				nums[arg3] = 0
			}
			ip = ip + 4

		} else if op == 8 { // equals
			var arg1, arg2, arg3 int
			p1 := parameter_mode1(nums[ip])
			p2 := parameter_mode2(nums[ip])
			// arg1
			if p1 == 1 { // 1 == immediate mode
				arg1 = nums[ip+1]
			} else {
				arg1 = nums[ip+1]
				arg1 = nums[arg1]
			}
			// arg2
			if p2 == 1 { // 1 == immediate mode
				arg2 = nums[ip+2]
			} else {
				arg2 = nums[ip+2]
				arg2 = nums[arg2]
			}

			arg3 = nums[ip+3]
			if arg1 == arg2 {
				nums[arg3] = 1
			} else {
				nums[arg3] = 0
			}
			ip = ip + 4

		} else { // bad opcode
			fmt.Printf("bad opcode (%d) at ip index %d \n", op, ip)
			return State{Ip: ip, Nums: nums, Steps: steps, Ok: false}
		}
		steps = steps + 1
	}
	fmt.Printf("bad ip (%d) we did not halt\n", ip)
	return State{Ip: ip, Nums: nums, Steps: steps, Ok: false}
}

/*
func run(nums []int) State {
	fmt.Print("running ...\n")
	final := interpret(nums)
	fmt.Printf("finished ...\nfinal state\n%v\n", final)
	return final
        }*/

func splitter(str string) []string {
	return strings.Split(str, ",")
}

func decode(text string) []int {
	// here we read entire content
	words := splitter(text)
	fmt.Printf("\nwords => %v \n", words)
	nums := parseNumbers(words)
	fmt.Printf("numbers => %v \n", nums)
	return nums
}

func part1(nums []int) {
	var wrote []int

	// just return 1 always
	reader := func() int {
		return 1
	}

	// remember what was written
	writer := func(n int) {
		wrote = append(wrote, n)
	}

	interpret(nums, reader, writer)

	fmt.Printf("part1 result !\n")
	fmt.Printf("%v \n", wrote)

}

func part2(nums []int) {
	var wrote []int

	// just return 5 always
	reader := func() int {
		return 5
	}

	// remember what was written
	writer := func(n int) {
		wrote = append(wrote, n)
	}

	interpret(nums, reader, writer)

	fmt.Printf("part2 result !\n")
	fmt.Printf("%v \n", wrote)

}

// func part2(nums []int) {
// 	// replace address 1 and 2 with number from 0 to 99
// 	var nums2 []int = make([]int, len(nums))
// 	copy(nums2, nums)

// 		//fmt.Printf("nums size %d , nums2 size %d\n", len(nums), len(nums2))
// 		nums[0] = 123
// 		if nums2[0] == 123 {
// 			fmt.Printf("aliasing on 123 at nums[0]\n")
// 		} else {
// 			fmt.Printf("OK - no aliasing \n")
// 		}

// 	for i := 0; i <= 99; i++ {
// 		for k := 0; k <= 99; k++ {
// 			nums2[1] = i
// 			nums2[2] = k

// 			interpret(nums2)

// 			if nums2[0] == 19690720 { // aha ! found ya
// 				noun := i
// 				verb := k
// 				result := (100 * noun) + verb
// 				fmt.Printf("part2 result ! noun= %d , verb = %d , result = %d \n", noun, verb, result)
// 				return
// 			}
// 			// reset - copy original tape code across
// 			copy(nums2, nums)
// 		}
// 	}

// 	fmt.Printf("no solution found ?\n")
// 	return

// }

func right_most_digits() {
	for i := 0; i <= 1000; i++ {
		fmt.Printf("rightmost digits of %d is %d \n", i, i%100)
	}
}

/*
   parameter mode 0 = positional mode = day 2 add multiply halt
   parameter mode 1 = immediate mode = day 5 read write
   1002
   01002

   want to translate 1002 into 01002
*/

func parameter_opcode(n int) int {
	return n % 100
}

func parameter_mode1(n int) int { // 0 or 1
	// 100
	//  xx
	if n < 100 {
		return 0
	}
	return (n % 1000) / 100
}

func parameter_mode2(n int) int { // 0 or 1
	// 1000
	//   xx
	if n < 1000 {
		return 0
	}
	return (n % 10000) / 1000
}

func parameter_mode3(n int) int { // 0 or 1
	// 10000
	//    xx
	if n < 10000 {
		return 0
	}
	return (n % 100000) / 10000
}

func parameter_mode_checks() {
	for i := 0; i <= 10; i++ {
		n := rand.IntN(100000)
		p1 := parameter_mode1(n)
		p2 := parameter_mode2(n)
		p3 := parameter_mode3(n)
		fmt.Printf("check %d : n= %d : p1 %d : p2 %d : p3 %d \n", i, n, p1, p2, p3)
	}
}

func main() {

	fmt.Printf("Yello world\n")

	filePath := "../input.txt"
	content, err := os.ReadFile(filePath)
	if err != nil {
		log.Fatal(err)
	}
	text := string(content)
	//decode(text)

	//part1(decode(text))

	part2(decode(text))

	// check understanding of modulo
	// right_most_digits() modulo is ok for remainder

	//parameter_mode_checks()

	//run(decode("3,2,99"))
	//run(decode("4,2,99"))

	/*
		run(decode("1,0,0,0,99"))
		run(decode("2,3,0,3,99"))
		run(decode("2,4,4,5,99,0"))
		run(decode("1,1,1,4,99,5,6,0,99"))
		run(decode("1,9,10,3,2,3,11,0,99,30,40,50"))

		part1(decode(text))
		part2(decode(text))
	*/
	//mycheck([]int{1, 2, 3})

	// words0 := strings.Split("0,0,0\n", ",")
	// fmt.Printf("words0 %v \n", words0)

	// ok nums looks ok
	// lets now figure out what it means

	// part1(nums)
	// part2(nums)

	/*
		var f int = 2
		fmt.Printf("fuel %d requires %d in total\n", f, fuel2(f))
		f = 1969
		fmt.Printf("fuel %d requires %d in total\n", f, fuel2(f))
		f = 100756
		fmt.Printf("fuel %d requires %d in total\n", f, fuel2(f))
	*/

}
