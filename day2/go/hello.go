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

func interpret(nums []int) State {
	ip := 0
	op := 0
	arg1 := 0
	arg2 := 0
	arg3 := 0
	steps := 0

	//fmt.Printf("ip is set at %d\n", ip)
	//fmt.Printf("there are %d numbers in input\n", len(nums))

	for (ip >= 0) && (ip < len(nums)) {
		//fmt.Printf("ip = %d\n", ip)
		op = nums[ip]
		if op == 99 { // halt
			//fmt.Printf("halted\n")
			return State{Ip: ip, Nums: nums, Steps: steps, Ok: true}
		}
		arg1 = nums[ip+1]
		arg2 = nums[ip+2]
		arg3 = nums[ip+3]

		if op == 1 { // add
			nums[arg3] = nums[arg1] + nums[arg2]
			ip = ip + 4
			//fmt.Printf("added\n")
		} else if op == 2 { // mult
			nums[arg3] = nums[arg1] * nums[arg2]
			ip = ip + 4
			//fmt.Printf("multiplied\n")

		} else { // bad opcode
			fmt.Printf("bad opcode (%d) at ip index %d \n", op, ip)
			return State{Ip: ip, Nums: nums, Steps: steps, Ok: false}
		}
		steps = steps + 1
	}
	fmt.Printf("bad ip (%d) we did not halt\n", ip)
	return State{Ip: ip, Nums: nums, Steps: steps, Ok: false}
}

func run(nums []int) State {
	fmt.Print("running ...\n")
	final := interpret(nums)
	fmt.Printf("finished ...\nfinal state\n%v\n", final)
	return final
}

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
	interpret(nums)
	fmt.Printf("part1 result ! %d\n", nums[0])

}

func part2(nums []int) {
	// replace address 1 and 2 with number from 0 to 99
	var nums2 []int = make([]int, len(nums))
	copy(nums2, nums)

	/*
		//fmt.Printf("nums size %d , nums2 size %d\n", len(nums), len(nums2))
		nums[0] = 123
		if nums2[0] == 123 {
			fmt.Printf("aliasing on 123 at nums[0]\n")
		} else {
			fmt.Printf("OK - no aliasing \n")
		}
	*/
	for i := 0; i <= 99; i++ {
		for k := 0; k <= 99; k++ {
			nums2[1] = i
			nums2[2] = k

			interpret(nums2)

			if nums2[0] == 19690720 { // aha ! found ya
				noun := i
				verb := k
				result := (100 * noun) + verb
				fmt.Printf("part2 result ! noun= %d , verb = %d , result = %d \n", noun, verb, result)
				return
			}
			// reset - copy original tape code across
			copy(nums2, nums)
		}
	}

	fmt.Printf("no solution found ?\n")
	return

}

func main() {

	fmt.Printf("Yello world\n")

	filePath := "../input.txt"
	content, err := os.ReadFile(filePath)
	if err != nil {
		log.Fatal(err)
	}
	text := string(content)

	run(decode("1,0,0,0,99"))
	run(decode("2,3,0,3,99"))
	run(decode("2,4,4,5,99,0"))
	run(decode("1,1,1,4,99,5,6,0,99"))
	run(decode("1,9,10,3,2,3,11,0,99,30,40,50"))

	part1(decode(text))

	part2(decode(text))

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
