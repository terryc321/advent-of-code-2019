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

func fuel1(mass int) int {
	return (mass / 3) - 2
}

// recursive
func fuel2(mass int) int {
	// ahead of time declaration -- hark old days of c again ...
	var fuel2r (func(int) int)

	fuel2r = func(mass int) int {
		f1 := (mass / 3) - 2
		if f1 < 0 {
			return 0
		} else {
			return f1 + fuel2r(f1)
		}
	}

	// if toplevel mass given results in negative fuel - just return mass of fuel itself
	// otherwise go recursive
	f1 := (mass / 3) - 2
	if f1 < 0 {
		return mass
	} else {
		return f1 + fuel2r(f1)
	}
}

func part1(nums []int) int {
	tot := 0
	for _, mass := range nums {
		tot = tot + fuel1(mass)
	}
	fmt.Printf("total fuel required %d\n", tot)
	return tot
}

func part2(nums []int) int {
	tot := 0
	for _, mass := range nums {
		tot = tot + fuel2(mass)
	}
	fmt.Printf("total fuel required %d\n", tot)
	return tot
}

func parseNumbers(lines []string) []int {
	var nums []int
	for _, line := range lines {
		if line == "" {
		} else if line == "(" || line == ")" {
		} else {
			n, err := strconv.Atoi(line)
			if err != nil {
			} else {
				nums = append(nums, n)
			}
		}
	}
	return nums
}

func main() {

	fmt.Printf("Hello world\n")

	filePath := "../input"
	content, err := os.ReadFile(filePath)
	if err != nil {
		log.Fatal(err)
	}

	// here we read entire content
	text := string(content)
	lines := strings.Split(text, "\n")

	nums := parseNumbers(lines)

	part1(nums)
	part2(nums)

	/*
		var f int = 2
		fmt.Printf("fuel %d requires %d in total\n", f, fuel2(f))
		f = 1969
		fmt.Printf("fuel %d requires %d in total\n", f, fuel2(f))
		f = 100756
		fmt.Printf("fuel %d requires %d in total\n", f, fuel2(f))
	*/

}
