package main

import (
	"errors"
	"fmt"
	"strings"
	// "unicode/utf8"
)

var ex1 string = `
#########
#b.A.@.a#
#########`

var ex2 string = `
########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################`

var ex3 string = `
########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################`

var ex4 string = `
#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################`

var ex5 string = `
########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################`

type Position struct {
	X int
	Y int
}

type Grid struct {
	Width  int
	Height int
	Start  Position
	Cells  [][]byte
}

/*
convert ascii representation to a grid
ensure all lines of string are same length after removed whitespace
*/
func gridize(str string) Grid {
	var result []string
	var width, height int
	var start Position
	lines := strings.Split(str, "\n")
	for _, line := range lines {
		if strings.TrimSpace(line) != "" {
			result = append(result, line)
		}
	}
	// assert all lines are of equal length
	//	var len0 int = utf8.RuneCountInString(result[0])
	var len0 int = len(result[0])
	for i, line := range result {
		//if utf8.RuneCountInString(line) != len0 {
		if len(line) != len0 {
			errors.New("different rune count in string")
		}
		fmt.Printf("%d: %s\n", i, line)
	}
	// assert all lines in result are of equal length
	grid := make([][]byte, len(lines))

	for y, line := range result {
		grid[y] = make([]byte, len(line))
		for x := 0; x < len(line); x++ {
			if line[x] == '@' {
				start = Position{
					X: x,
					Y: y,
				}
			}
			grid[y][x] = line[x]
		}
	}
	fmt.Printf("%c\n", grid[1][1]) // prints 'f'
	width = len(result[0])
	height = len(result)
	fmt.Printf("created a grid width %d , height %d , start %+v\n", width, height, start)

	return Grid{
		Width:  width,
		Height: height,
		Start:  start,
		Cells:  grid,
	}
}

/*
copy a grid
*/
func copyGrid(g Grid) Grid {
	grid := make([][]byte, g.Height)
	start := Position{X: 0, Y: 0}
	for y := 0; y < g.Height; y++ {
		grid[y] = make([]byte, g.Width)
		for x := 0; x < g.Width; x++ {
			grid[y][x] = g.Cells[y][x]
		}
	}
	start.X = g.Start.X
	start.Y = g.Start.Y
	width := g.Width
	height := g.Height

	return Grid{
		Width:  width,
		Height: height,
		Start:  start,
		Cells:  grid,
	}

}

/*
show a grid
*/
func showGrid(g Grid) {
	for y := 0; y < g.Height; y++ {
		for x := 0; x < g.Width; x++ {
			ch := g.Cells[y][x]
			fmt.Printf("%c", ch)
		}
		fmt.Println()
	}
	fmt.Println()
}

func isLower(b byte) bool {
	return b >= 'a' && b <= 'z'
}
func isUpper(b byte) bool {
	return b >= 'A' && b <= 'Z'
}

// domain keys and doors
func isKey(b byte) bool {
	return isLower(b)
}
func isDoor(b byte) bool {
	return isUpper(b)
}
func isWall(b byte) bool {
	return b == '#'
}
func isFloor(b byte) bool {
	return b == '.'
}

/*
find the shortest path that collects all the keys
compute all reachable keys (lower case letters) in the grid
do not exceed the bounds of the grid
no grid has exits , # goes all the way around exterior of play area
*/
func search(g Grid, x int, y int, step int) {
	if x < 1 || x >= g.Width {
		return
	}
	if y < 1 || y >= g.Height {
		return
	}
	got := g.Cells[y][x]
	fmt.Printf("searching from %d %d got %c", x, y, got)
	if got == '@' {
		fmt.Printf("we at start square %c\n", got)
	} else if isKey(got) {
		fmt.Printf("we got a key %c\n", got)
	} else if isDoor(got) {
		fmt.Printf("we reached a door %c\n", got)
	} else if isFloor(got) {
		fmt.Printf("we on floor %c\n", got)
	} else if isWall(got) {
		fmt.Printf("we reached a wall %c\n", got)
	} else {
		// not recognised ?
	}

}

/*
identify dead ends
######
#..... << a dead end
######
fill in dead ends makes search space smaller ?
are we looking for shortest path to next key ?
excess Doors which have no keys and do not lead anywhere besides dead ends

	WANT. a proof of the solution i found ... reasoning ?
*/
func reachable(g Grid) {
	// from a start position
	search(g, g.Start.X+1, g.Start.Y, 1)
	search(g, g.Start.X-1, g.Start.Y, 1)
	search(g, g.Start.X, g.Start.Y-1, 1)
	search(g, g.Start.X, g.Start.Y+1, 1)

}

func main() {
	fmt.Println("Hello, World!")
	gridize(ex1)
	gridize(ex2)
	gridize(ex3)
	gridize(ex4)
	g := gridize(ex5)
	gc := copyGrid(g)
	fmt.Println("start", gc.Start, gc.Start.X, gc.Start.Y)
	showGrid(gc)
	reachable(gc)
	//fmt.Println(ex1)
}
