package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func part2(s string) {
	lines := strings.Split(s, "\n")

	safe_count := 0
	for _, line := range lines {
		if len(line) <= 0 {
			continue
		}
		delta := 0
		levels_s := strings.Split(line, " ")

		levels_num := []int{}
		for _, s := range levels_s {
			n, _ := strconv.Atoi(s)
			levels_num = append(levels_num, n)
		}

		any_safe := false

		for mask := -1; mask < len(levels_num); mask++ {
			masked := []int{}

			for i, l := range levels_num {
				if i == mask {
					continue
				}
				masked = append(masked, l)
			}

			if masked[0] > masked[1] {
				delta = -1
			} else {
				delta = 1
			}

			unsafe_count := 0
			for i := 0; i < len(masked)-1; i++ {

				l := masked[i]

				if delta > 0 && l > masked[i+1] {
					unsafe_count++
					break
				}

				if delta < 0 && l < masked[i+1] {
					unsafe_count++
					break
				}

				if masked[i+1] == l {
					unsafe_count++
					break
				}

				if (delta > 0 && masked[i+1]-l > 3) || (delta < 0 && l-masked[i+1] > 3) {
					unsafe_count++
					break

				}

			}
			if unsafe_count <= 0 {
				any_safe = true
			}
		}

		if any_safe {
			safe_count++
		}

	}

	fmt.Println("Safe reports: ", safe_count)

}

func part1(s string) {
	lines := strings.Split(s, "\n")

	safe_count := 0
	for _, line := range lines {
		if len(line) <= 0 {
			continue
		}
		delta := 0
		levels_s := strings.Split(line, " ")

		levels_num := []int{}
		for _, s := range levels_s {
			n, _ := strconv.Atoi(s)
			levels_num = append(levels_num, n)
		}

		any_safe := false

		if levels_num[0] > levels_num[1] {
			delta = -1
		} else {
			delta = 1
		}

		unsafe_count := 0
		for i := 0; i < len(levels_num)-1; i++ {

			l := levels_num[i]

			if delta > 0 && l > levels_num[i+1] {
				unsafe_count++
				break
			}

			if delta < 0 && l < levels_num[i+1] {
				unsafe_count++
				break
			}

			if levels_num[i+1] == l {
				unsafe_count++
				break
			}

			if (delta > 0 && levels_num[i+1]-l > 3) || (delta < 0 && l-levels_num[i+1] > 3) {
				unsafe_count++
				break

			}

		}
		if unsafe_count <= 0 {
			any_safe = true
		}

		if any_safe {
			safe_count++
		}

	}

	fmt.Println("Safe reports: ", safe_count)
}

func main() {
	b, _ := os.ReadFile(os.Args[1])
	s := string(b)

	fmt.Println("--Part 1--")
	part1(s)
	fmt.Println("--Part 2--")
	part2(s)

}
