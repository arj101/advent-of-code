package main

import (
	"fmt"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"
)

func part1(s string) {
	left := []int{}
	right := []int{}

	for _, line := range strings.Split(s, "\n") {
		if len(line) < 1 { continue }
		words := strings.Split(line, " ")
		ln, _ := strconv.Atoi(words[0])
		rn, _ := strconv.Atoi(words[len(words)-1])
		left = append(left, ln)
		right = append(right, rn)
	}

	sort.Slice(left, func(i, j int) bool {
		return left[i] > left[j]
	})

	sort.Slice(right, func(i, j int) bool {
		return right[i] > right[j]
	})

	dist := 0
	for i := 0; i < len(right); i++ {
		dist += int(math.Abs(float64(left[i] - right[i])))
	}
	fmt.Println("Total distance = ", dist)
}

func part2(s string) {
	left := []int{}

	freq := map[int]int{}

	for _, line := range strings.Split(s, "\n") {
		if len(line) < 1 { continue }
		words := strings.Split(line, " ")
		ln, _ := strconv.Atoi(words[0])
		rn, _ := strconv.Atoi(words[len(words)-1])
		left = append(left, ln)
		freq[rn] += 1
	}


	score := 0
	for i := 0; i < len(left); i++ {
		score += left[i] * freq[left[i]]
	}
	fmt.Println("Total score = ", score)

}

func main() {
	sb, _ := os.ReadFile(os.Args[1])
	s := string(sb)

	fmt.Println("--Part1--")
	part1(s)
	fmt.Println("--Part2--")
	part2(s)
}


