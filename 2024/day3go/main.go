package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func perr() error {
	return fmt.Errorf("Parse error")
}

func is_digit(s string) bool {
	if _, err := strconv.Atoi(s); err != nil {
		return false
	}
	return true
}

func to_digit(s string) int {
	if s, err := strconv.Atoi(s); err == nil {
		return s
	}
	return 0
}

func parseMul(s string) (int, int, error) {
	adv := 0
	if !strings.HasPrefix(s, "mul(") {
		return 0, 0, perr()
	}
	s = s[4:]
	adv += 4
	if len(s) < 4 {
		return 0, 0, perr()
	}

	n1l := 0
	if !is_digit(string(s[0])) {
		return 0, 0, perr()
	}

	n1 := 0
	for is_digit(string(s[0])) {
		n1l++
		n1 = n1*10 + to_digit(string(s[0]))
		s = s[1:]
		adv += 1
	}
	if n1l > 3 {
		return 0, 0, perr()
	}
	if s[0] != ',' {
		return 0, 0, perr()
	}
	s = s[1:]
	adv += 1
	n2 := 0
	n2l := 0
	for is_digit(string(s[0])) {
		n2l++
		n2 = n2*10 + to_digit(string(s[0]))
		s = s[1:]
		adv += 1
	}

	if n2l > 3 {
		return 0, 0, perr()
	}

	if s[0] != ')' {
		return 0, 0, perr()
	}


	return n1 * n2, adv, nil
}

func part1(s string) {
	sum := 0

	for len(s) > 8 {

		n, adv, err := parseMul(s)
		if err == nil {
			s = s[adv:]
			sum += n
		} else {
			s = s[1:]
		}
	}
	fmt.Println("Sum =", sum)

}

func part2(s string) {
	sum := 0

	disabled := false
	for len(s) > 8 {
		if strings.HasPrefix(s, "don't()") {
			disabled = true
		}
		if strings.HasPrefix(s, "do()") {
			disabled = false
		}

		n, adv, err := parseMul(s)
		if err == nil {
			s = s[adv:]
			if !disabled {
				sum += n
			}

		} else {
			s = s[1:]
		}
	}
	fmt.Println("Sum =", sum)
}

func main() {
	sb, _ := os.ReadFile(os.Args[1])
	s := string(sb)

	fmt.Println("-- Part 1 --")
	part1(s)
	fmt.Println("\n-- Part 2 --")
	part2(s)
}
