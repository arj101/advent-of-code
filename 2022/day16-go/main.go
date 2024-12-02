package main

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

type Valve struct {
	name    string
	rate    int
	open    bool
	visited bool
	conns   []string
}

type Path = []string

var cache_lock = false
var path_cache = map[string](map[string]int){}

func shortest_paths(valves *map[string]*Valve, source string) map[string]int {
	if _, ok := path_cache[source]; ok {
		return path_cache[source]
	}

	dists := map[string]int{}


	unvisited := map[string]bool{}
	for _, valve := range *valves {
			unvisited[valve.name] = true
			dists[valve.name] = 9999999999
	}
	dists[source] = 0


	min_unvisited := func () string {
		min_dist := 99999999999
		min_valve := ""

		for valve, unvisited := range unvisited {
			if !unvisited { continue }
			if dists[valve] < min_dist {
				min_dist = dists[valve]
				min_valve = valve
			}
		}
		return min_valve
	}


	for {
		valve := min_unvisited()
		if len(valve) <= 0 {
			break;
		}

		unvisited[valve] = false
		for _, neighbour := range (*valves)[valve].conns {
			if dists[neighbour] > (1 + dists[valve]) {
				dists[neighbour] = 1 + dists[valve]
			}
		}
	}

	
	//cache_lock = true
	path_cache[source] = dists
	//cache_lock = false
	

	return dists
}

func with_open(open map[string]bool, valve string) map[string]bool {

	open_new := map[string]bool {}
	for valve, val := range open {
		open_new[valve] = val
	}
	
	open_new[valve] = true

	return open_new
}


func valve_score(valves *map[string]*Valve, valve string, minutes_left int, open map[string]bool, outer bool) (int,int,string) {
	dists := path_cache[valve]
	if outer {
	fmt.Println("paths from ",valve, " -> ", dists)
}
	if minutes_left < 0 {
		return 0,0,""
	}


	type chan_data struct {
		score int
		valve_name string
	}


	max_score := 0
	max_score_valve := ""


	for _, valve_other := range (*valves) {
		if open[valve_other.name] { continue }
		score := 0
		valve_minutes_left := minutes_left - dists[valve_other.name]
		if valve_other.rate > 0 {
			valve_minutes_left -= 1;
		}
		score = valve_other.rate * valve_minutes_left
		if score < 0 {
			continue
		}
		open[valve_other.name] = true
		score_, _,_ :=  valve_score(valves, valve_other.name, valve_minutes_left , open,false)
		open[valve_other.name] = false
		score += score_

		if outer {
			fmt.Println(valve_other.name, score)
		}

		

		if score > max_score {
			max_score = score
			max_score_valve = valve_other.name
		}
	}


	return max_score, dists[max_score_valve], max_score_valve
}



func part1(input string) {
	lines := strings.Split(input, "\n")

	valves := map[string]*Valve{}

	for _, line := range lines {
		line = strings.Trim(line, "\n\r\t ")
		if len(line) < 1 {
			continue
		}
		words := strings.Split(line, " ")
		valve := words[1]
		flow_rate := strings.Split(words[4], "=")[1]
		flow_rate = flow_rate[:len(flow_rate)-1]
		flow_rate_n, _ := strconv.Atoi(flow_rate)
		conns := words[9:]
		for i := 0; i < len(conns); i++ {
			conns[i] = strings.Trim(conns[i], ",")
		}
		v := Valve {
			name:    valve,
			rate:    flow_rate_n,
			open:    false,
			visited: false,
			conns:   conns,
		}
		valves[valve] = &v
	}

	/*
	clear_visited := func() {
		for _, valve := range valves {
			valve.visited = false
		}
	}
	*/
	
	valves["AA"].open = true

	open := map[string]bool{"AA":true}
	
	curr_valve := "AA"

	minutes_left := 30

	pressure := 0

	for valve, _ := range valves {
		fmt.Println("--> Calculating ", valve, " shortest paths...")
		dists := shortest_paths(&valves, valve)
		fmt.Println(dists)
	}
	fmt.Println("Completed shortest path calcs")
	
	for len(curr_valve) > 0 {

		fmt.Println("Open valves: ", open)
		fmt.Println("Releasing pressure: ", pressure)
		fmt.Println("Current valve >> ", curr_valve)
		score, dist, next_valve := valve_score(&valves, curr_valve, minutes_left, open,true)
		fmt.Println("Max from current >> ", score, dist, next_valve)

		curr_valve = next_valve
		open[curr_valve] = true

		minutes_left -= dist 
		if len(curr_valve) > 0 {
			if valves[curr_valve].rate > 0 {
				minutes_left -= 1
			}
			pressure += valves[curr_valve].rate * minutes_left
		}

	}

	fmt.Println("Total pressure release =", pressure)

}

func main() {
	f, _ := os.Open(os.Args[1])
	bytes, _ := io.ReadAll(f)
	s := string(bytes[:])
	fmt.Println("---Part 1---")
	part1(s)
}
