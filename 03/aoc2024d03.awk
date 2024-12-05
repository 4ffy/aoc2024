#!/usr/bin/env -S gawk -f

# Store all matches of re in str into arr. re must be passed as a string.
func all_matches(arr, re, str) {
	delete arr
	i = 1
	suffix = str
	while (match(suffix, re)) {
		arr[i] = substr(suffix, RSTART, RLENGTH)
		suffix = substr(suffix, RSTART + RLENGTH)
		i++
	}
}

# Find the result of a mul(x,y) instruction.
func do_mul(str) {
	match(str, mul_re, subscripts)
	return subscripts[1] * subscripts[2]
}

BEGIN {
	mul_re = "mul\\(([[:digit:]]{1,3}),([[:digit:]]{1,3})\\)"
	token_re = "do\\(\\)|don't\\(\\)|" mul_re
}

{
	all_matches(matches, token_re, $0)
	for (k in matches) {
		if (matches[k] == "do()") {
			disabled = 0
		} else if (matches[k] == "don't()") {
			disabled = 1
		} else {
			prod = do_mul(matches[k])
			part1_sum += prod
			if (!disabled) {
				part2_sum += prod
			}
		}
	}
}

END {
	print "Part 1:", part1_sum
	print "Part 2:", part2_sum
}
