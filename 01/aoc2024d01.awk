#!/usr/bin/env -S gawk -f
function abs(x) {
	return x >= 0 ? x : -x
}

function count(list, item) {
	result = 0
	for (x in list) {
		if (list[x] == item) {
			result += 1
		}
	}
	return result
}

# $NR /should/ work for filling the arrays according to my AWK understanding but
# it did not. So, I may as well keep the record count myself.
BEGIN {
	i = 0
}

{
	i += 1
	left[i] = $1
	right[i] = $2
}

END {
	asort(left)
	asort(right)

	sum = 0
	for (x in left) {
		sum += abs(left[x] - right[x])
	}
	print "Sum: " sum

	similarity = 0
	for (x in left) {
		similarity += left[x] * count(right, left[x])
	}
	print "Similarity: " similarity
}
