#!/usr/bin/env -S gawk -f

# Calculate the absolute value of a number.
function abs(x) {
	return x >= 0 ? x : -x
}

# Store the fields of the current record into a list.
function record_to_list(out) {
	for (i = 1; i <= NF; i++) {
		out[i] = $i
	}
}

# Store the fields of the current record into a list, but SKIP field x.
function record_to_list_skip(out, x) {
	if (x < 1 || x > NF) {
		return
	}
	for (i = 1; i < x; i++) {
		out[i] = $i
	}
	for (i = x + 1; i <= NF; i++) {
		out[i - 1] = $i
	}
}

# Determine whether a numeric list is strictly increasing.
function is_increasing(list) {
	for (i in list) {
		if (i + 1 in list && list[i] >= list[i + 1]) {
			return 0
		}
	}
	return 1
}

# Determine whether a numeric list is strictly decreasing.
function is_decreasing(list) {
	for (i in list) {
		if (i + 1 in list && list[i] <= list[i + 1]) {
			return 0
		}
	}
	return 1
}

# Determine whether all adjacent items in a list have a distance of 3 or less.
function is_smooth(list) {
	for (i in list) {
		if (i + 1 in list && abs(list[i + 1] - list[i]) > 3) {
			return 0
		}
	}
	return 1
}

# Determine whether a list is naively safe i.e. is smooth and either increasing
# or decreasing.
function is_naively_safe(list) {
	return (is_increasing(list) || is_decreasing(list)) && is_smooth(list)
}

# Determine whether a record is safe without removing any elements.
function record_is_naively_safe() {
	delete list
	record_to_list(list)
	if (is_naively_safe(list)) {
		return 1
	}
	return 0
}

# Determine whether a record is safe.
function record_is_safe() {
	delete list
	record_to_list(list)

	if (is_naively_safe(list)) {
		return 1
	}

	for (a = 1; a <= NF; a++) {
		delete sublist
		record_to_list_skip(sublist, a)
		if (is_naively_safe(sublist)) {
			return 1
		}
	}

	return 0
}

{
	if (record_is_naively_safe()) {
		naive++
		safe++
	} else {
		if (record_is_safe()) {
			safe++
		}
	}
}

END {
	print "Naively safe:", naive
	print "Safe:", safe
}
