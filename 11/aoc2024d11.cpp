#include <cmath>
#include <fstream>
#include <print>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

string read_file(string filename)
{
	ifstream infile(filename.data());
	if (infile.is_open()) {
		stringstream buf;
		buf << infile.rdbuf();
		return buf.str();
	}
	println(stderr, "Failed to read '{}'.", filename);
	exit(1);
}

vector<string> split_regex(string str, string pat)
{
	vector<string> result{};
	regex re{pat};
	sregex_token_iterator it(str.begin(), str.end(), re, -1);
	sregex_token_iterator end;
	while (it != end) {
		string temp = it->str();
		if (!temp.empty()) {
			result.push_back(std::move(temp));
		}
		++it;
	}
	return result;
}

long parse_number(string_view data)
{
	long result = 0;
	for (auto c : data) {
		if (c < '0' || c > '9') {
			throw runtime_error("String is not a number.");
		}
		result = 10 * result + (c - '0');
	}
	return result;
}

vector<long> parse_data(string data)
{
	vector<long> result{};
	vector<string> numbers = split_regex(data, "[ \\n]+");
	for (auto &n : numbers) {
		result.push_back(parse_number(n));
	}
	return result;
}

using Cache = unordered_map<pair<long, int>, long>;

template<> struct std::hash<pair<long, int>> {
	using argument_type = pair<long, int>;
	using result_type = size_t;
	result_type operator()(argument_type const &s) const noexcept {
		result_type const h1(std::hash<long>{}(s.first));
		result_type const h2(std::hash<int>{}(s.second));
		return h1 & (h2 << 1);
	}
};

static inline long digits(long x)
{
	return static_cast<long>(floor(log10(static_cast<double>(x))) + 1);
}

static inline long pow_10(long exp)
{
	return static_cast<long>(pow(10.0, static_cast<double>(exp)));
}

static inline pair<long, long> split(long x)
{
	long split = pow_10(digits(x) / 2);
	return pair{x / split, x % split};
}

long blink(long x, int gens, Cache &cache)
{
	if (gens == 0) {
		return 1;
	}

	pair<long, int> key{x, gens};
	if (cache.contains(key)) {
		return cache[key];
	}

	long result = 0;
	if (x == 0) {
		result = blink(1, gens - 1, cache);
	} else if ((digits(x) & 1) == 0) {
		pair<long, long> s = split(x);
		result += blink(s.first, gens - 1, cache);
		result += blink(s.second, gens - 1, cache);
	} else {
		result += blink(2024 * x, gens - 1, cache);
	}
	cache[key] = result;
	return result;
}

int main(int argc, char *argv[]) {
	if (argc != 2) {
		println(stderr, "Usage: {} input", argv[0]);
		return 1;
	}
	string src = read_file(argv[1]);
	vector<long> numbers = parse_data(std::move(src));
	Cache cache{};

	long sum = 0;
	for (auto n : numbers) {
		sum += blink(n, 25, cache);
	}
	println("25 blinks: {}", sum);

	sum = 0;
	for (auto n : numbers) {
		sum += blink(n, 75, cache);
	}
	println("75 blinks: {}", sum);

	return 0;
}
