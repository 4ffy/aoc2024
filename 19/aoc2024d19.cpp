#include <fstream>
#include <optional>
#include <print>
#include <regex>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
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
		result.push_back(it->str());
		++it;
	}
	return result;
}

optional<pair<vector<string>, vector<string>>> parse_data(string src)
{
	vector<string> sections = split_regex(src, "\n\n");
	if (sections.size() != 2) {
		return {};
	}
	vector<string> patterns = split_regex(sections.at(0), ", ");
	vector<string> designs = split_regex(sections.at(1), "\n");
	return pair{patterns, designs};
}

bool validate_design(unordered_set<string_view> &patterns, string_view design)
{
	if (patterns.contains(design)) {
		return true;
	}
	for (size_t i = 0; i < design.size(); ++i) {
		string_view prefix = design.substr(0, design.size() - i);
		string_view suffix = design.substr(design.size() - i, i);
		if (patterns.contains(suffix) && validate_design(patterns, prefix)) {
			return true;
		}
	}
	return false;
}

long count_build_ways(unordered_set<string_view> patterns, string_view design)
{
	vector<long> cache(design.size() + 1, 0);
	cache.at(0) = 1;
	for (size_t i = 1; i <= design.size(); i++) {
		for (auto pat : patterns) {
			size_t length = pat.size();
			if (i >= length && design.substr(i - length, length) == pat) {
				cache.at(i) += cache.at(i - length);
			}
		}
	}
	return cache.at(design.size());
}

int main(int argc, char *argv[])
{
	if (argc != 2) {
		println(stderr, "Usage: {} input", argv[0]);
		return 1;
	}
	string src = read_file(argv[1]);
	auto data = parse_data(src);
	if (!data) {
		println(stderr, "Bad input format.");
		return 1;
	}
	vector<string> patterns = std::move(data.value().first);
	vector<string> designs = std::move(data.value().second);

	unordered_set<string_view> pats{};
	for (auto &p : patterns) {
		pats.insert(p);
	}
	long valid = 0;
	for (auto &d : designs) {
		if (validate_design(pats, d)) {
			++valid;
		}
	}
	println("Valid designs: {}", valid);

	long ways = 0;
	for (auto &d : designs) {
		ways += count_build_ways(pats, d);
	}
	println("Sum of ways(?): {}", ways);

	return 0;
}
