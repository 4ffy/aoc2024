#include <fstream>
#include <print>
#include <regex>
#include <string>
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

int main(int argc, char *argv[]) {
	if (argc != 2) {
		println(stderr, "Usage: {} input", argv[0]);
		return 1;
	}
	string src = read_file(argv[1]);
	vector<string> lines = split_regex(src, "\n");
	int i = 0;
	for (auto &line : lines) {
		++i;
		println("{}\t{}", i, line);
	}
	return 0;
}
