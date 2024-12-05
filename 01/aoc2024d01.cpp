#include <cassert>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <memory>
#include <optional>
#include <print>
#include <span>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using namespace std;

class Scanner
{
  private:
	char const *m_start;
	char const *m_current;

	void skip_whitespace()
	{
		while (true) {
			char c = *m_current;
			switch (c) {
			case ' ':
			case '\r':
			case '\t':
			case '\n':
				++m_current;
				break;
			default:
				return;
			}
		}
	}

  public:
	Scanner(string_view src) : m_start(src.data()), m_current(m_start) {}

	optional<long> scan_number()
	{
		skip_whitespace();
		m_start = m_current;
		if (*m_current == '\0') {
			return {};
		}
		while (*m_current >= '0' && *m_current <= '9') {
			++m_current;
		}
		return strtol(m_start, nullptr, 10);
	}
};

static unique_ptr<string> read_file(string_view filename)
{
	ifstream infile(filename.data());
	if (infile.is_open()) {
		stringstream buf;
		buf << infile.rdbuf();
		return make_unique<string>(buf.str());
	}
	println(stderr, "Failed to read '{}'.", filename);
	exit(1);
}

static long distance(span<long> left, span<long> right)
{
	assert(left.size() == right.size());
	assert(std::is_sorted(left.cbegin(), left.cend()));
	assert(std::is_sorted(right.cbegin(), right.cend()));

	long distance = 0;
	for (size_t i = 0; i < left.size(); ++i) {
		if (left[i] > right[i]) {
			distance += left[i] - right[i];
		} else {
			distance += right[i] - left[i];
		}
	}
	return distance;
}

static long similarity(span<long> left, span<long> right)
{
	assert(left.size() == right.size());
	assert(std::is_sorted(left.cbegin(), left.cend()));
	assert(std::is_sorted(right.cbegin(), right.cend()));

	long similarity = 0;
	for (size_t i = 0; i < left.size(); ++i) {
		similarity +=
			left[i] * std::count(right.cbegin(), right.cend(), left[i]);
	}
	return similarity;
}

int main(int argc, char *argv[])
{
	if (argc != 2) {
		println(stderr, "Usage: {} input", argv[0]);
		exit(1);
	}
	unique_ptr<string> data = read_file(argv[1]);

	Scanner scanner(*data);
	vector<long> left{};
	vector<long> right{};

	while (true) {
		optional<long> a = scanner.scan_number();
		optional<long> b = scanner.scan_number();
		if (!a || !b) {
			break;
		}
		left.push_back(a.value());
		right.push_back(b.value());
	}

	std::sort(left.begin(), left.end());
	std::sort(right.begin(), right.end());

	println("Distance: {}", distance(left, right));
	println("Similarity: {}", similarity(left, right));
}
