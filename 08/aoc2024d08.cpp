#include <array>
#include <fstream>
#include <print>
#include <regex>
#include <string>
#include <unordered_set>
#include <vector>

#define NUM_CHARS 62

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

int char_to_index(char c)
{
	if (c >= '0' && c <= '9') {
		return c - '0';
	}
	if (c >= 'A' && c <= 'Z') {
		return c - 'A' + 10;
	}
	if (c >= 'a' && c <= 'z') {
		return c - 'a' + 36;
	}
	return -1;
}

char index_to_char(int idx)
{
	if (idx < 10) {
		return (char)idx + '0';
	}
	if (idx < 36) {
		return (char)idx + 'A' - 10;
	}
	return (char)idx + 'a' - 36;
}

struct Point {
	long y;
	long x;
	Point(long y, long x) : y(y), x(x) {}

	inline bool operator==(Point const &other) const
	{
		return (y == other.y) && (x == other.x);
	}
};

template <> struct std::formatter<Point> : std::formatter<string> {
	auto format(Point const &point, format_context &ctx) const
	{
		string temp;
		format_to(back_inserter(temp), "(y={}, x={})", point.y, point.x);
		return std::formatter<string>::format(temp, ctx);
	}
};

template <> struct std::hash<Point> {
	typedef Point argument_type;
	typedef std::size_t result_type;
	result_type operator()(argument_type const &s) const noexcept
	{
		result_type const h1(std::hash<long>{}(s.y));
		result_type const h2(std::hash<long>{}(s.x));
		return h1 ^ (h2 << 1);
	}
};

class Grid
{
  private:
	array<vector<Point>, NUM_CHARS> m_data;
	long m_height;
	long m_width;

	bool in_bounds(long y, long x)
	{
		return 0 <= y && y < m_height && 0 <= x && x < m_width;
	}

  public:
	Grid(array<vector<Point>, NUM_CHARS> data, long height, long width)
		: m_data(data), m_height(height), m_width(width)
	{
	}

	auto data() const -> array<vector<Point>, NUM_CHARS> const &
	{
		return m_data;
	}

	static Grid from_string(string src)
	{
		vector<string> lines = split_regex(src, "\n");
		array<vector<Point>, NUM_CHARS> data{};
		long height = static_cast<long>(lines.size());
		long width = static_cast<long>(lines.at(0).size());
		for (long y = 0; y < height; ++y) {
			for (long x = 0; x < width; ++x) {
				char c = lines.at(y).at(x);
				int idx = char_to_index(c);
				if (idx != -1) {
					data.at(idx).push_back(
						Point{static_cast<int>(y), static_cast<int>(x)});
				}
			}
		}
		return Grid{data, height, width};
	}

	unordered_set<Point> find_antinodes()
	{
		unordered_set<Point> result{};
		for (auto &group : m_data) {
			if (group.empty()) {
				continue;
			}
			for (size_t i = 0; i < group.size() - 1; ++i) {
				for (size_t j = i + 1; j < group.size(); ++j) {
					Point p1 = group.at(i);
					Point p2 = group.at(j);
					long dy = p2.y - p1.y;
					long dx = p2.x - p1.x;
					if (in_bounds(p1.y - dy, p1.x - dx)) {
						result.insert(Point{p1.y - dy, p1.x - dx});
					}
					if (in_bounds(p2.y + dy, p2.x + dx)) {
						result.insert(Point{p2.y + dy, p2.x + dx});
					}
				}
			}
		}
		return result;
	}

	unordered_set<Point> find_antinodes_2()
	{
		unordered_set<Point> result{};
		for (auto &group : m_data) {
			if (group.empty()) {
				continue;
			}
			for (size_t i = 0; i < group.size() - 1; ++i) {
				for (size_t j = i + 1; j < group.size(); ++j) {
					Point p1 = group.at(i);
					Point p2 = group.at(j);
					long dy = p2.y - p1.y;
					long dx = p2.x - p1.x;
					long k = 0;
					bool done = false;
					while (!done) {
						done = true;
						if (in_bounds(p1.y - dy * k, p1.x - dx * k)) {
							done = false;
							result.insert(Point{p1.y - dy * k, p1.x - dx * k});
						}
						if (in_bounds(p2.y + dy * k, p2.x + dx * k)) {
							done = false;
							result.insert(Point{p2.y + dy * k, p2.x + dx * k});
						}
						++k;
					}
				}
			}
		}
		return result;
	}
};

template <> struct std::formatter<Grid> : std::formatter<string> {
	auto format(Grid const &grid, format_context &ctx) const
	{
		string temp;
		for (size_t i = 0; i < grid.data().size(); i++) {
			format_to(back_inserter(temp),
					  "{}:", index_to_char(static_cast<int>(i)));
			for (auto &x : grid.data().at(i)) {
				format_to(back_inserter(temp), " {}", x);
			}
			temp.push_back('\n');
		}
		return std::formatter<string>::format(temp, ctx);
	}
};

int main(int argc, char *argv[])
{
	if (argc != 2) {
		println(stderr, "Usage: {} input", argv[0]);
		return 1;
	}
	string src = read_file(argv[1]);
	Grid grid = Grid::from_string(src);
	// println("{}", grid);
	println("Unique antinodes: {}", grid.find_antinodes().size());
	println("Unique antinodes v2: {}", grid.find_antinodes_2().size());
}
