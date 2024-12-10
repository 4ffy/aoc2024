#include <fstream>
#include <print>
#include <regex>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <string>
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

struct Point {
	int y;
	int x;

	Point(int y, int x) : y(y), x(x) {}

	bool operator==(Point const &rhs) const
	{
		return this->y == rhs.y && this->x == rhs.x;
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

struct WalkResult {
	int score;
	int rating;
};

class Grid
{
  private:
	vector<int> m_data;
	int m_height;
	int m_width;

	WalkResult walk(int start_y, int start_x)
	{
		int rating = 0;
		unordered_set<Point> unique{};
		stack<Point> stak{};
		stak.push(Point{start_y, start_x});
		while (!stak.empty()) {
			Point curr = stak.top();
			stak.pop();
			int y = curr.y;
			int x = curr.x;
			int value = get(y, x);
			int target = value + 1;
			if (value == 9) {
				rating += 1;
				unique.insert(curr);
				continue;
			}
			if (y > 0 && get(y - 1, x) == target) {
				stak.push(Point{y - 1, x});
			}
			if (y < m_height - 1 && get(y + 1, x) == target) {
				stak.push(Point{y + 1, x});
			}
			if (x > 0 && get(y, x - 1) == target) {
				stak.push(Point{y, x - 1});
			}
			if (x < m_width - 1 && get(y, x + 1) == target) {
				stak.push(Point{y, x + 1});
			}
		}
		return WalkResult{.score = static_cast<int>(unique.size()),
						  .rating = rating};
	}

  public:
	Grid(vector<int> data, int height, int width)
		: m_data(data), m_height(height), m_width(width)
	{
	}

	auto height() const -> int { return m_height; }
	auto width() const -> int { return m_width; }
	auto get(int y, int x) const -> int { return m_data[y * m_width + x]; }

	static Grid from_string(string src)
	{
		vector<string> lines = split_regex(src, "\n");
		int height = static_cast<int>(lines.size());
		int width = static_cast<int>(lines[0].size());
		vector<int> data{};
		data.reserve(height * width);
		for (auto &line : lines) {
			for (auto c : line) {
				if (c < '0' && c > '9') {
					throw runtime_error("Bad input.");
				}
				data.push_back(c - '0');
			}
		}
		return Grid{std::move(data), height, width};
	}

	WalkResult score()
	{
		WalkResult result{};
		for (int y = 0; y < m_height; y++) {
			for (int x = 0; x < m_width; x++) {
				if (get(y, x) == 0) {
					WalkResult temp = walk(y, x);
					result.score += temp.score;
					result.rating += temp.rating;
				}
			}
		}
		return result;
	}
};

int main(int argc, char *argv[])
{
	if (argc != 2) {
		println(stderr, "Usage: {} input", argv[0]);
		return 1;
	}
	string src = read_file(argv[1]);
	Grid grid = Grid::from_string(std::move(src));
	WalkResult result = grid.score();
	println("Sum of score: {}", result.score);
	println("Sum of rating: {}", result.rating);
	return 0;
}
