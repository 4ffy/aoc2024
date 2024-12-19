#include <array>
#include <fstream>
#include <optional>
#include <print>
#include <queue>
#include <regex>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

using namespace std;

struct Point {
	long y;
	long x;
	Point(long y, long x) : y(y), x(x) {}
	bool operator==(Point const &rhs) const
	{
		return this->y == rhs.y && this->x == rhs.x;
	}
};

static inline long manhattan(Point a, Point b)
{
	return abs(b.y - a.y) + abs(b.x - a.x);
}

template <> struct std::hash<Point> {
	size_t operator()(Point const &p) const noexcept
	{
		size_t h1(std::hash<long>{}(p.y));
		size_t h2(std::hash<long>{}(p.x));
		return h1 ^ (h2 << 1);
	}
};

struct Tile {
	bool visited;
	long start_dist;
	long end_dist;
	Tile()
		: visited(false), start_dist(numeric_limits<long>::max() / 2),
		  end_dist(numeric_limits<long>::max() / 2)
	{
	}
};

struct QueueItem {
	long priority;
	Point value;
	QueueItem(long priority, Point value) : priority(priority), value(value) {}
	bool operator>(QueueItem const &rhs) const
	{
		return this->priority > rhs.priority;
	}
};

class Grid
{
  private:
	unordered_map<Point, Tile> m_data;
	long m_height;
	long m_width;

	inline long queue_weight(Point p)
	{
		return m_data.at(p).start_dist + m_data.at(p).end_dist;
	}

  public:
	auto height() const -> long { return m_height; }
	auto width() const -> long { return m_width; }
	auto get(Point p) const -> optional<Tile>
	{
		if (m_data.contains(p)) {
			return m_data.at(p);
		}
		return {};
	}

	Grid(unordered_map<Point, Tile> data, long height, long width)
		: m_data(data), m_height(height), m_width(width)
	{
	}

	static Grid from_obstacles(vector<Point> obstacles, long count)
	{
		long height = -1;
		long width = -1;
		// There's some way to do this with ranges.
		for (auto &p : obstacles) {
			if (p.y > height) {
				height = p.y;
			}
			if (p.x > width) {
				width = p.x;
			}
		}
		++height;
		++width;
		unordered_map<Point, Tile> data{static_cast<size_t>(width * height)};
		for (long y = 0; y < height; ++y) {
			for (long x = 0; x < width; ++x) {
				data.insert({Point{y, x}, Tile{}});
			}
		}
		for (long i = 0; i < count; ++i) {
			data.erase(obstacles.at(i));
		}
		return Grid(std::move(data), height, width);
	}

	optional<long> a_star(Point start, Point end)
	{
		for (auto &item : m_data) {
			item.second.visited = false;
			item.second.start_dist = numeric_limits<long>::max() / 2;
			item.second.end_dist = numeric_limits<long>::max() / 2;
		}
		m_data.at(start).start_dist = 0;
		m_data.at(start).end_dist = manhattan(start, end);

		priority_queue<QueueItem, vector<QueueItem>, std::greater<QueueItem>>
			queue{};
		queue.push(QueueItem{queue_weight(start), start});

		while (!queue.empty()) {
			QueueItem curr = queue.top();
			queue.pop();
			Point pos = curr.value;
			Tile &tile = m_data.at(pos);
			tile.visited = true;
			if (pos == end) {
				return tile.start_dist;
			}

			array<Point, 4> dirs{
				Point{pos.y - 1, pos.x},
				Point{pos.y + 1, pos.x},
				Point{pos.y, pos.x - 1},
				Point{pos.y, pos.x + 1},
			};
			for (auto dir : dirs) {
				if (m_data.contains(dir) && !m_data.at(dir).visited) {
					if (tile.start_dist + 1 < m_data.at(dir).start_dist) {
						m_data.at(dir).start_dist = tile.start_dist + 1;
					}
					m_data.at(dir).end_dist = manhattan(pos, end);
					queue.push(QueueItem{queue_weight(dir), dir});
				}
			}
		}
		return {};
	}
};

void print_grid(Grid &grid)
{
	for (long y = 0; y < grid.height(); ++y) {
		for (long x = 0; x < grid.width(); ++x) {
			if (grid.get(Point{y, x})) {
				print(".");
			} else {
				print("#");
			}
		}
		println();
	}
}

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

optional<long> parse_integer(string_view src)
{
	long sum = 0;
	for (auto c : src) {
		if (c < '0' || c > '9') {
			return {};
		}
		sum = 10 * sum + c - '0';
	}
	return sum;
}

optional<vector<Point>> parse_obstacles(string src)
{
	vector<long> numbers{};
	regex re{"\\d+"};
	sregex_iterator it(src.begin(), src.end(), re);
	sregex_iterator end;
	while (it != end) {
		optional<long> temp = parse_integer(it->str());
		if (!temp) {
			return {};
		}
		numbers.push_back(temp.value());
		++it;
	}
	if ((numbers.size() & 1) == 1) {
		return {};
	}
	vector<Point> result{};
	for (size_t i = 0; i < numbers.size(); i += 2) {
		result.push_back(Point{numbers[i + 1], numbers[i]});
	}
	return result;
}

int main(int argc, char *argv[])
{
	if (argc != 3) {
		println(stderr, "Usage: {} input num_obstacles", argv[0]);
		return 1;
	}
	string src = read_file(argv[1]);
	optional<vector<Point>> obstacles = parse_obstacles(std::move(src));
	if (!obstacles) {
		println(stderr, "Bad input format.");
		exit(1);
	}
	optional<long> num_obstacles = parse_integer(argv[2]);
	if (!num_obstacles || num_obstacles.value() < 0) {
		println(stderr, "Invalid integer '{}'.", argv[2]);
		exit(1);
	}
	if (static_cast<size_t>(num_obstacles.value()) > obstacles.value().size()) {
		println(stderr, "Too many obstacles.");
		exit(1);
	}
	Grid grid = Grid::from_obstacles(obstacles.value(), num_obstacles.value());
	print_grid(grid);
	Point start{0, 0};
	Point end{grid.height() - 1, grid.width() - 1};
	optional<long> dist = grid.a_star(start, end);
	if (dist) {
		println("Shortest path: {} steps.", dist.value());
	} else {
		println("End point unreachable.");
	}
	return 0;
}
