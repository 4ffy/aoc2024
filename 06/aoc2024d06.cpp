#include <format>
#include <fstream>
#include <print>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using namespace std;

enum class Direction {
	North = 1 << 0,
	South = 1 << 1,
	West = 1 << 3,
	East = 1 << 4,
};

enum class WalkResult {
	Exit,
	Loop,
};

struct Cell {
	bool is_obstacle;
	unsigned visited;
};

template <> struct std::formatter<Cell> : std::formatter<char> {
	auto format(Cell const &c, format_context &ctx) const
	{
		char temp;
		if (c.is_obstacle) {
			temp = '#';
		} else if (c.visited) {
			temp = '@';
		} else {
			temp = '.';
		}
		return std::formatter<char>::format(temp, ctx);
	}
};

class Grid
{
  private:
	vector<Cell> m_data;
	long m_width;
	long m_height;
	long m_start_x;
	long m_start_y;
	bool m_walked;

  public:
	auto width() const -> long { return m_width; }
	auto height() const -> long { return m_height; }
	auto start_x() const -> long { return m_start_x; }
	auto start_y() const -> long { return m_start_y; }
	auto walked() const -> bool { return m_walked; }

	auto get(long y, long x) const -> Cell { return m_data[y * m_width + x]; }
	auto get_mut_ref(long y, long x) -> Cell &
	{
		return m_data[y * m_width + x];
	}

	Grid(string_view src)
		: m_width(-1), m_height(0), m_start_x(-1), m_start_y(-1),
		  m_walked(false)
	{
		for (size_t i = 0; i < src.size(); ++i) {
			switch (src[i]) {
			case '\n':
				++m_height;
				if (m_width == -1) {
					m_width = i;
				}
				break;
			case '#':
				m_data.push_back(Cell{.is_obstacle = true, .visited = 0});
				break;
			case '^':
				m_data.push_back(Cell{.is_obstacle = false, .visited = 0});
				if (m_width == -1) {
					m_start_x = i;
				} else {
					m_start_x = (i - m_height) % m_width;
				}
				m_start_y = m_height;
				break;
			default:
				m_data.push_back(Cell{.is_obstacle = false, .visited = 0});
				break;
			}
		}
	}

	void clear_walk()
	{
		for (auto &x : m_data) {
			x.visited = 0;
		}
		m_walked = false;
	}

	WalkResult walk()
	{
		if (m_walked) {
			clear_walk();
		}

		long x = m_start_x;
		long y = m_start_y;
		Direction dir = Direction::North;
		while (true) {
			Cell &curr = get_mut_ref(y, x);
			unsigned dir_val = static_cast<unsigned>(dir);
			if ((curr.visited | dir_val) == curr.visited) {
				return WalkResult::Loop;
			}
			curr.visited |= dir_val;

			switch (dir) {
				using enum Direction;
			case North:
				if (y - 1 < 0) {
					return WalkResult::Exit;
				} else if (get(y - 1, x).is_obstacle) {
					dir = East;
				} else {
					--y;
				}
				break;
			case South:
				if (y + 1 == m_height) {
					return WalkResult::Exit;
				} else if (get(y + 1, x).is_obstacle) {
					dir = West;
				} else {
					++y;
				}
				break;
			case West:
				if (x - 1 < 0) {
					return WalkResult::Exit;
				} else if (get(y, x - 1).is_obstacle) {
					dir = North;
				} else {
					--x;
				}
				break;
			case East:
				if (x + 1 == m_height) {
					return WalkResult::Exit;
				} else if (get(y, x + 1).is_obstacle) {
					dir = South;
				} else {
					++x;
				}
				break;
			}
		}
		m_walked = true;
	}

	bool obstacle_creates_loop(long y, long x)
	{
		if (y == m_start_y && x == m_start_x) {
			return false;
		}
		Cell &cell = get_mut_ref(y, x);
		cell.is_obstacle = true;
		clear_walk();
		if (walk() == WalkResult::Loop) {
			cell.is_obstacle = false;
			return true;
		}
		cell.is_obstacle = false;
		return false;
	}

	int count_visited()
	{
		int sum = 0;
		if (!m_walked) {
			walk();
		}
		for (auto c : m_data) {
			if (c.visited) {
				sum++;
			}
		}
		return sum;
	}

	int count_loop_adds()
	{
		int sum = 0;
		if (!m_walked) {
			walk();
		}
		Grid copy = *this;
		for (long y = 0; y < m_height; y++) {
			for (long x = 0; x < m_width; x++) {
				if (get(y, x).visited && copy.obstacle_creates_loop(y, x)) {
					++sum;
				}
			}
		}
		return sum;
	}
};

template <> struct std::formatter<Grid> : std::formatter<string> {
	auto format(Grid const &grid, format_context &ctx) const
	{
		string temp;
		for (long y = 0; y < grid.height(); ++y) {
			for (long x = 0; x < grid.width(); ++x) {
				format_to(back_inserter(temp), "{}", grid.get(y, x));
			}
			format_to(back_inserter(temp), "\n");
		}
		return std::formatter<string>::format(temp, ctx);
	}
};

string read_file(string_view filename)
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

int main(int argc, char *argv[])
{
	if (argc != 2) {
		println(stderr, "Usage: {} input", argv[0]);
		exit(1);
	}

	string data = read_file(argv[1]);

	Grid grid{data};
	println("Cells visited: {}", grid.count_visited());
	println("Loops possible: {}", grid.count_loop_adds());

	return 0;
}
