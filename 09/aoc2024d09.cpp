#include <array>
#include <fstream>
#include <optional>
#include <print>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

using namespace std;

class SimpleDisk : public vector<optional<int>>
{
  public:
	static SimpleDisk from_string(string_view src)
	{
		// Suppose the data is composed of uniformly-distributed random digits
		// 0-9. Then the average value for each file and free space is 4.5.
		// Therefore, rounding up, we need on average (5 * num_chars) elements
		// to store the data. Some inputs (e.g. all 1s or all 9s) will waste
		// space or require reallocation.
		SimpleDisk result{};
		result.reserve(5 * src.size());
		for (size_t i = 0; i < src.size() - 1; ++i) {
			if (src[i] < '0' || src[i] > '9') {
				println("{:x}", static_cast<int>(src[i]));
				throw runtime_error("Bad input.");
			}
			int id = static_cast<int>(i) / 2;
			int count = src[i] - '0';
			if (i & 1) {
				for (int j = 0; j < count; ++j) {
					result.push_back({});
				}
			} else {
				for (int j = 0; j < count; ++j) {
					result.push_back(id);
				}
			}
		}
		return result;
	}

	void compress()
	{
		auto *start = this->data();
		auto *end = &this->data()[this->size() - 1];
		while (start < end) {
			while (*start) {
				++start;
				goto cont; // continue outer
			}
			while (!*end) {
				--end;
				goto cont;
			}
			std::swap(*start, *end);
			++start;
			--end;
		cont:
		}
	}

	long checksum()
	{
		long sum = 0;
		long id = 0;
		for (auto v : *this) {
			if (v) {
				sum += id * v.value();
			}
			++id;
		}
		return sum;
	}
};

struct Block {
	int id;   // negative for empty
	int size; // only ever 0-9, but whatever
};

#if 0
struct BlockQueueItem {
	size_t index;
	Block *block;

	bool operator<(BlockQueueItem const &rhs) const
	{
		return this->block < rhs.block;
	}
};

template <> struct std::greater<BlockQueueItem> {
	constexpr bool operator()(BlockQueueItem &lhs, BlockQueueItem &rhs)
	{
		return lhs.block > rhs.block;
	}
};
#endif

class FancyDisk : public vector<Block>
{
  public:
	static FancyDisk from_string(string_view src)
	{
		// Each block stores its size, so unlike SimpleDisk we only need to
		// reserve num_chars. And it's exact!
		FancyDisk result{};
		result.reserve(src.size() - 1);
		for (size_t i = 0; i < src.size() - 1; ++i) {
			if (src[i] < '0' || src[i] > '9') {
				throw runtime_error("Bad input.");
			}
			int id = static_cast<int>(i) / 2;
			int count = src[i] - '0';
			if (i & 1) {
				result.push_back(Block{.id = -1, .size = count});
			} else {
				result.push_back(Block{.id = id, .size = count});
			}
		}
		return result;
	}

	// OH NO
	void defragment()
	{
		// Can't use priority_queue because I need direct access to the
		// underlying container. So we make a heap.
		auto free_space = array<vector<size_t>, 10>{};
		for (size_t i = 0; i < this->size(); ++i) {
			Block &b = (*this)[i];
			if (b.id == -1) {
				free_space[b.size].push_back(i);
			}
		}
		for (auto &q : free_space) {
			std::make_heap(q.begin(), q.end(), std::greater<>{});
		}

		for (size_t i = this->size() - 1; i > 0; --i) {
			Block &curr = (*this)[i];
			if (curr.id == -1) {
				continue;
			}

			// Scan for first non-empty queue of at least size. Skip if none.
			size_t leftmost = curr.size;
			while (leftmost < free_space.size() &&
				   free_space[leftmost].empty()) {
				++leftmost;
			}
			if (leftmost == free_space.size()) {
				continue;
			}
			// Scan the rest for any smaller locations.
			for (size_t j = leftmost; j < free_space.size(); j++) {
				if (!free_space[j].empty() &&
					free_space[j].front() < free_space[leftmost].front()) {
					leftmost = j;
				}
			}

			std::pop_heap(free_space[leftmost].begin(),
						  free_space[leftmost].end(), std::greater<>{});
			size_t swap_to = free_space[leftmost].back();
			free_space[leftmost].pop_back();
			if (swap_to >= i) {
				continue;
			}

			Block &to = (*this)[swap_to];
			int size_diff = to.size - curr.size;
			to.size = curr.size;
			std::swap(curr, to);

			if (size_diff > 0) {
				auto pos = this->begin();
				this->insert(std::next(pos, swap_to + 1),
							 Block{.id = -1, .size = size_diff});
				free_space[size_diff].push_back(swap_to);
				for (auto &q : free_space) {
					for (auto &v : q) {
						if (v >= swap_to) {
							++v;
						}
					}
					std::make_heap(q.begin(), q.end(), std::greater<>{});
				}
			}
		}
	}

	long checksum()
	{
		long sum = 0;
		long id = 0;
		for (auto &b : *this) {
			for (int i = 0; i < b.size; i++) {
				if (b.id != -1) {
					sum += id * b.id;
				}
				++id;
			}
		}
		return sum;
	}
};

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

int main(int argc, char *argv[])
{
	if (argc != 2) {
		println(stderr, "Usage: {} input", argv[0]);
		return 1;
	}
	string src = read_file(argv[1]);
	SimpleDisk disk_1 = SimpleDisk::from_string(src);
	FancyDisk disk_2 = FancyDisk::from_string(src);
	disk_1.compress();
	disk_2.defragment();
	println("Compress checksum: {}", disk_1.checksum());
	println("Defragment checksum {}", disk_2.checksum());
}
