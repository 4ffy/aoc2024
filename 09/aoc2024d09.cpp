#include <fstream>
#include <optional>
#include <print>
#include <sstream>
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
		for (size_t i = 0; i < src.size() - 1; i++) {
			int count = src[i] - '0';
			if (i & 1) {
				for (int j = 0; j < count; j++) {
					result.push_back({});
				}
			} else {
				for (int j = 0; j < count; j++) {
					result.push_back(i / 2);
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
	SimpleDisk disk = SimpleDisk::from_string(src);
	println("Start checksum: {}", disk.checksum());
	disk.compress();
	println("Compressed checksum: {}", disk.checksum());
}
