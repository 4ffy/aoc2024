#include <fstream>
#include <memory>
#include <optional>
#include <print>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

using namespace std;

class Trie
{
  private:
	bool m_leaf;
	optional<unique_ptr<Trie>> m_b; // Black
	optional<unique_ptr<Trie>> m_g; // Green
	optional<unique_ptr<Trie>> m_r; // Red
	optional<unique_ptr<Trie>> m_u; // blUe
	optional<unique_ptr<Trie>> m_w; // White

  public:
	Trie() : m_leaf(false), m_b({}), m_g({}), m_r({}), m_u({}), m_w({}) {}

	size_t size()
	{
		size_t sum = m_leaf ? 1 : 0;
		if (m_b) {
			sum += m_b.value()->size();
		}
		if (m_g) {
			sum += m_g.value()->size();
		}
		if (m_r) {
			sum += m_r.value()->size();
		}
		if (m_u) {
			sum += m_u.value()->size();
		}
		if (m_w) {
			sum += m_w.value()->size();
		}
		return sum;
	}

	bool insert(string_view item)
	{
		if (item.length() == 0) {
			if (!m_leaf) {
				m_leaf = true;
				return true;
			} else {
				return false;
			}
		}
		string_view next{item.data() + 1, item.size() - 1};
		switch (item.at(0)) {
		case 'b':
			if (!m_b) {
				m_b = make_unique<Trie>();
			}
			return m_b.value()->insert(next);
		case 'g':
			if (!m_g) {
				m_g = make_unique<Trie>();
			}
			return m_g.value()->insert(next);
		case 'r':
			if (!m_r) {
				m_r = make_unique<Trie>();
			}
			return m_r.value()->insert(next);
		case 'u':
			if (!m_u) {
				m_u = make_unique<Trie>();
			}
			return m_u.value()->insert(next);
		case 'w':
			if (!m_w) {
				m_w = make_unique<Trie>();
			}
			return m_w.value()->insert(next);
		default:
			throw invalid_argument("Invalid char.");
		}
	}

	bool contains(string_view item)
	{
		if (item.length() == 0) {
			return m_leaf;
		}
		string_view next{item.data() + 1, item.size() - 1};
		switch (item.at(0)) {
		case 'b':
			return m_b ? m_b.value()->contains(next) : false;
		case 'g':
			return m_g ? m_g.value()->contains(next) : false;
		case 'r':
			return m_r ? m_r.value()->contains(next) : false;
		case 'u':
			return m_u ? m_u.value()->contains(next) : false;
		case 'w':
			return m_w ? m_w.value()->contains(next) : false;
		default:
			throw invalid_argument("Invalid char.");
		}
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

bool validate_design(Trie &patterns, string_view design)
{
	if (patterns.contains(design)) {
		return true;
	}
	for (size_t i = 0; i < design.size(); ++i) {
		string_view prefix{design.data(), design.size() - i};
		string_view suffix{design.data() + design.size() - i, i};
		if (patterns.contains(suffix) && validate_design(patterns, prefix)) {
			return true;
		}
	}
	return false;
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
	Trie trie{};
	for (auto &p : patterns) {
		trie.insert(p);
	}
	int valid = 0;
	for (auto &d : designs) {
		if (validate_design(trie, d)) {
			++valid;
		}
	}
	println("Valid designs: {}", valid);
	return 0;
}
