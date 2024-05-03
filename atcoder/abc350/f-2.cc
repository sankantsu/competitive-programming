#include <iostream>
#include <vector>
#include <string>
#include <cassert>
#include <ranges>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

// <expr> ::= <term><expr> | \eps
// <term> ::= (<expr>) | <char>
// <char> ::= [a-zA-Z]

char flip(char c) {
    if (isupper(c)) {
        return tolower(c);
    }
    else {
        return toupper(c);
    }
}

struct Node {
    Node() {}
    Node(char _c) {
        c = _c;
    }
    bool is_leaf() const {
        return c != 0;
    }
    void add_child(int child) {
        children.push_back(child);
    }
    char c = 0;
    vector<int> children;
};

struct Parser {
    Parser(string&& s) : _s(s) { }
    void parse() {
        _expr();
    }
    string dfs() {
        string acc;
        _dfs(0, 0, acc);
        return acc;
    }
    private:
    int _expr() {
        int v = _nodes.size();
        Node node;
        _nodes.push_back(std::move(node));
        while (_pos < _s.size()) {
            int child = _term();
            if (child == -1) {
                break;
            }
            else {
                _nodes[v].add_child(child);
            }
        }
        return v;
    }
    int _term() {
        char c = _s[_pos];
        if (c == ')') {
            _pos++;
            return -1;
        }
        if (c == '(') {
            _pos++;
            while (true) {
                int v = _expr();
                return v;
            }
        }
        else {
            int v = _char();
            return v;
        }
    }
    int _char() {
        char c = _s[_pos];
        assert(isalpha(c));
        int v = _nodes.size();
        Node n(c);
        _nodes.push_back(n);
        _pos++;
        return v;
    }
    void _dfs(int i, int depth, string& acc) {
        const auto& node = _nodes[i];
        if (node.is_leaf()) {
            char c = node.c;
            if (depth%2 == 0) c = flip(c);
            acc.push_back(c);
        }
        else {
            if (depth%2 == 0) {
                for (auto child : node.children) {
                    _dfs(child, depth+1, acc);
                }
            }
            else {
                for (auto child : node.children | views::reverse) {
                    _dfs(child, depth+1, acc);
                }
            }
        }
    }
    int _pos = 0;
    string _s;
    vector<Node> _nodes;
};

int main() {
    string s;
    cin >> s;

    Parser parser(std::move(s));
    parser.parse();

    string ans = parser.dfs();
    cout << ans << endl;
}
