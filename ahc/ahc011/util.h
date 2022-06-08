#pragma once
#include <vector>
#include <chrono>

constexpr int hexchar2int(char c) {
    if ('0' <= c && c <= '9') {
        return c - '0';
    }
    else if ('a' <= c && c <= 'f') {
        return 10 + c - 'a';
    }
    else throw;
}

constexpr char dir2char(int dir) {
    switch(dir) {
        case(0): return 'L';
        case(1): return 'U';
        case(2): return 'R';
        case(3): return 'D';
        default:   throw;
    }
}

constexpr int char2dir(char c) {
    switch(c) {
        case('L'): return 0;
        case('U'): return 1;
        case('R'): return 2;
        case('D'): return 3;
        default:   throw;
    }
}

constexpr char reverse_dir(char c) {
    switch(c) {
        case('L'): return 'R';
        case('U'): return 'D';
        case('R'): return 'L';
        case('D'): return 'U';
        default:   throw;
    }
}

struct uftree {
    uftree(int n) : par(n) {
        for (int i = 0; i < n; i++) par[i] = i;
    }
    int find(int x) {
        if (par[x] == x) return x;
        return par[x] = find(par[x]);
    }
    bool same (int x, int y) {
        return find(x) == find(y);
    }
    void unite(int x, int y) {
        x = find(x); y = find(y);
        if (x == y) return;
        par[x] = y;
    }
    private:
    std::vector<int> par;
};

struct BIT {
    BIT(int n_) {
        n = 1;
        while (n < n_) {
            n *= 2;
        }
        bit.resize(n+1);
    }
    int sum(int i) {
        int s = 0;
        while (i > 0) {
            s += bit[i];
            i -= i&-i;
        }
        return s;
    }
    void add(int i, int x) {
        while (i <= n) {
            bit[i] += x;
            i += i&-i;
        }
    }
    private:
    int n;
    std::vector<int> bit;
};

struct Timer {
    using clock_type = std::chrono::system_clock;
    Timer() {
        start_time = clock_type::now();
    }
    int elapsed_milliseconds() {
        auto now_time = clock_type::now();
        auto duration = now_time - start_time;
        auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(duration).count();
        return static_cast<int>(ms);
    }
    private:
    clock_type::time_point start_time;
};
