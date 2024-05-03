#ifdef ONLINE_JUDGE
#define NDEBUG
#pragma GCC target("avx2")
#pragma GCC optimize("O3")
#pragma GCC optimize("unroll-loops")
#else
#undef NDEBUG
#endif

#include <algorithm>
#include <array>
#include <bitset>
#include <cassert>
#include <chrono>
#include <cmath>
#include <complex>
#include <concepts>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <functional>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <mutex>
#include <numeric>
#include <optional>
#include <queue>
#include <ranges>
#include <set>
#include <sstream>
#include <stack>
#include <string>
#include <thread>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace shr {
	namespace basic {
		using namespace std;
		using uchar = unsigned char;
		using uint = unsigned int;
		using ushort = unsigned short;
		using ull = unsigned long long;
		using ll = long long;
		using pii = pair<int, int>;
		using pdi = pair<double, int>;

		template <class T>
		concept printable = requires(T t, ostream& out) { out << t; };

		int len(const string& a) {
			return (int) a.length();
		}

		template <class T>
		int len(const vector<T>& a) {
			return (int) a.size();
		}

		template <class T>
		int len(const set<T>& a) {
			return (int) a.size();
		}

		template <class T>
		int len(const deque<T>& a) {
			return (int) a.size();
		}

		template <class T>
		int len(const priority_queue<T>& a) {
			return (int) a.size();
		}

		template <class T, int N>
		int len(const T (&a)[N]) {
			return N;
		}

		template <class T, int N>
		void clear_with(T (&a)[N], int val) {
			memset(a, val, sizeof(a));
		}

		template <totally_ordered T>
		bool update_min(T candidate, T& current_min) {
			if (candidate < current_min) {
				current_min = candidate;
				return true;
			}
			return false;
		}

		template <totally_ordered T>
		bool update_min_eq(T candidate, T& current_min) {
			if (candidate <= current_min) {
				current_min = candidate;
				return true;
			}
			return false;
		}

		template <totally_ordered T>
		bool update_max(T candidate, T& current_max) {
			if (candidate > current_max) {
				current_max = candidate;
				return true;
			}
			return false;
		}

		template <totally_ordered T>
		bool update_max_eq(T candidate, T& current_max) {
			if (candidate >= current_max) {
				current_max = candidate;
				return true;
			}
			return false;
		}

		template <class T>
		string tos(T a) {
			return to_string(a);
		}

		template <printable T>
		string tos(T a) {
			ostringstream os;
			os << a;
			return os.str();
		}

		constexpr double linearstep(double edge0, double edge1, double t) {
			return clamp((t - edge0) / (edge1 - edge0), 0.0, 1.0);
		}

		constexpr double smoothstep(double edge0, double edge1, double t) {
			t = linearstep(edge0, edge1, t);
			return t * t * (3 - 2 * t);
		}

		double exp_interp(double from, double to, double t) {
			return pow(from, 1 - t) * pow(to, t);
		}

		template <ranges::range Range, class F>
		auto mapped(Range& as, F f) {
			using T = ranges::range_value_t<Range>;
			using U = invoke_result_t<F, T&>;
			vector<U> res;
			for (auto& a : as) {
				res.push_back(f(a));
			}
			return res;
		}

		template <ranges::range Range, class F>
		auto mapped(const Range& as, F f) {
			using T = ranges::range_value_t<Range>;
			using U = invoke_result_t<F, const T&>;
			vector<U> res;
			for (auto& a : as) {
				res.push_back(f(a));
			}
			return res;
		}
	} // namespace basic
	using namespace basic;

	namespace timer {
		double time_scale = 1.0;

		// return in ms
		int timer(bool reset = false) {
			static auto st = chrono::system_clock::now();
			if (reset) {
				st = chrono::system_clock::now();
				return 0;
			} else {
				auto en = chrono::system_clock::now();
				int elapsed = (int) chrono::duration_cast<chrono::milliseconds>(en - st).count();
				return (int) round(elapsed / time_scale);
			}
		}
	} // namespace timer

	namespace tracer {
		bool debug = true;

		template <class T>
		concept is_pair = requires(T t) {
			t.first;
			t.second;
		};

		template <class T>
		concept has_str = requires(T t) {
			{ t.str() } -> convertible_to<string>;
		};

		template <printable T>
		void tracen(T&& t) {
			if (!debug)
				return;
			cerr << t;
		}

		template <class T>
		requires(has_str<T> && !printable<T>)
		void tracen(T&& t) {
			if (!debug)
				return;
			cerr << t.str();
		}

		template <class T, class U>
		void tracen(pair<T, U>& t) { // <- ?????????????????????? need this for trace(<iterable of pairs>)
			if (!debug)
				return;
			cerr << "(";
			tracen(t.first);
			cerr << ", ";
			tracen(t.second);
			cerr << ")";
		}

		template <class T, class U>
		void tracen(pair<T, U>&& t) { // <- ?????????????????????? need this for trace(make_pair(1, 2))
			if (!debug)
				return;
			cerr << "(";
			tracen(t.first);
			cerr << ", ";
			tracen(t.second);
			cerr << ")";
		}

		template <class T>
		requires(!printable<T>)
		void tracen(T&& t) {
			if (!debug)
				return;
			bool first = true;
			auto from = t.begin();
			auto until = t.end();
			cerr << "{";
			while (from != until) {
				if (first) {
					first = false;
				} else {
					cerr << ", ";
				}
				tracen(*from);
				from++;
			}
			cerr << "}";
		}

		template <class T, int N>
		requires(!same_as<decay_t<T>, char>)
		void tracen(T (&a)[N]) {
			if (!debug)
				return;
			cerr << "{";
			for (int i = 0; i < N; i++) {
				if (i > 0)
					cerr << ", ";
				tracen(a[i]);
			}
			cerr << "}";
		}

		template <class T1, class T2, class... Rest>
		void tracen(T1&& t1, T2&& t2, Rest&&... rest) {
			if (!debug)
				return;
			tracen(forward<T1>(t1));
			tracen(forward<T2>(t2), forward<Rest>(rest)...);
		}

		void trace() {
			if (!debug)
				return;
			cerr << endl;
		}

		template <class T, class... Rest>
		void trace(T&& t, Rest&&... rest) {
			if (!debug)
				return;
			tracen(forward<T>(t), forward<Rest>(rest)...);
			cerr << endl;
		}

		template <class T>
		requires(!printable<T>)
		void trace2d(T&& t, int h, int w) {
			if (!debug)
				return;
			bool first = true;
			auto from = t.begin();
			auto until = t.end();
			for (int i = 0; i < h; i++) {
				for (int j = 0; j < w; j++) {
					if (j > 0)
						tracen(" ");
					tracen(*from);
					from++;
					if (j == w - 1)
						trace();
				}
			}
		}

		template <class T, int N>
		requires(!same_as<decay_t<T>, char>)
		void trace2d(T (&a)[N], int h, int w) {
			if (!debug)
				return;
			int idx = 0;
			for (int i = 0; i < h; i++) {
				for (int j = 0; j < w; j++) {
					if (j > 0)
						tracen(" ");
					tracen(a[idx]);
					idx++;
					if (j == w - 1)
						trace();
				}
			}
		}
	} // namespace tracer
	using namespace tracer;

	namespace random {
		class rngen {
		public:
			rngen() {
			}

			// to avoid bugs
			rngen(const rngen&) = delete;

			rngen& operator=(const rngen&&) = delete;

			rngen(int s) {
				seed(s);
			}

			ull get_state() {
				return state;
			}

			void set_state(ull state) {
				this->state = state;
			}

			void seed(int s) {
				state = s + INCR;
				next32();
			}

			int next_int() {
				return next31();
			}

			int next_int(int mod) {
				assert(mod > 0);
				return (int) ((ull) next31() * mod >> 31);
			}

			int next_int(int min, int max) {
				return min + next_int(max - min + 1);
			}

			uint next_uint() {
				return next32();
			}

			ull next_ull() {
				return (ull) next32() << 32 | next32();
			}

			double next_float() {
				return (double) next31() / 0x80000000;
			}

			double next_float(double min, double max) {
				return min + next_float() * (max - min);
			}

			double next_normal() {
				return sqrt(-2 * log(next_float())) * cos(6.283185307179586 * next_float());
			}

			double next_normal(double mean, double sigma) {
				return mean + next_normal() * sigma;
			}

		private:
			static constexpr ull MULT = 0x8b46ad15ae59daadull;
			static constexpr ull INCR = 0xf51827be20401689ull;
			ull state = (ull) chrono::duration_cast<chrono::nanoseconds>(
			    chrono::system_clock::now().time_since_epoch())
			                .count();

			uint next32() {
				uint r = (uint) (state >> 59);
				state = state * MULT + INCR;
				state ^= state >> 18;
				uint t = (uint) (state >> 27);
				return t >> r | t << (-r & 31);
			}

			int next31() {
				return (int) (next32() & 0x7fffffff);
			}
		};

		void random_permutation(int* a, int n, rngen& rng) {
			assert(n >= 0);
			if (n == 0)
				return;
			a[0] = 0;
			for (int i = 1; i < n; i++) {
				a[i] = i;
				swap(a[i], a[rng.next_int(i + 1)]);
			}
		}

		template <class RandomAccessContainer>
		void shuffle(RandomAccessContainer& c, rngen& rng) {
			int n = len(c);
			for (int i = 1; i < n; i++) {
				swap(c[i], c[rng.next_int(i + 1)]);
			}
		}

		template <class RandomAccessContainer>
		auto& random_pick(RandomAccessContainer& c, rngen& rng) {
			return c[rng.next_int(len(c))];
		}

		template <class RandomAccessContainer>
		const auto& random_pick(const RandomAccessContainer& c, rngen& rng) {
			return c[rng.next_int(len(c))];
		}

		template <class T, int N>
		T& random_pick(T (&c)[N], rngen& rng) {
			return c[rng.next_int(N)];
		}

		template <class T, int N>
		const T& random_pick(const T (&c)[N], rngen& rng) {
			return c[rng.next_int(N)];
		}
	} // namespace random
	using namespace random;

	namespace ds {
		// random access: O(1)
		// push: O(1)
		// insert: n/a
		// erase by position: O(1)
		// erase by element: n/a
		// max size: fixed
		template <class T, int N>
		class fast_vector {
		private:
			T data[N];
			int num = 0;

		public:
			using iterator = T*;
			using const_iterator = const T*;

			iterator begin() {
				return data;
			}

			iterator end() {
				return data + num;
			}

			const_iterator begin() const {
				return data;
			}

			const_iterator end() const {
				return data + num;
			}

			void push_back(T a) {
				assert(num < N);
				data[num++] = a;
			}

			template <class... Args>
			void emplace_back(Args&&... args) {
				push_back(T(forward<Args>(args)...));
			}

			void erase(iterator where) {
				assert(where >= begin() && where < end());
				*where = data[--num];
			}

			void pop_back() {
				assert(num > 0);
				num--;
			}

			T& operator[](int i) {
				assert(i >= 0 && i < num);
				return data[i];
			}

			const T& operator[](int i) const {
				assert(i >= 0 && i < num);
				return data[i];
			}

			int size() const {
				return num;
			}
		};

		// random access: O(1)
		// push: O(1)
		// insert: n/a
		// erase: n/a
		// reallocation: never
		template <class T, int UnitBits = 20>
		class increasing_vector {
		private:
			static constexpr int UNIT_SIZE = 1 << UnitBits;
			static constexpr int UNIT_MASK = UNIT_SIZE - 1;
			T** packs;
			int num_packs = 0;
			int num_total = 0;

		public:
			increasing_vector(const increasing_vector& vec) = delete;
			increasing_vector& operator=(const increasing_vector& vec) = delete;

			increasing_vector() : packs(new T*[65536]) {
			}

			~increasing_vector() {
				for (int i = 0; i < num_packs; i++) {
					delete[] packs[i];
				}
				delete[] packs;
			}

			T* next_pointer() {
				if ((num_total++ & UNIT_MASK) == 0) {
					packs[num_packs++] = new T[UNIT_SIZE];
				}
				return &(*this)[num_total - 1];
			}

			void push_back(T a) {
				*next_pointer() = a;
			}

			template <class... Args>
			void emplace_back(Args&&... args) {
				push_back(T(forward<Args>(args)...));
			}

			T& operator[](int i) {
				assert(i >= 0 && i < num_total);
				return packs[i >> UnitBits][i & UNIT_MASK];
			}

			const T& operator[](int i) const {
				assert(i >= 0 && i < num_total);
				return packs[i >> UnitBits][i & UNIT_MASK];
			}

			int size() const {
				return num_total;
			}
		};

		// random access: O(1)
		// insert: O(1)
		// erase: O(1)
		// check: O(1)
		// max value: fixed
		template <int N>
		class fast_iset {
		private:
			int data[N];
			int indices[N];
			int num = 0;

		public:
			using iterator = int*;
			using const_iterator = const int*;

			fast_iset() {
				memset(indices, -1, sizeof(indices));
			}

			iterator begin() {
				return data;
			}

			iterator end() {
				return data + num;
			}

			const_iterator begin() const {
				return data;
			}

			const_iterator end() const {
				return data + num;
			}

			bool insert(int a) {
				assert(a >= 0 && a < N);
				if (indices[a] != -1)
					return false;
				data[num] = a;
				indices[a] = num;
				num++;
				return true;
			}

			bool erase(int a) {
				assert(a >= 0 && a < N);
				int index = indices[a];
				if (index == -1)
					return false;
				assert(num > 0);
				indices[data[index] = data[--num]] = index;
				indices[a] = -1;
				return true;
			}

			void clear() {
				memset(indices, -1, sizeof(indices));
				num = 0;
			}

			bool contains(int a) const {
				return indices[a] != -1;
			}

			const int& operator[](int i) const {
				assert(i >= 0 && i < num);
				return data[i];
			}

			int size() const {
				return num;
			}

			bool empty() const {
				return num == 0;
			}
		};

		// insert: O(1)
		// get/set: O(1)
		// clear: O(1)
		// erase: n/a
		template <class T, int BucketBits = 20>
		class hash_imap {
		private:
			static constexpr int BUCKET_SIZE = 1 << BucketBits;
			static constexpr int BUCKET_MASK = BUCKET_SIZE - 1;

			ull* keys;
			T* values;
			ushort* access_time;
			ushort time = (ushort) -1;
			int num_elements = 0;
			int last_index = -1;
			ull last_key = -1;
			bool last_found = false;

		public:
			hash_imap()
			    : keys(new ull[BUCKET_SIZE]), values(new T[BUCKET_SIZE]),
			      access_time(new ushort[BUCKET_SIZE]) {
			}

			~hash_imap() {
				delete[] keys;
				delete[] values;
				delete[] access_time;
			}

			hash_imap(const hash_imap& map)
			    : keys(new ull[BUCKET_SIZE]), values(new T[BUCKET_SIZE]),
			      access_time(new ushort[BUCKET_SIZE]) {
				memcpy(keys, map.keys, sizeof(ull[BUCKET_SIZE]));
				memcpy(values, map.values, sizeof(T[BUCKET_SIZE])); // can be potentially dangerous?
				memcpy(access_time, map.access_time, sizeof(ushort[BUCKET_SIZE]));
				time = map.time;
				num_elements = map.num_elements;
				last_index = map.last_index;
				last_key = map.last_key;
				last_found = map.last_found;
			}

			hash_imap& operator=(const hash_imap& map) {
				if (this == &map)
					return *this;
				delete[] keys;
				delete[] values;
				delete[] access_time;
				keys = new ull[BUCKET_SIZE];
				values = new T[BUCKET_SIZE];
				access_time = new ushort[BUCKET_SIZE];
				memcpy(keys, map.keys, sizeof(ull[BUCKET_SIZE]));
				memcpy(values, map.values, sizeof(T[BUCKET_SIZE])); // can be potentially dangerous?
				memcpy(access_time, map.access_time, sizeof(ushort[BUCKET_SIZE]));
				time = map.time;
				num_elements = map.num_elements;
				last_index = map.last_index;
				last_key = map.last_key;
				last_found = map.last_found;
				return *this;
			}

			void clear() {
				num_elements = 0;
				last_found = false;
				last_index = -1;
				if (++time == 0) {
					memset(access_time, 0, sizeof(ushort[BUCKET_SIZE]));
					time = 1;
				}
			}

			bool access(ull key) {
				last_key = key;
				last_index = (int) (key & BUCKET_MASK);
				bool debug = false;
				while (true) {
					if (access_time[last_index] != time) {
						return last_found = false;
					} else if (keys[last_index] == key) {
						return last_found = true;
					}
					last_index = (last_index + 1) & BUCKET_MASK;
				}
			}

			T get() const {
				assert(last_found);
				return values[last_index];
			}

			void set(T value) {
				assert(last_index != -1);
				access_time[last_index] = time;
				keys[last_index] = last_key;
				values[last_index] = value;
				num_elements += !last_found;
				assert(("bucket size is too small", num_elements < 0.85 * BUCKET_SIZE));
			}
		};

#if 0
		// a bitset, but cooler than std::bitset
		template <int Size>
		class rich_bitset {
		private:
			using word = ull;
			static_assert(has_single_bit(sizeof(word)));
			static constexpr int WORD_SHIFT = std::countr_zero(8 * sizeof(word));
			static constexpr int WORD_SIZE = 1 << WORD_SHIFT;
			static constexpr int WORD_MASK = WORD_SIZE - 1;
			static constexpr int NUM_WORDS = (Size + WORD_SIZE - 1) / WORD_SIZE;
			static constexpr int LAST_WORD = NUM_WORDS - 1;
			static constexpr word LAST_WORD_MASK =
			    (Size & WORD_MASK) == 0 ? word(-1) : (word(1) << (Size & WORD_MASK)) - 1;
#define REP_WORDS(i) for (int i = 0; i < NUM_WORDS; i++)
#define REP_INNER_WORDS(i) for (int i = 0; i < NUM_WORDS - 1; i++)
#define REP_WORDS_REV(i) for (int i = NUM_WORDS - 1; i >= 0; i--)
#define REP_INNER_WORDS_REV(i) for (int i = NUM_WORDS - 2; i >= 0; i--)

			// [LAST_WORD] [LAST_WORD - 1] [...] [1] [0]
			// <- higher bits              lower bits ->
			word data[NUM_WORDS];

			struct ref {
				rich_bitset<Size>& bs;
				const int pos;

				ref(rich_bitset<Size>& bs, int pos) : bs(bs), pos(pos) {
				}

				ref& operator=(bool val) {
					bs.set(pos, val);
					return *this;
				}

				operator bool() const {
					return bs.test(pos);
				}
			};

			void trim() {
				if constexpr ((Size & WORD_MASK) != 0) {
					data[LAST_WORD] &= LAST_WORD_MASK;
				}
			}

		public:
			rich_bitset(ull value = 0) {
				constexpr int BITS = sizeof(ull) * 8;
				for (int i = 0; i < (BITS + WORD_SIZE - 1) / WORD_SIZE; i++) {
					data[i] = value >> i * WORD_SIZE;
				}
				constexpr int OFFSET = (BITS + WORD_SIZE - 1) / WORD_SIZE;
				if constexpr (OFFSET < NUM_WORDS) {
					memset(data + OFFSET, 0, sizeof(word) * (NUM_WORDS - OFFSET));
				}
			}

			bool all() const {
				bool res = true;
				REP_INNER_WORDS(i) {
					res &= data[i] == word(-1);
				}
				res &= data[LAST_WORD] == LAST_WORD_MASK;
				return res;
			}

			bool none() const {
				bool res = true;
				REP_WORDS(i) {
					res &= data[i] == 0;
				}
				return res;
			}

			bool any() const {
				bool res = false;
				REP_WORDS(i) {
					res |= data[i] != 0;
				}
				return res;
			}

			int count() const {
				int res = 0;
				REP_WORDS(i) {
					res += popcount(data[i]);
				}
				return res;
			}

			int countr_zero() const {
				if constexpr (LAST_WORD == 0) {
					return std::countr_zero(word(data[LAST_WORD] | ~LAST_WORD_MASK));
				} else {
					int res = std::countr_zero(data[0]);
					int mask = -(res == WORD_SIZE); // continue adding if -1
					for (int i = 1; i < NUM_WORDS - 1; i++) {
						int count = std::countr_zero(data[i]);
						res += count & mask;
						mask &= -(count == WORD_SIZE);
					}
					int count = std::countr_zero(word(data[LAST_WORD] | ~LAST_WORD_MASK));
					res += count & mask;
					return res;
				}
			}

			int countl_zero() const {
				constexpr int LAST_WORD_SIZE = popcount(LAST_WORD_MASK);
				int res = std::countl_zero(word(~(~data[LAST_WORD] << (WORD_SIZE - LAST_WORD_SIZE))));
				int mask = -(res == LAST_WORD_SIZE); // continue adding if -1
				for (int i = NUM_WORDS - 2; i >= 0; i--) {
					int count = std::countl_zero(data[i]);
					res += count & mask;
					mask &= -(count == WORD_SIZE);
				}
				return res;
			}

			int countr_one() const {
				if constexpr (LAST_WORD == 0) {
					return std::countr_one(data[LAST_WORD]);
				} else {
					int res = std::countr_one(data[0]);
					int mask = -(res == WORD_SIZE); // continue adding if -1
					for (int i = 1; i < NUM_WORDS - 1; i++) {
						int count = std::countr_one(data[i]);
						res += count & mask;
						mask &= -(count == WORD_SIZE);
					}
					int count = std::countr_one(data[LAST_WORD]);
					res += count & mask;
					return res;
				}
			}

			int countl_one() const {
				constexpr int LAST_WORD_SIZE = popcount(LAST_WORD_MASK);
				int res = std::countl_one(word(data[LAST_WORD] << (WORD_SIZE - LAST_WORD_SIZE)));
				int mask = -(res == LAST_WORD_SIZE); // continue adding if -1
				for (int i = NUM_WORDS - 2; i >= 0; i--) {
					int count = std::countl_one(data[i]);
					res += count & mask;
					mask &= -(count == WORD_SIZE);
				}
				return res;
			}

			int size() const {
				return Size;
			}

			bool test(int pos) const {
				assert(pos >= 0 && pos < Size);
				return (data[pos >> WORD_SHIFT] >> (pos & WORD_MASK)) & 1;
			}

			uint to_uint() const {
				constexpr int BITS = sizeof(uint) * 8;
				for (int i = (BITS + WORD_SIZE - 1) / WORD_SIZE; i < NUM_WORDS; i++) {
					assert(("uint overflow", data[i] == 0));
				}
				if constexpr (WORD_SIZE > BITS) {
					assert(("uint overflow", (data[0] >> BITS) == 0));
				}
				uint res = (uint) data[0];
				for (int i = 1; i < (BITS + WORD_SIZE - 1) / WORD_SIZE && i < NUM_WORDS; i++) {
					res |= (uint) data[i] << i * WORD_SIZE;
				}
				return res;
			}

			ull to_ull() const {
				constexpr int BITS = sizeof(ull) * 8;
				for (int i = (BITS + WORD_SIZE - 1) / WORD_SIZE; i < NUM_WORDS; i++) {
					assert(("ull overflow", data[i] == 0));
				}
				if constexpr (WORD_SIZE > BITS) {
					assert(("ull overflow", (data[0] >> BITS) == 0));
				}
				ull res = (ull) data[0];
				for (int i = 1; i < (BITS + WORD_SIZE - 1) / WORD_SIZE && i < NUM_WORDS; i++) {
					res |= (ull) data[i] << i * WORD_SIZE;
				}
				return res;
			}

			rich_bitset& set(int pos, bool val = true) {
				assert(pos >= 0 && pos < Size);
				word bit = word(1) << (pos & WORD_MASK);
				if (val) {
					data[pos >> WORD_SHIFT] |= bit;
				} else {
					data[pos >> WORD_SHIFT] &= ~bit;
				}
				return *this;
			}

			rich_bitset& reset(int pos) {
				assert(pos >= 0 && pos < Size);
				return set(pos, false);
			}

			rich_bitset& flip(int pos) {
				assert(pos >= 0 && pos < Size);
				word bit = word(1) << (pos & WORD_MASK);
				data[pos >> WORD_SHIFT] ^= bit;
				return *this;
			}

			rich_bitset& set() {
				clear_with(data, -1);
				trim();
				return *this;
			}

			rich_bitset& reset() {
				clear_with(data, 0);
				return *this;
			}

			rich_bitset& flip() {
				REP_INNER_WORDS(i) {
					data[i] ^= word(-1);
				}
				data[LAST_WORD] ^= LAST_WORD_MASK;
				return *this;
			}

			rich_bitset& operator&=(const rich_bitset& a) {
				REP_WORDS(i) {
					data[i] &= a.data[i];
				}
				return *this;
			}

			rich_bitset& operator|=(const rich_bitset& a) {
				REP_WORDS(i) {
					data[i] |= a.data[i];
				}
				return *this;
			}

			rich_bitset& operator^=(const rich_bitset& a) {
				REP_WORDS(i) {
					data[i] ^= a.data[i];
				}
				return *this;
			}

			rich_bitset& operator<<=(int amount) {
				assert(amount >= 0 && amount < Size);
				int nw = amount >> WORD_SHIFT;
				if (nw > 0) {
					REP_WORDS_REV(i) {
						data[i] = i - nw < 0 ? 0 : data[i - nw];
					}
				}
				int nb = amount & WORD_MASK;
				if (nb) {
					for (int i = NUM_WORDS - 1; i > 0; i--) {
						data[i] = data[i] << nb | data[i - 1] >> (WORD_SIZE - nb);
					}
					data[0] <<= nb;
				}
				trim();
				return *this;
			}

			rich_bitset& operator>>=(int amount) {
				assert(amount >= 0 && amount < Size);
				int nw = amount >> WORD_SHIFT;
				if (nw > 0) {
					REP_WORDS(i) {
						data[i] = i + nw >= NUM_WORDS ? 0 : data[i + nw];
					}
				}
				int nb = amount & WORD_MASK;
				if (nb) {
					REP_INNER_WORDS(i) {
						data[i] = data[i] >> nb | data[i + 1] << (WORD_SIZE - nb);
					}
					data[LAST_WORD] >>= nb;
				}
				return *this;
			}

			rich_bitset& operator+=(const rich_bitset& a) {
				word carry = 0;
				REP_WORDS(i) {
					word l = data[i];
					word r = a.data[i];
					word sum = l + r;
					data[i] = sum + carry;
					carry = (sum < l) | (data[i] < sum);
				}
				trim();
				return *this;
			}

			rich_bitset& operator-=(const rich_bitset& a) {
				word carry = 1;
				REP_WORDS(i) {
					word l = data[i];
					word r = ~a.data[i];
					word sum = l + r;
					data[i] = sum + carry;
					carry = (sum < l) | (data[i] < sum);
				}
				trim();
				return *this;
			}

			rich_bitset& operator++() {
				word carry = 1;
				REP_WORDS(i) {
					word l = data[i];
					data[i] = l + carry;
					carry = (data[i] < l);
				}
				trim();
				return *this;
			}

			rich_bitset operator++(int) {
				rich_bitset res = *this;
				operator++();
				return res;
			}

			rich_bitset& operator--() {
				word carry = 0;
				REP_WORDS(i) {
					word l = data[i];
					data[i] = l - 1 + carry;
					carry = (l | carry) != 0;
				}
				trim();
				return *this;
			}

			rich_bitset operator--(int) {
				rich_bitset res = *this;
				operator--();
				return res;
			}

			rich_bitset operator~() const {
				rich_bitset res = *this;
				res.flip();
				return res;
			}

			friend rich_bitset operator&(const rich_bitset& a, const rich_bitset& b) {
				rich_bitset res = a;
				res &= b;
				return res;
			}

			friend rich_bitset operator|(const rich_bitset& a, const rich_bitset& b) {
				rich_bitset res = a;
				res |= b;
				return res;
			}

			friend rich_bitset operator^(const rich_bitset& a, const rich_bitset& b) {
				rich_bitset res = a;
				res ^= b;
				return res;
			}

			friend rich_bitset operator<<(const rich_bitset& a, int amount) {
				rich_bitset res = a;
				res <<= amount;
				return res;
			}

			friend rich_bitset operator>>(const rich_bitset& a, int amount) {
				rich_bitset res = a;
				res >>= amount;
				return res;
			}

			friend rich_bitset operator+(const rich_bitset& a, const rich_bitset& b) {
				rich_bitset res = a;
				res += b;
				return res;
			}

			friend rich_bitset operator-(const rich_bitset& a, const rich_bitset& b) {
				rich_bitset res = a;
				res -= b;
				return res;
			}

			friend bool operator==(const rich_bitset& a, const rich_bitset& b) {
				return memcmp(a.data, b.data, sizeof(a.data)) == 0;
			}

			friend bool operator!=(const rich_bitset& a, const rich_bitset& b) {
				return memcmp(a.data, b.data, sizeof(a.data)) != 0;
			}

			friend int operator<=>(const rich_bitset& a, const rich_bitset& b) {
				REP_WORDS_REV(i) {
					if (a.data[i] != b.data[i])
						return a.data[i] < b.data[i] ? -1 : 1;
				}
				return 0;
			}

			ref operator[](int pos) {
				return {*this, pos};
			}

			bool operator[](int pos) const {
				return test(pos);
			}

			string str() const {
				ostringstream oss;
				oss << *this;
				return oss.str();
			}

			friend ostream& operator<<(ostream& out, const rich_bitset& bs) {
				for (int i = Size - 1; i >= 0; i--) {
					out << (bs.test(i) ? '1' : '0');
				}
				return out;
			}
#undef REP_WORDS
#undef REP_INNER_WORDS
#undef REP_WORDS_REV
#undef REP_INNER_WORDS_REV
		};
#endif

		template <class T>
		class easy_stack {
		public:
			vector<T> data;

			auto begin() {
				return data.begin();
			}

			auto end() {
				return data.end();
			}

			auto begin() const {
				return data.begin();
			}

			auto end() const {
				return data.end();
			}

			void clear() {
				data.clear();
			}

			void push_back(T a) {
				data.push_back(a);
			}

			template <class... Args>
			void emplace_back(Args&&... args) {
				data.emplace_back(forward<Args>(args)...);
			}

			T pop_back() {
				T res = data.back();
				data.pop_back();
				return res;
			}

			int size() const {
				return (int) data.size();
			}

			bool empty() const {
				return data.empty();
			}
		};

		template <int N>
		int len(const fast_iset<N>& a) {
			return a.size();
		}

		template <class T, int N>
		int len(const fast_vector<T, N>& a) {
			return a.size();
		}

		template <class T, int BucketBits>
		int len(const hash_imap<T, BucketBits>& a) {
			return a.size();
		}

		template <class T>
		int len(const easy_stack<T>& a) {
			return a.size();
		}

		template <class T>
		requires(same_as<T, char> || same_as<T, short> || same_as<T, int>)
		struct int_vec2 {
			T i;
			T j;

			template <class U>
			constexpr int_vec2(int_vec2<U> a) : i(a.i), j(a.j) {
				assert(i == a.i);
				assert(j == a.j);
			}

			constexpr int_vec2() : i(0), j(0) {
			}

			constexpr int_vec2(T i, T j) : i(i), j(j) {
			}

			constexpr static int_vec2 dir(int index) {
				constexpr T DIRS[4][2] = {
				    {-1, 0},
				    {1, 0},
				    {0, -1},
				    {0, 1},
				};
				return {DIRS[index][0], DIRS[index][1]};
			}

			constexpr int dir_index() const {
				assert((i != 0) + (j != 0) == 1);
				return i < 0 ? 0 : i > 0 ? 1 : j < 0 ? 2 : 3;
			}

			constexpr int_vec2 rot(int sij, int num = 1) const {
				num &= 3;
				int_vec2 res = {i, j};
				while (num) {
					res = {sij - 1 - res.j, res.i};
					num--;
				}
				return res;
			}

			constexpr int_vec2 min(int_vec2 a) const {
				return {std::min(i, a.i), std::min(j, a.j)};
			}

			constexpr int_vec2 max(int_vec2 a) const {
				return {std::max(i, a.i), std::max(j, a.j)};
			}

			constexpr int_vec2 clamp(int_vec2 min, int_vec2 max) const {
				return {std::clamp(i, min.i, max.i), std::clamp(j, min.j, max.j)};
			}

			int_vec2 abs() const {
				return {std::abs(i), std::abs(j)};
			}

			int norm() const {
				return std::abs(i) + std::abs(j);
			}

			constexpr int dot(int_vec2 a) const {
				return i * a.i + j * a.j;
			}

			constexpr int pack(int sij) const {
				return pack(sij, sij);
			}

			constexpr int pack(int si, int sj) const {
				assert(in_bounds(si, sj));
				return i * sj + j;
			}

			constexpr int pack_if_in_bounds(int sij) const {
				return pack_if_in_bounds(sij, sij);
			}

			constexpr int pack_if_in_bounds(int si, int sj) const {
				if (!in_bounds(si, sj))
					return -1;
				return i * sj + j;
			}

			constexpr static int_vec2 unpack(int packed, int sij) {
				return unpack(packed, sij, sij);
			}

			constexpr static int_vec2 unpack(int packed, int si, int sj) {
				uint p = packed;
				uint i = packed / sj;
				uint j = packed - i * sj;
				assert(int_vec2(i, j).in_bounds(si, sj));
				return int_vec2(i, j);
			}

			constexpr bool in_bounds(int sij) const {
				return in_bounds(sij, sij);
			}

			constexpr bool in_bounds(int si, int sj) const {
				return i >= 0 && i < si && j >= 0 && j < sj;
			}

			constexpr int_vec2 operator+() const {
				return {i, j};
			}

			constexpr int_vec2 operator-() const {
				return {-i, -j};
			}

			constexpr friend int_vec2 operator+(int_vec2 a, int_vec2 b) {
				return {a.i + b.i, a.j + b.j};
			}

			constexpr friend int_vec2 operator+(T a, int_vec2 b) {
				return {a + b.i, a + b.j};
			}

			constexpr friend int_vec2 operator+(int_vec2 a, T b) {
				return {a.i + b, a.j + b};
			}

			constexpr friend int_vec2 operator-(int_vec2 a, int_vec2 b) {
				return {a.i - b.i, a.j - b.j};
			}

			constexpr friend int_vec2 operator-(T a, int_vec2 b) {
				return {a - b.i, a - b.j};
			}

			constexpr friend int_vec2 operator-(int_vec2 a, T b) {
				return {a.i - b, a.j - b};
			}

			constexpr friend int_vec2 operator*(int_vec2 a, int_vec2 b) {
				return {a.i * b.i, a.j * b.j};
			}

			constexpr friend int_vec2 operator*(T a, int_vec2 b) {
				return {a * b.i, a * b.j};
			}

			constexpr friend int_vec2 operator*(int_vec2 a, T b) {
				return {a.i * b, a.j * b};
			}

			constexpr friend int_vec2 operator/(int_vec2 a, int_vec2 b) {
				return {a.i / b.i, a.j / b.j};
			}

			constexpr friend int_vec2 operator/(T a, int_vec2 b) {
				return {a / b.i, a / b.j};
			}

			constexpr friend int_vec2 operator/(int_vec2 a, T b) {
				return {a.i / b, a.j / b};
			}

			constexpr friend int_vec2 operator%(int_vec2 a, int_vec2 b) {
				return {a.i % b.i, a.j % b.j};
			}

			constexpr friend int_vec2 operator%(T a, int_vec2 b) {
				return {a % b.i, a % b.j};
			}

			constexpr friend int_vec2 operator%(int_vec2 a, T b) {
				return {a.i % b, a.j % b};
			}

			constexpr int_vec2 operator+=(int_vec2 a) {
				i += a.i;
				j += a.j;
				return *this;
			}

			constexpr int_vec2 operator+=(T a) {
				i += a;
				j += a;
				return *this;
			}

			constexpr int_vec2 operator-=(int_vec2 a) {
				i -= a.i;
				j -= a.j;
				return *this;
			}

			constexpr int_vec2 operator-=(T a) {
				i -= a;
				j -= a;
				return *this;
			}

			constexpr int_vec2 operator*=(int_vec2 a) {
				i *= a.i;
				j *= a.j;
				return *this;
			}

			constexpr int_vec2 operator*=(T a) {
				i *= a;
				j *= a;
				return *this;
			}

			constexpr int_vec2 operator/=(int_vec2 a) {
				i /= a.i;
				j /= a.j;
				return *this;
			}

			constexpr int_vec2 operator/=(T a) {
				i /= a;
				j /= a;
				return *this;
			}

			constexpr int_vec2 operator%=(int_vec2 a) {
				i %= a.i;
				j %= a.j;
				return *this;
			}

			constexpr int_vec2 operator%=(T a) {
				i %= a;
				j %= a;
				return *this;
			}

			constexpr friend bool operator==(int_vec2 a, int_vec2 b) {
				return a.i == b.i && a.j == b.j;
			}

			constexpr friend bool operator!=(int_vec2 a, int_vec2 b) {
				return a.i != b.i || a.j != b.j;
			}

			constexpr friend bool operator<(int_vec2 a, int_vec2 b) {
				return a.i < b.i || a.i == b.i && a.j < b.j;
			}

			constexpr friend bool operator<=(int_vec2 a, int_vec2 b) {
				return a.i <= b.i || a.i == b.i && a.j <= b.j;
			}

			constexpr friend bool operator>(int_vec2 a, int_vec2 b) {
				return a.i > b.i || a.i == b.i && a.j > b.j;
			}

			constexpr friend bool operator>=(int_vec2 a, int_vec2 b) {
				return a.i >= b.i || a.i == b.i && a.j >= b.j;
			}

			friend ostream& operator<<(ostream& out, int_vec2 a) {
				out << "(" << a.i << ", " << a.j << ")";
				return out;
			}
		};

		template <class T>
		requires(same_as<T, char> || same_as<T, short> || same_as<T, int>)
		struct int_vec3 {
			T i;
			T j;
			T k;

			template <class U>
			constexpr int_vec3(int_vec3<U> a) : i(a.i), j(a.j), k(a.k) {
				assert(i == a.i);
				assert(j == a.j);
				assert(k == a.k);
			}

			constexpr int_vec3() : i(0), j(0), k(0) {
			}

			constexpr int_vec3(T i, T j, T k) : i(i), j(j), k(k) {
			}

			constexpr static int_vec3 dir(int index) {
				constexpr T DIRS[6][3] = {
				    {-1, 0, 0},
				    {1, 0, 0},
				    {0, -1, 0},
				    {0, 1, 0},
				    {0, 0, -1},
				    {0, 0, 1},
				};
				return {DIRS[index][0], DIRS[index][1], DIRS[index][2]};
			}

			constexpr int dir_index() const {
				assert((i != 0) + (j != 0) + (k != 0) == 1);
				return i < 0 ? 0 : i > 0 ? 1 : j < 0 ? 2 : j > 0 ? 3 : k < 0 ? 4 : 5;
			}

			constexpr int_vec3 min(int_vec3 a) const {
				return {std::min(i, a.i), std::min(j, a.j), std::min(k, a.k)};
			}

			constexpr int_vec3 max(int_vec3 a) const {
				return {std::max(i, a.i), std::max(j, a.j), std::max(k, a.k)};
			}

			constexpr int_vec3 clamp(int_vec3 min, int_vec3 max) const {
				return {
				    std::clamp(i, min.i, max.i), std::clamp(j, min.j, max.j), std::clamp(k, min.k, max.k)};
			}

			int_vec3 abs() const {
				return {std::abs(i), std::abs(j), std::abs(k)};
			}

			int norm() const {
				return std::abs(i) + std::abs(j) + std::abs(k);
			}

			constexpr int dot(int_vec3 a) const {
				return i * a.i + j * a.j + k * a.k;
			}

			constexpr int pack(int sijk) const {
				return pack(sijk, sijk, sijk);
			}

			constexpr int pack(int si, int sj, int sk) const {
				assert(in_bounds(si, sj, sk));
				return (i * sj + j) * sk + k;
			}

			constexpr int pack_if_in_bounds(int sijk) const {
				return pack_if_in_bounds(sijk, sijk, sijk);
			}

			constexpr int pack_if_in_bounds(int si, int sj, int sk) const {
				if (!in_bounds(si, sj, sk))
					return -1;
				return (i * sj + j) * sk + k;
			}

			constexpr static int_vec3 unpack(int packed, int sijk) {
				return unpack(packed, sijk, sijk, sijk);
			}

			constexpr static int_vec3 unpack(int packed, int si, int sj, int sk) {
				uint p = packed;
				uint ij = p / sk;
				uint k = p - ij * sk;
				uint i = ij / sj;
				uint j = ij - i * sj;
				assert(int_vec3(i, j, k).in_bounds(si, sj, sk));
				return int_vec3(i, j, k);
			}

			constexpr bool in_bounds(int sijk) const {
				return in_bounds(sijk, sijk, sijk);
			}

			constexpr bool in_bounds(int si, int sj, int sk) const {
				return i >= 0 && i < si && j >= 0 && j < sj && k >= 0 && k < sk;
			}

			constexpr int_vec3 operator+() const {
				return {i, j, k};
			}

			constexpr int_vec3 operator-() const {
				return {-i, -j, -k};
			}

			constexpr friend int_vec3 operator+(int_vec3 a, int_vec3 b) {
				return {a.i + b.i, a.j + b.j, a.k + b.k};
			}

			constexpr friend int_vec3 operator+(T a, int_vec3 b) {
				return {a + b.i, a + b.j, a + b.k};
			}

			constexpr friend int_vec3 operator+(int_vec3 a, T b) {
				return {a.i + b, a.j + b, a.k + b};
			}

			constexpr friend int_vec3 operator-(int_vec3 a, int_vec3 b) {
				return {a.i - b.i, a.j - b.j, a.k - b.k};
			}

			constexpr friend int_vec3 operator-(T a, int_vec3 b) {
				return {a - b.i, a - b.j, a - b.k};
			}

			constexpr friend int_vec3 operator-(int_vec3 a, T b) {
				return {a.i - b, a.j - b, a.k - b};
			}

			constexpr friend int_vec3 operator*(int_vec3 a, int_vec3 b) {
				return {a.i * b.i, a.j * b.j, a.k * b.k};
			}

			constexpr friend int_vec3 operator*(T a, int_vec3 b) {
				return {a * b.i, a * b.j, a * b.k};
			}

			constexpr friend int_vec3 operator*(int_vec3 a, T b) {
				return {a.i * b, a.j * b, a.k * b};
			}

			constexpr friend int_vec3 operator/(int_vec3 a, int_vec3 b) {
				return {a.i / b.i, a.j / b.j, a.k / b.k};
			}

			constexpr friend int_vec3 operator/(T a, int_vec3 b) {
				return {a / b.i, a / b.j, a / b.k};
			}

			constexpr friend int_vec3 operator/(int_vec3 a, T b) {
				return {a.i / b, a.j / b, a.k / b};
			}

			constexpr friend int_vec3 operator%(int_vec3 a, int_vec3 b) {
				return {a.i % b.i, a.j % b.j, a.k % b.k};
			}

			constexpr friend int_vec3 operator%(T a, int_vec3 b) {
				return {a % b.i, a % b.j, a % b.k};
			}

			constexpr friend int_vec3 operator%(int_vec3 a, T b) {
				return {a.i % b, a.j % b, a.k % b};
			}

			constexpr int_vec3 operator+=(int_vec3 a) {
				i += a.i;
				j += a.j;
				k += a.k;
				return *this;
			}

			constexpr int_vec3 operator+=(T a) {
				i += a;
				j += a;
				k += a;
				return *this;
			}

			constexpr int_vec3 operator-=(int_vec3 a) {
				i -= a.i;
				j -= a.j;
				k -= a.k;
				return *this;
			}

			constexpr int_vec3 operator-=(T a) {
				i -= a;
				j -= a;
				k -= a;
				return *this;
			}

			constexpr int_vec3 operator*=(int_vec3 a) {
				i *= a.i;
				j *= a.j;
				k *= a.k;
				return *this;
			}

			constexpr int_vec3 operator*=(T a) {
				i *= a;
				j *= a;
				k *= a;
				return *this;
			}

			constexpr int_vec3 operator/=(int_vec3 a) {
				i /= a.i;
				j /= a.j;
				k /= a.k;
				return *this;
			}

			constexpr int_vec3 operator/=(T a) {
				i /= a;
				j /= a;
				k /= a;
				return *this;
			}

			constexpr int_vec3 operator%=(int_vec3 a) {
				i %= a.i;
				j %= a.j;
				k %= a.k;
				return *this;
			}

			constexpr int_vec3 operator%=(T a) {
				i %= a;
				j %= a;
				k %= a;
				return *this;
			}

			constexpr friend bool operator==(int_vec3 a, int_vec3 b) {
				return a.i == b.i && a.j == b.j && a.k == b.k;
			}

			constexpr friend bool operator!=(int_vec3 a, int_vec3 b) {
				return a.i != b.i || a.j != b.j || a.k != b.k;
			}

			constexpr friend bool operator<(int_vec3 a, int_vec3 b) {
				return a.i < b.i || a.i == b.i && (a.j < b.j || a.j == b.j && a.k < b.k);
			}

			constexpr friend bool operator<=(int_vec3 a, int_vec3 b) {
				return a.i <= b.i || a.i == b.i && (a.j <= b.j || a.j == b.j && a.k <= b.k);
			}

			constexpr friend bool operator>(int_vec3 a, int_vec3 b) {
				return a.i > b.i || a.i == b.i && (a.j > b.j || a.j == b.j && a.k > b.k);
			}

			constexpr friend bool operator>=(int_vec3 a, int_vec3 b) {
				return a.i >= b.i || a.i == b.i && (a.j >= b.j || a.j == b.j && a.k >= b.k);
			}

			friend ostream& operator<<(ostream& out, int_vec3 a) {
				out << "(" << a.i << ", " << a.j << ", " << a.k << ")";
				return out;
			}
		};

		using cvec2 = int_vec2<char>;
		using svec2 = int_vec2<short>;
		using ivec2 = int_vec2<int>;
		using cvec3 = int_vec3<char>;
		using svec3 = int_vec3<short>;
		using ivec3 = int_vec3<int>;
	} // namespace ds
	using namespace ds;

	namespace beam_search {
#if 0
		// (state) -> score
		template <class T, class State, class Score>
		concept get_score =
		    totally_ordered<Score> && invocable<T, State&> && same_as<invoke_result_t<T, State&>, Score>;

		// (state, move) -> void
		template <class T, class State, class MoveId>
		concept apply_move =
		    invocable<T, State&, MoveId> && same_as<invoke_result_t<T, State&, MoveId>, void>;

		// (state) -> void
		template <class T, class State>
		concept undo_move = invocable<T, State&> && same_as<invoke_result_t<T, State&>, void>;

		// (state) -> void
		// see also: add_candidate
		template <class T, class State>
		concept enumerate_candidates = invocable<T, State&> && same_as<invoke_result_t<T, State&>, void>;

		// (turn) -> void
		// see also: candidates_to_filter
		template <class T>
		concept filter_candidates = invocable<T, int> && same_as<invoke_result_t<T, int>, void>;

		template <class State, totally_ordered Score, class MoveId, MoveId UnusedMoveId, class CandidateData,
		    class Direction = greater<Score>, int HashBucketBits = 20>
		class beam_search {
		private:
			struct candidate {
				int index; // index in orig_candidates
				int parent;
				MoveId move_id;
				CandidateData data;
				ull hash;
			};
			struct orig_candidate {
				int parent;
				MoveId move_id;
				bool chosen;
			};

			Direction dir = {};
			int current_parent = 0;
			hash_imap<int, HashBucketBits> best_indices;
			bool enumerating = false;
			bool filtering = false;
			vector<candidate> candidates;
			vector<orig_candidate> orig_candidates;

			void clear_candidates() {
				candidates.clear();
				orig_candidates.clear();
			}

		public:
			Score best_score = 0;
			int max_turn = -1;

			beam_search() {
			}

			beam_search(Direction dir) : dir(dir) {
			}

			void add_candidate(MoveId move_id, CandidateData data, ull hash) {
				assert(("not enumerating now", enumerating));
				candidates.emplace_back((int) candidates.size(), current_parent, move_id, data, hash);
				orig_candidates.emplace_back(current_parent, move_id);
			}

			vector<candidate>& candidates_to_filter() {
				assert(("not filtering now", filtering));
				return candidates;
			}

			// CAUTION: not stable
			template <predicate<candidate&, candidate&> CandidateDirection>
			void remove_duplicates(CandidateDirection candidate_direction) {
				assert(("not filtering now", filtering));
				best_indices.clear();
				int n = (int) candidates.size();
				for (int i = 0; i < n; i++) {
					candidate& cand = candidates[i];
					if (best_indices.access(cand.hash)) {
						int j = best_indices.get();
						candidate& cand2 = candidates[j];
						if (candidate_direction(cand, cand2)) {
							swap(candidates[i], candidates[j]);
						}
						swap(candidates[i], candidates[--n]);
						i--;
					} else {
						best_indices.set(i);
					}
				}
				candidates.resize(n);
			}

			template <get_score<State, Score> GetScore, apply_move<State, MoveId> ApplyMove,
			    enumerate_candidates<State> EnumerateCandidates, filter_candidates FilterCandidates>
			vector<MoveId> run(const State& initial_state, GetScore get_score, ApplyMove apply_move,
			    EnumerateCandidates enumerate_candidates, FilterCandidates filter_candidates) {
				struct node {
					State state;
					int history_index;
				};
				struct history {
					MoveId move_id;
					int parent;
				};
				vector<node> src;
				vector<node> dst;
				increasing_vector<history> hs;
				int turn = 0;

				// set initial state
				src.emplace_back(initial_state, -1);

				while (true) {
					int num_states = (int) src.size();

					clear_candidates();
					if (max_turn == -1 || turn < max_turn) {
						// enumerate candidates
						enumerating = true;
						for (int i = 0; i < num_states; i++) {
							current_parent = i;
							enumerate_candidates(src[i].state);
						}
						enumerating = false;

						// filer candiadtes
						filtering = true;
						filter_candidates(turn);
						filtering = false;
					}

					// check if finished
					if (candidates.empty()) {
						assert(("no states at the end", num_states > 0));

						// pick the best state
						best_score = get_score(src[0].state);
						int best_index = 0;
						for (int i = 1; i < num_states; i++) {
							Score score = get_score(src[i].state);
							if (dir(score, best_score)) {
								best_score = score;
								best_index = i;
							}
						}

						// restore moves
						vector<MoveId> res;
						int history_top = src[best_index].history_index;
						while (history_top != -1) {
							history& h = hs[history_top];
							res.push_back(h.move_id);
							history_top = h.parent;
						}
						reverse(res.begin(), res.end());
						return res;
					}

					// compute next states
					dst.clear();
					for (const auto& cand : candidates) {
						const auto& src_node = src[cand.parent];
						dst.emplace_back(src_node.state, hs.size());
						apply_move(dst.back().state, cand.move_id);
						hs.emplace_back(cand.move_id, src_node.history_index);
					}
					src.swap(dst);
					turn++;
				}
			}

			template <get_score<State, Score> GetScore, apply_move<State, MoveId> ApplyMove,
			    undo_move<State> UndoMove, enumerate_candidates<State> EnumerateCandidates,
			    filter_candidates FilterCandidates>
			vector<MoveId> run_tree(const State& initial_state, GetScore get_score, ApplyMove apply_move,
			    UndoMove undo_move, EnumerateCandidates enumerate_candidates,
			    FilterCandidates filter_candidates) {
				constexpr MoveId UNDO = UnusedMoveId;
				struct tour {
					vector<MoveId> src;
					vector<MoveId> dst;

					void move(const MoveId& move_id) {
						dst.push_back(move_id);
					}

					int position() {
						return (int) dst.size();
					}

					void swap() {
						src.swap(dst);
						dst.clear();
					}
				} tour;
				vector<MoveId> global_path;
				vector<MoveId> path;
				vector<orig_candidate> leaves;
				State st = initial_state;
				int turn = 0;
				int level = 0;
				int next_start_pos = 0;

				auto global_move = [&](const MoveId& move_id) {
					apply_move(st, move_id);
					global_path.push_back(move_id);
					level++;
				};

				auto global_undo = [&]() {
					undo_move(st);
					global_path.pop_back();
					level--;
				};

				while (true) {
					bool has_next_turn = max_turn == -1 || turn < max_turn;

					// compute the next tour
					int pos = next_start_pos;
					int prev_root_level = level;
					int next_root_level = numeric_limits<int>::max();
					orig_candidate best_leaf = {-1, MoveId{}, false};
					enumerating = true;
					clear_candidates();
					if (turn == 0) {
						best_score = get_score(st);
						best_leaf.chosen = true;
						if (has_next_turn) {
							current_parent = tour.position();
							enumerate_candidates(st);
						}
					} else {
						for (const orig_candidate& leaf : leaves) {
							int parent_pos = leaf.parent;

							// visit the parent of the leaf node
							if (pos < parent_pos) {
								// visit the LCA
								path.clear();
								do {
									auto move = tour.src[pos++];
									if (move == UNDO) {
										if (path.empty()) {
											global_undo();
											tour.move(UNDO);
											next_root_level = min(next_root_level, level);
										} else {
											path.pop_back();
										}
									} else {
										path.push_back(move);
									}
								} while (pos < parent_pos);

								// go directly to the parent
								for (auto move : path) {
									global_move(move);
									tour.move(move);
								}
							} // now we are at the parent of the leaf node

							// visit the leaf node
							apply_move(st, leaf.move_id);
							tour.move(leaf.move_id);

							Score score = get_score(st);
							if (!best_leaf.chosen || dir(score, best_score)) {
								best_score = score;
								best_leaf = leaf;
							}
							if (has_next_turn) {
								current_parent = tour.position();
								enumerate_candidates(st);
							}

							// leave the leaf node
							undo_move(st);
							tour.move(UNDO);
						}
					}
					next_root_level = min(next_root_level, level);
					enumerating = false;

					filtering = true;
					filter_candidates(turn);
					filtering = false;

					if (candidates.empty()) {
						assert(best_leaf.chosen);
						// undo to the root level
						while (level > prev_root_level) {
							global_undo();
						}
						// visit the best leaf
						pos = next_start_pos;
						while (pos < best_leaf.parent) {
							auto move = tour.src[pos++];
							if (move == UNDO) {
								global_undo();
							} else {
								global_move(move);
							}
						}
						if (best_leaf.parent != -1) {
							global_move(best_leaf.move_id);
						}
						return global_path;
					}

					// finalize the next tour
					tour.swap();
					turn++;

					// collect the next leaf nodes, in the original order
					leaves.clear();
					for (const candidate& cand : candidates) {
						orig_candidates[cand.index].chosen = true;
					}
					for (const orig_candidate& cand : orig_candidates) {
						if (!cand.chosen)
							continue;
						leaves.push_back(cand);
					}

					// undo to the next root level
					while (level > next_root_level) {
						global_undo();
					}

					// adjust the next starting position
					next_start_pos = next_root_level - prev_root_level;
				}
			}
		};

		class beam_width_manager {
		private:
			double prev_time = 0;
			double moving_average_time = 0;
			vector<double> progress_history;
			vector<double> time_history;
			vector<int> width_history;
			int last_width = 0;
			int count = 0;

		public:
			int window_size = 50;
			int default_width;

			beam_width_manager(int default_width) : default_width(default_width) {
			}

			int next(double progress, double time, double time_limit) {
				progress_history.push_back(progress);
				time_history.push_back(time);
				width_history.push_back(last_width);
				count++;
				if (count <= window_size) {
					return last_width = default_width;
				}
				int i1 = count - 1 - window_size;
				int i2 = count - 1;
				double progress_sum = progress_history[i2] - progress_history[i1];
				double time_sum = time_history[i2] - time_history[i1];
				if (progress_sum == 0 || time_sum == 0) {
					// window size is too small
					window_size *= 2;
					return last_width = default_width;
				}
				int width_sum = 0;
				for (int i = i1 + 1; i <= i2; i++) {
					width_sum += width_history[i];
				}
				double progress_per_turn = progress_sum / window_size;
				double time_per_width = time_sum / width_sum;
				double left_time = time_limit - time;
				double left_progress = 1 - progress;
				if (left_time <= 0 || left_progress <= 0)
					return 1;
				double left_turn = left_progress / progress_per_turn;
				double left_time_per_turn = left_time / left_turn;
				double left_width_per_turn = left_time_per_turn / time_per_width;
				return last_width = round(left_width_per_turn);
			}

			void report(int actual_last_width) {
				last_width = actual_last_width;
			}
		};
#endif
	} // namespace beam_search

	namespace simulated_annealing {
		// (state) -> score
		template <class T, class State, class Score>
		concept get_score =
		    totally_ordered<Score> && invocable<T, State&> && same_as<invoke_result_t<T, State&>, Score>;

		// (iter) -> progress
		template <class T>
		concept update_progress = invocable<T, int> && same_as<invoke_result_t<T, int>, double>;

		// (state, tolerance) -> accepted
		template <class T, class State>
		concept try_transition =
		    invocable<T, State&, double> && same_as<invoke_result_t<T, State&, double>, bool>;

		template <class State, totally_ordered Score, class Direction = greater<Score>>
		class simulated_annealing {
		private:
			Direction dir = {};

		public:
			int clock_interval = 10;
			double t_from = 100;
			double t_to = 0.01;
			double progress = 0;
			int num_iterations = 0;
			int num_acceptances = 0;
			int num_rejections = 0;
			bool use_linear_temp = false;
			Score best_score = 0;

			simulated_annealing() {
			}

			simulated_annealing(Direction dir) : dir(dir) {
			}

			template <get_score<State, Score> GetScore, update_progress UpdateProgress,
			    try_transition<State> TryTransition>
			State run(const State& initial_state, rngen& rng, GetScore get_score,
			    UpdateProgress update_progress, TryTransition try_transition,
			    function<void(State&, Score, int, double)> best_updated = nullptr) {
				State state = initial_state;
				Score score = get_score(state);
				State best_state = state;
				best_score = score;

				num_iterations = 0;
				num_acceptances = 0;
				num_rejections = 0;
				int interval = clock_interval;
				progress = 0;
				double t = t_from;
				while (true) {
					if (--interval <= 0) {
						progress = update_progress(num_iterations);
						if (progress >= 1)
							break;
						t = use_linear_temp ? lerp(t_from, t_to, progress)
						                    : exp_interp(t_from, t_to, progress);
						interval = clock_interval;
					}
					double tolerance = t * -log(rng.next_float());
					if (try_transition(state, tolerance)) {
						num_acceptances++;
						score = get_score(state);
						if (dir(score, best_score)) {
							best_state = state;
							best_score = score;
							if (best_updated) {
								best_updated(state, score, num_iterations, t);
							}
						}
					} else {
						num_rejections++;
					}
					num_iterations++;
				}
				return best_state;
			}
		};
	} // namespace simulated_annealing

	namespace dijkstra {
#if 0
		// (vertex) -> index
		template <class T, class Vertex>
		concept get_index = invocable<T, Vertex> && same_as<invoke_result_t<T, Vertex>, int>;

		// (vertex) -> is_goal
		template <class T, class Vertex>
		concept is_goal = invocable<T, Vertex> && same_as<invoke_result_t<T, Vertex>, bool>;

		// (vertex, distance) -> void
		template <class T, class Vertex, class Weight>
		concept visit_adjacent_vertices =
		    invocable<T, Vertex, Weight> && same_as<invoke_result_t<T, Vertex, Weight>, void>;

		template <class Vertex, class Weight, Weight Infinity, int MaxVertexIndex>
		requires(integral<Weight> || floating_point<Weight>)
		class dijkstra {
		private:
			using vw = pair<Vertex, Weight>;
			static constexpr int VERTEX_ARRAY_SIZE = MaxVertexIndex + 1;
			vector<vw> toVisit;
			bool visiting = false;

		public:
			array<bool, VERTEX_ARRAY_SIZE> visited;
			array<Weight, VERTEX_ARRAY_SIZE> distance;
			array<optional<Vertex>, VERTEX_ARRAY_SIZE> previous;

			dijkstra() {
			}

			template <get_index<Vertex> GetIndex, is_goal<Vertex> IsGoal,
			    visit_adjacent_vertices<Vertex, Weight> VisitAdjacentVertices>
			void run(const vector<Vertex>& starts, GetIndex get_index, IsGoal is_goal,
			    VisitAdjacentVertices visit_adjacent_vertices) {
				auto comp = [](vw& a, vw& b) {
					return a.second > b.second;
				};

				visited.fill(false);
				previous.fill(nullopt);
				distance.fill(Infinity);
				priority_queue<vw, vector<vw>, decltype(comp)> q(comp);
				for (auto& st : starts) {
					distance[get_index(st)] = Weight(0);
					q.emplace(st, Weight(0));
				}

				int loop = 0;
				while (!q.empty()) {
					if (++loop % 10 == 0)
						trace(loop, " ", timer::timer());
					auto [from, dist] = q.top();
					q.pop();
					int fromi = get_index(from);
					if (visited[fromi])
						continue;
					visited[fromi] = true;
					if (is_goal(from)) {
						return;
					}

					visiting = true;
					toVisit.clear();
					visit_adjacent_vertices(from, dist);
					visiting = false;

					for (vw& pair : toVisit) {
						Vertex to = pair.first;
						int toi = get_index(to);
						Weight new_dist = pair.second;
						if (new_dist < distance[toi]) {
							distance[toi] = new_dist;
							previous[toi] = from;
							q.emplace(to, new_dist);
						}
					}
				}
			}

			void visit(Vertex vertex, Weight total_distance) {
				assert(("not visiting now", visiting));
				toVisit.emplace_back(vertex, total_distance);
			}

			template <get_index<Vertex> GetIndex>
			vector<Vertex> restore_path(Vertex goal, GetIndex get_index) {
				assert(("goal not reached", visited[get_index(goal)]));
				vector<Vertex> res;
				Vertex v = goal;
				while (true) {
					res.push_back(v);
					if (!previous[get_index(v)])
						break;
					v = previous[get_index(v)].value();
				}
				reverse(res.begin(), res.end());
				return res;
			}
		};
#endif
	}; // namespace dijkstra

	namespace file {
		string pad4(int n) {
			return (n < 0 || n >= 1000 ? "" : n < 10 ? "000" : n < 100 ? "00" : "0") + tos(n);
		}

		string input_file_name(int seed) {
			return "in/" + pad4(seed) + ".txt";
		}

		string output_file_name(int seed) {
			return "out/" + pad4(seed) + ".txt";
		}

		string movie_file_name(int seed) {
			return "mov/" + pad4(seed) + ".smv";
		}

		bool write_text(string fileName, string text, bool append = false) {
			ofstream fout;
			fout.open(fileName, append ? ios::out | ios::app : ios::out);
			if (!fout)
				return false;
			fout << text;
			fout.close();
			return true;
		}

		pair<string, bool> read_text(string fileName) {
			ifstream fin;
			fin.open(fileName, ios::in);
			if (!fin)
				return make_pair("", false);
			string res;
			string line;
			while (getline(fin, line)) {
				res += line;
				res += "\n";
			}
			return make_pair(res, true);
		}

		void write_text_assert(string fileName, string text, bool append = false) {
			auto res = write_text(fileName, text, append);
			assert(res);
		}

		string read_text_assert(string fileName) {
			auto res = read_text(fileName);
			assert(res.second);
			return res.first;
		}
	} // namespace file
} // namespace shr
using namespace shr::basic;
using namespace shr::ds;
using namespace shr::beam_search;
using namespace shr::simulated_annealing;
using namespace shr::dijkstra;
using namespace shr::random;
using namespace shr::timer;
using namespace shr::tracer;
using namespace shr::file;

//
// --- macros ---
//

#define rep(i, from, until) for (int i = (from); i < (until); i++)
#define repr(i, from, until) for (int i = (until) -1; i >= (from); i--)
#define rep0(i, until) rep(i, 0, until)
#define rep0r(i, until) repr(i, 0, until)

//
// --- movie lib ---
//

class movie {
private:
	bool is_open = false;
	string file_name;
	ofstream out;

#ifdef ONLINE_JUDGE
	void write_instruction(const string& it, const vector<double>& argf, const vector<string>& args) {
	}
#else
	void write_instruction(const string& it, const vector<double>& argf, const vector<string>& args) {
		assert(("file name is not set", file_name != ""));
		if (!is_open) {
			is_open = true;
			out = ofstream(file_name, ios_base::out | ios_base::binary);
			out.write("smv", 3);
		}
		write_string(it);
		for (double f : argf) {
			write_float(f);
		}
		for (auto& s : args) {
			write_string(s);
		}
		if (it == "n") {
			out.flush(); // flush at the end of each frame
		}
	}

	void write_float(double a) {
		float f = (float) a;
		out.write((char*) &f, 4);
	}

	void write_string(const string& str) {
		out.write(str.c_str(), str.length() + 1);
	}
#endif

public:
	static constexpr int LEFT = 0;
	static constexpr int CENTER = 1;
	static constexpr int RIGHT = 2;

	movie() {
	}

	void set_file(string file) {
		assert(!is_open);
		file_name = file;
	}

	void close_file() {
		if (!is_open)
			return;
		is_open = false;
		out.close();
	}

	void fill(double rgb, double a = 1.0) {
		fill(rgb, rgb, rgb, a);
	}

	void fill(double r, double g, double b, double a = 1.0) {
		write_instruction("f", {r, g, b, a}, {});
	}

	void stroke(double rgb, double a = 1.0) {
		stroke(rgb, rgb, rgb, a);
	}

	void stroke(double r, double g, double b, double a = 1.0) {
		write_instruction("s", {r, g, b, a}, {});
	}

	void no_fill() {
		write_instruction("nf", {}, {});
	}

	void no_stroke() {
		write_instruction("ns", {}, {});
	}

	void comment(string text) {
		write_instruction("cm", {}, {text});
	}

	void tooltip(string text) {
		write_instruction("tt", {}, {text});
	}

	void no_tooltip() {
		write_instruction("nt", {}, {});
	}

	void stroke_weight(double weight) {
		write_instruction("sw", {weight}, {});
	}

	void text_size(double size) {
		write_instruction("ts", {size}, {});
	}

	void text_align(int align) {
		write_instruction("ta", {(double) align}, {});
	}

	void rect(double x, double y, double w, double h) {
		write_instruction("r", {x, y, w, h}, {});
	}

	void circle(double x, double y, double r) {
		write_instruction("c", {x, y, r}, {});
	}

	void ellipse(double x, double y, double rx, double ry) {
		write_instruction("e", {x, y, rx, ry}, {});
	}

	void line(double x1, double y1, double x2, double y2) {
		write_instruction("l", {x1, y1, x2, y2}, {});
	}

	void text(string str, double x, double y) {
		write_instruction("t", {x, y}, {str});
	}

	void transform(double e00, double e01, double e10, double e11) {
		write_instruction("tf", {e00, e01, e10, e11}, {});
	}

	void translate(double tx, double ty) {
		write_instruction("tr", {tx, ty}, {});
	}

	void rotate(double ang) {
		write_instruction("ro", {ang}, {});
	}

	void scale(double s) {
		scale(s, s);
	}

	void scale(double sx, double sy) {
		write_instruction("sc", {sx, sy}, {});
	}

	void push() {
		write_instruction("pu", {}, {});
	}

	void pop() {
		write_instruction("po", {}, {});
	}

	void end_frame() {
		write_instruction("n", {}, {});
	}

	void target(string name) {
		write_instruction("tg", {}, {name});
	}
};

movie mov;

// --------------------------------------------------
// main part
// --------------------------------------------------

bool isLocal = false;
bool render = false;

// define N and stuff here
constexpr int W = 1000;
constexpr int MIN_D = 5;
constexpr int MAX_D = 50;
constexpr int MIN_N = 5;
constexpr int MAX_N = 50;
constexpr int INF = 10000000;
constexpr int TL1 = 500;
constexpr int MAX_TL1 = 2200;
constexpr int TL2 = 2500;
constexpr int TL3 = 2900;
#define repd(i) rep0(i, d)
#define repn(i) rep0(i, n)

class Problem {
private:
public:
	int d = 0;
	int n = 0;
	int as[MAX_D][MAX_N] = {};

	void load(istream& in) {
		int w;
		in >> w >> d >> n;
		assert(w == W);
		assert(d >= MIN_D && d <= MAX_D);
		assert(n >= MIN_N && n <= MAX_N);
		bool partial = false; // for test purpose
		bool first = false;
		if (partial) {
			int dmid = d / 2;
			repd(i) {
				repn(j) {
					in >> as[i][j];
				}
			}
			if (first) {
				d = dmid;
			} else {
				d = d - dmid;
				repd(i) {
					repn(j) {
						as[i][j] = as[dmid + i][j];
					}
				}
			}
		} else {
			repd(i) {
				repn(j) {
					in >> as[i][j];
				}
			}
		}
	}
};

constexpr int getHeight(int area, int width) {
	return (int) ((area + width - 1) / (double) width);
}

struct Entry { // entry of reservation
	int idx;
	int area;
	vector<int> heights; // height for each strip

	Entry(int idx = -1, int area = 0) : idx(idx), area(area) {
	}

	void initHeights(const vector<int>& widths) {
		heights.clear();
		for (int w : widths) {
			heights.push_back(getHeight(area, w));
		}
	}
};

struct Day {
	vector<Entry> es;
};

struct Block { // instantiated reservation in a strip
	int idx; // entry index
	int height;
	int pos;

	Block(int idx = -1, int height = 0, int pos = 0) : idx(idx), height(height), pos(pos) {
	}

	int start() const {
		return pos;
	}

	int end() const {
		return pos + height;
	}

	bool operator==(const Block& b) const {
		return idx == b.idx;
	}
};

using Range = array<short, 2>;

int rangeToInt(Range range) {
	return *reinterpret_cast<int*>(&range);
}

Range intToRange(int a) {
	return *reinterpret_cast<Range*>(&a);
}

struct Wall {
	int day = -1;
	int div = -1;
	Range origRange = {};
	Range range = {};
	Wall* parent = nullptr;
	Wall* child = nullptr;

	void init(int day, int div, int start, int end) {
		this->day = day;
		this->div = div;
		origRange = {(short) start, (short) end};
		range = {(short) start, (short) end};
		parent = nullptr;
		child = nullptr;
	}
};

struct Strip {
	int width = 0;
	int room = W;
	vector<Wall*> walls;
	vector<Block> bs;

	void updateRoom() {
		room = W;
		for (auto& b : bs) {
			room -= b.height;
		}
	}

	void initialize() {
		room = W;
		for (auto& b : bs)
			room -= b.height;
		int num = len(bs);
		if (num == 0)
			return;
		if (num == 1) {
			bs[0].pos = W - bs[0].height;
			return;
		}
		ranges::sort(bs, [](auto& a, auto& b) {
			return a.height < b.height;
		});
		auto& first = bs.front();
		auto& last = bs.back();
		first.pos = 0;
		last.pos = W - last.height;
		static vector<int> ps;
		ps.clear();
		rep0(i, num - 2) {
			ps.push_back((i + 1) * room / (num - 1)); // evenly space between blocks
		}
		ranges::sort(ps);
		int offset = first.height;
		rep(i, 1, num - 1) {
			int p = ps[i - 1];
			auto& b = bs[i];
			b.pos = offset + p;
			assert(b.end() <= W);
			offset += b.height;
		}
	}
};

struct DayState {
	vector<Strip> ss;
};

constexpr int MAX_WALLS = MAX_D * MAX_N;

class WallPool {
public:
	Wall* pick(int day, int div, int start, int end) {
		assert(wallIdx < MAX_WALLS);
		if (wallPool.empty()) {
			int index = wallIdx++;
			auto res = walls + index;
			res->init(day, div, start, end);
			return res;
		}
		auto res = wallPool.pop_back();
		res->init(day, div, start, end);
		return res;
	}

	void pool(Wall* wall) {
		wallPool.push_back(wall);
	}

	int toIndex(Wall* wall) const {
		return (int) (wall - walls);
	}

	Wall* fromIndex(int index) {
		return walls + index;
	}

	void fixPointers(const WallPool& orig) {
		for (auto& w : wallPool) {
			w = fromIndex(orig.toIndex(w));
		}
	}

private:
	Wall walls[MAX_WALLS] = {};
	int wallIdx = 0;
	easy_stack<Wall*> wallPool;
};

class State {
public:
	vector<DayState> days;
	bool ignore[MAX_D] = {};
	int cost = 1;
	bool allowEmpty = false;

	void clearHistory() {
		his.clear();
	}

	void fixPointers(const State& orig) {
		walls.fixPointers(orig.walls);
		auto fix = [&](Wall*& wall) {
			wall = walls.fromIndex(orig.walls.toIndex(wall));
		};
		for (auto& day : days) {
			for (auto& s : day.ss) {
				for (auto& w : s.walls) {
					fix(w);
					if (w->parent)
						fix(w->parent);
					if (w->child)
						fix(w->child);
				}
			}
		}
	}

	int aDay() {
		int d = len(days);
		repd(day) {
			if (!ignore[day])
				return day;
		}
		return -1;
	}

	void mixStrips(vector<Day>& dayInfos, rngen& rng) {
		static vector<int> idxs1List[MAX_D];
		static vector<int> idxs2List[MAX_D];
		int count = 0;
		int day0 = aDay();
		rep0(iter, 1000) {
			int divs = len(days[day0].ss);
			int div1;
			int div2;
			int width1;
			int width2;
			div1 = rng.next_int(divs);
			div2 = rng.next_int(divs - 1);
			if (div2 >= div1)
				div2++;
			width1 = days[day0].ss[div1].width;
			width2 = days[day0].ss[div2].width;
			if (width1 > width2) {
				swap(div1, div2);
				swap(width1, width2);
			}
			int widthSum = width1 + width2;
			int w1 = max(1, (int) (rng.next_float(0.01, 0.5) * widthSum));
			int w2 = widthSum - w1;
			if (w1 == width1)
				continue;
			auto heightsOf = [&](int day, int idx) -> array<int, 2> {
				int area = dayInfos[day].es[idx].area;
				return {(int) ((area + w1 - 1) / (double) w1), (int) ((area + w2 - 1) / (double) w2)};
			};
			int d = len(days);
			bool ng = false;
			repd(day) {
				if (ignore[day])
					continue;
				auto& s1 = days[day].ss[div1];
				auto& s2 = days[day].ss[div2];
				static vector<int> idxs;
				idxs.clear();
				for (auto& b : s1.bs) {
					idxs.push_back(b.idx);
				}
				for (auto& b : s2.bs) {
					idxs.push_back(b.idx);
				}
				shuffle(idxs, rng);
				static fast_iset<MAX_N> idxs1;
				static fast_iset<MAX_N> idxs2;
				idxs1.clear();
				idxs2.clear();
				int num = len(idxs);
				int room1 = W;
				rep0(i, num) {
					int idx = idxs[i];
					int height = heightsOf(day, idx)[0];
					if (room1 < height)
						break;
					idxs1.insert(idx);
					room1 -= height;
				}
				int room2 = W;
				rep(i, len(idxs1), num) {
					int idx = idxs[i];
					int height = heightsOf(day, idx)[1];
					idxs2.insert(idx);
					room2 -= height;
				}
				if (!(room1 >= 0 && room2 >= 0 && (allowEmpty || !idxs1.empty() && !idxs2.empty()))) {
					bool ok = false;
					rep0(iter, 100) {
						if (idxs2.empty() || !idxs1.empty() && room1 < room2) {
							int idx = random_pick(idxs1, rng);
							auto [h1, h2] = heightsOf(day, idx);
							idxs1.erase(idx);
							idxs2.insert(idx);
							room1 += h1;
							room2 -= h2;
						} else {
							int idx = random_pick(idxs2, rng);
							auto [h1, h2] = heightsOf(day, idx);
							idxs2.erase(idx);
							idxs1.insert(idx);
							room2 += h2;
							room1 -= h1;
						}
						if (room1 >= 0 && room2 >= 0 && (allowEmpty || !idxs1.empty() && !idxs2.empty())) {
							ok = true;
							break;
						}
					}
					if (!ok) {
						ng = true;
						break;
					}
				}
				idxs1List[day].clear();
				idxs2List[day].clear();
				for (int idx : idxs1)
					idxs1List[day].push_back(idx);
				for (int idx : idxs2)
					idxs2List[day].push_back(idx);
			}
			if (ng)
				continue;
			// trace("!!! ", width1, " + ", width2, " -> ", w1, " + ", w2);
			repd(day) {
				if (ignore[day])
					continue;
				auto& s1 = days[day].ss[div1];
				auto& s2 = days[day].ss[div2];
				removeWalls(day, div1);
				removeWalls(day, div2);
			}
			repd(day) {
				if (ignore[day])
					continue;
				auto& s1 = days[day].ss[div1];
				auto& s2 = days[day].ss[div2];
				s1.width = w1;
				s2.width = w2;
				s1.bs.clear();
				s2.bs.clear();
			}
			repd(day) {
				if (ignore[day])
					continue;
				auto& s1 = days[day].ss[div1];
				auto& s2 = days[day].ss[div2];
				auto& es = dayInfos[day].es;
				int n = len(es);
				repn(idx) {
					auto [h1, h2] = heightsOf(day, idx);
					es[idx].heights[div1] = h1;
					es[idx].heights[div2] = h2;
					assert(h1 * w1 >= es[idx].area);
					assert(h2 * w2 >= es[idx].area);
				}
				for (int idx : idxs1List[day]) {
					s1.bs.emplace_back(idx, es[idx].heights[div1], 0);
				}
				for (int idx : idxs2List[day]) {
					s2.bs.emplace_back(idx, es[idx].heights[div2], 0);
				}
				s1.updateRoom();
				s2.updateRoom();
				assert(s1.room >= 0);
				assert(s2.room >= 0);
				justShuffleStrip(day, div1, rng);
				justShuffleStrip(day, div2, rng);
			}
			return;
		}
	}

	void sanityCheck(const vector<Day>& dayInfos) {
		int d = len(dayInfos);
		repd(day) {
			sanityCheck(day, dayInfos[day]);
		}
	}

	void sanityCheck(int day, const Day& dayInfo) {
		int divs = len(days[day].ss);
		rep0(div, divs) {
			auto& s = days[day].ss[div];
			for (auto& b : s.bs) {
				Range r = {(short) b.start(), (short) b.end()};
				for (auto wall : s.walls) {
					auto r2 = intersection(r, wall->origRange);
					if (r2[0] < r2[1]) {
						trace(r[0], " ", r[1], " ", wall->origRange[0], " ", wall->origRange[1]);
					}
					assert(r2[0] >= r2[1]);
				}
				assert(b.height * s.width >= dayInfo.es[b.idx].area);
			}
			for (auto wall : s.walls) {
				assert(wall->day == day);
				assert(wall->div == div);
			}
		}
	}

	void randomSwap(int day, const Day& dayInfo, rngen& rng, double tolerance) {
		auto& ss = days[day].ss;
		int divs = len(ss);
		rep0(iter, 100) {
			int div1 = rng.next_int(divs);
			int div2 = rng.next_int(divs - 1);
			if (div2 >= div1)
				div2++;
			auto& s1 = ss[div1];
			auto& s2 = ss[div2];
			if (allowEmpty && (s1.bs.empty() || s2.bs.empty()))
				continue;
			auto b1 = random_pick(s1.bs, rng);
			auto b2 = random_pick(s2.bs, rng);
			int idx1 = b1.idx;
			int idx2 = b2.idx;
			int newHeight1 = dayInfo.es[idx1].heights[div2];
			int newHeight2 = dayInfo.es[idx2].heights[div1];
			int diff1 = newHeight2 - b1.height;
			int diff2 = newHeight1 - b2.height;
			if (diff1 > s1.room || diff2 > s2.room)
				continue;
			int pcost = cost;
			int hisPos = his.size();
			int p1 = b1.pos;
			int p2 = b2.pos;

			static vector<Block> tmpBs1;
			static vector<Block> tmpBs2;

			// shuffle is each room is decreased
			// otherwise keep wall positions the same
			bool shuffle1 = diff1 > 0;
			bool shuffle2 = diff2 > 0;

			if (shuffle1)
				tmpBs1 = s1.bs;
			if (shuffle2)
				tmpBs2 = s2.bs;

			// replace blocks
			erase(s1.bs, b1);
			erase(s2.bs, b2);
			s1.bs.emplace_back(idx2, newHeight2, p1);
			s2.bs.emplace_back(idx1, newHeight1, p2);

			if (shuffle1) {
				justShuffleStrip(day, div1, rng);
			} else {
				// expand the wall just below b1
				Wall* wallToExpand = nullptr;
				for (auto w : s1.walls) {
					if (w->origRange[0] == b1.end()) {
						wallToExpand = w;
						break;
					}
				}
				if (wallToExpand) {
					auto r = wallToExpand->origRange;
					removeWall(wallToExpand);
					addWall(day, div1, r[0] + diff1, r[1]);
				}
			}

			if (shuffle2) {
				justShuffleStrip(day, div2, rng);
			} else {
				// expand the wall just below b2
				Wall* wallToExpand = nullptr;
				for (auto w : s2.walls) {
					if (w->origRange[0] == b2.end()) {
						wallToExpand = w;
						break;
					}
				}
				if (wallToExpand) {
					auto r = wallToExpand->origRange;
					removeWall(wallToExpand);
					addWall(day, div2, r[0] + diff2, r[1]);
				}
			}
			int ncost = cost;
			if (ncost > pcost + tolerance) {
				// bruh
				while (his.size() > hisPos) {
					undoLastEvent();
				}
				if (shuffle1) {
					s1.bs = tmpBs1;
				} else {
					s1.bs.pop_back();
					s1.bs.push_back(b1);
				}
				if (shuffle2) {
					s2.bs = tmpBs2;
				} else {
					s2.bs.pop_back();
					s2.bs.push_back(b2);
				}
				assert(cost == pcost);
				return;
			}
			s1.room -= diff1;
			s2.room -= diff2;
			return;
		}
	}

	void randomFlip(rngen& rng, double tolerance) {
		int d = len(days);
		int divs = len(days[0].ss);

		auto flipBetween = [&](int begin, int end, int div) {
			rep(day, begin, end) {
				auto& s = days[day].ss[div];
				for (auto wall : s.walls) {
					auto r = wall->origRange;
					wall->origRange = Range({W - r[1], W - r[0]});
					r = wall->range;
					wall->range = Range({W - r[1], W - r[0]});
				}
			}
		};

		int dbegin, dend;
		do {
			dbegin = rng.next_int(d);
			dend = rng.next_int(d);
			if (dend < dbegin)
				swap(dbegin, dend);
			dend++;
		} while (dbegin + 1 == dend);
		int div = rng.next_int(divs);
		int pcost = cost;
		int hisPos1 = his.size();

		static vector<Range> ranges[MAX_D];
		static vector<int> borderDays;
		borderDays.clear();
		if (dbegin > 0)
			borderDays.push_back(dbegin);
		if (dend < d)
			borderDays.push_back(dend - 1);
		for (int day : borderDays) {
			ranges[day].clear();
			for (auto wall : days[day].ss[div].walls) {
				ranges[day].push_back(wall->origRange);
			}
			removeWalls(day, div);
		}

		int fbegin = dbegin == 0 ? 0 : dbegin + 1;
		int fend = dend == d ? d : dend - 1;
		flipBetween(fbegin, fend, div);

		int hisPos2 = his.size();

		for (int day : borderDays) {
			assert(days[day].ss[div].walls.empty());
			for (auto range : ranges[day]) {
				addWall(day, div, W - range[1], W - range[0]);
			}
		}

		int ncost = cost;
		if (ncost > pcost + tolerance) {
			// bruh
			while (his.size() > hisPos2) {
				undoLastEvent();
			}
			flipBetween(fbegin, fend, div);
			while (his.size() > hisPos1) {
				undoLastEvent();
			}
			assert(cost == pcost);
			return;
		}
		rep(day, dbegin, dend) {
			auto& s = days[day].ss[div];
			for (auto& b : s.bs) {
				b.pos = W - b.end();
			}
		}
	}

	void randomMove(int day, const Day& dayInfo, rngen& rng, double tolerance) {
		auto& ss = days[day].ss;
		int divs = len(ss);
		rep0(iter, 100) {
			int div1 = rng.next_int(divs);
			auto& s1 = ss[div1];
			if (len(s1.bs) <= (allowEmpty ? 0 : 1))
				continue;
			int div2 = rng.next_int(divs - 1);
			if (div2 >= div1)
				div2++;
			auto& s2 = ss[div2];
			auto b1 = random_pick(s1.bs, rng);
			int idx1 = b1.idx;
			int newHeight1 = dayInfo.es[idx1].heights[div2];
			int diff1 = -b1.height;
			int diff2 = newHeight1;
			if (diff2 > s2.room)
				continue;
			static vector<Block> tmpBs1;
			static vector<Block> tmpBs2;
			tmpBs1 = s1.bs;
			tmpBs2 = s2.bs;
			int pcost = cost;
			int hisPos = his.size();
			erase(s1.bs, b1);
			s2.bs.emplace_back(idx1, newHeight1);

			Wall* wallAbove = nullptr;
			Wall* wallBelow = nullptr;
			for (auto w : s1.walls) {
				if (w->origRange[1] == b1.start())
					wallAbove = w;
				if (w->origRange[0] == b1.end())
					wallBelow = w;
			}
			if (!wallAbove) {
				for (auto w : s1.walls) {
					assert(w->origRange[0] >= b1.start());
				}
			}
			if (wallAbove) {
				auto r1 = wallAbove->origRange;
				if (wallBelow) {
					auto r2 = wallBelow->origRange;
					removeWall(wallAbove);
					removeWall(wallBelow);
					addWall(day, div1, r1[0], r2[1]);
				} else {
					removeWall(wallAbove);
				}
			} else {
				if (wallBelow) {
					removeWall(wallBelow);
				} else {
					assert(("no wall", allowEmpty));
				}
			}

			justShuffleStrip(day, div2, rng);

			int ncost = cost;
			if (ncost > pcost + tolerance) {
				// bruh
				while (his.size() > hisPos) {
					undoLastEvent();
				}
				// s1.bs.push_back(b1);
				s1.bs = tmpBs1;
				s2.bs = tmpBs2;
				assert(cost == pcost);
				return;
			}
			s1.room -= diff1;
			s2.room -= diff2;
			return;
		}
	}

	void randomMix(int day, const Day& dayInfo, rngen& rng, double tolerance) {
		auto& ss = days[day].ss;
		int divs = len(ss);
		rep0(iter, 100) {
			int div1 = rng.next_int(divs);
			int div2 = rng.next_int(divs - 1);
			if (div2 >= div1)
				div2++;
			auto& s1 = ss[div1];
			auto& s2 = ss[div2];
			if (allowEmpty && (s1.bs.empty() || s2.bs.empty()))
				continue;
			static vector<int> idxs;
			idxs.clear();
			for (auto& b : s1.bs)
				idxs.push_back(b.idx);
			for (auto& b : s2.bs)
				idxs.push_back(b.idx);
			shuffle(idxs, rng);
			int room1 = W;
			int num = len(idxs);
			int num1 = 0;
			rep0(i, num - 1) {
				int idx = idxs[i];
				int height = dayInfo.es[idx].heights[div1];
				if (num1 == num - 1 || room1 < height)
					break;
				num1++;
				room1 -= height;
			}
			if (num1 == 0)
				continue;
			int room2 = W;
			int num2 = 0;
			rep(i, num1, num) {
				int idx = idxs[i];
				int height = dayInfo.es[idx].heights[div2];
				num2++;
				room2 -= height;
			}
			if (room2 < 0)
				continue;

			static vector<Block> tmpBs1;
			static vector<Block> tmpBs2;
			tmpBs1 = s1.bs;
			tmpBs2 = s2.bs;
			int pcost = cost;
			int hisPos = his.size();
			s1.bs.clear();
			s2.bs.clear();
			rep0(i, num1) {
				int idx = idxs[i];
				s1.bs.emplace_back(idx, dayInfo.es[idx].heights[div1]);
			}
			rep0(i, num2) {
				int idx = idxs[num1 + i];
				s2.bs.emplace_back(idx, dayInfo.es[idx].heights[div2]);
			}
			justShuffleStrip(day, div1, rng);
			justShuffleStrip(day, div2, rng);
			int ncost = cost;
			if (ncost > pcost + tolerance) {
				// bruh
				while (his.size() > hisPos) {
					undoLastEvent();
				}
				s1.bs = tmpBs1;
				s2.bs = tmpBs2;
				assert(cost == pcost);
				return;
			}
			s1.room = room1;
			s2.room = room2;
			return;
		}
	}

	void justShuffleStrip(int day, int div, rngen& rng) {
		auto& s = days[day].ss[div];
		auto& bs = s.bs;

		removeWalls(day, div);

		int room = W;
		int num = len(bs);
		if (num <= 1) {
			if (num == 1) {
				auto& b = bs.back();
				b.pos = W - b.height;
			}
			return;
		}

		shuffle(bs, rng);
		auto& first = bs.front();
		auto& last = bs.back();
		first.pos = 0;
		last.pos = W - last.height;
		for (auto& b : bs) {
			room -= b.height;
		}
		assert(room >= 0);
		static vector<int> ps;
		ps.clear();
		rep0(i, num - 2) {
			ps.push_back(rng.next_int(room + 1));
		}
		ranges::sort(ps);
		int offset = first.height;
		rep(i, 1, num - 1) {
			int p = ps[i - 1];
			auto& b = bs[i];
			b.pos = offset + p;
			assert(b.end() <= W);
			offset += b.height;
		}
		rep(i, 1, num) {
			auto& pb = s.bs[i - 1];
			auto& nb = s.bs[i];
			int start = pb.end();
			int end = nb.start();
			assert(start <= end);
			addWall(day, div, start, end);
		}
	}

	void shuffleStrip(int day, rngen& rng, double tolerance) {
		static vector<Block> tmpBs;

		int div = rng.next_int(len(days[aDay()].ss));
		auto& s = days[day].ss[div];
		auto& bs = s.bs;

		int room = W;
		int num = len(bs);
		if (num <= 1) {
			if (num == 1) {
				auto& b = bs.back();
				b.pos = W - b.height;
			}
			return;
		}

		tmpBs = bs;
		shuffle(bs, rng);
		auto& first = bs.front();
		auto& last = bs.back();
		first.pos = 0;
		last.pos = W - last.height;
		for (auto& b : bs) {
			room -= b.height;
		}
		static vector<int> ps;
		ps.clear();
		rep0(i, num - 2) {
			ps.push_back(rng.next_int(room + 1));
		}
		ranges::sort(ps);
		int offset = first.height;
		rep(i, 1, num - 1) {
			int p = ps[i - 1];
			auto& b = bs[i];
			b.pos = offset + p;
			assert(b.end() <= W);
			offset += b.height;
		}

		int pcost = cost;

		int hisPos = his.size();

		removeWalls(day, div);

		// assume bs is sorted
		rep(i, 1, num) {
			auto& pb = s.bs[i - 1];
			auto& nb = s.bs[i];
			int start = pb.end();
			int end = nb.start();
			assert(start <= end);
			addWall(day, div, start, end);
		}

		int ncost = cost;

		if (ncost > pcost + tolerance) {
			// bruh
			bs = tmpBs;
			while (his.size() > hisPos) {
				undoLastEvent();
			}
			assert(cost == pcost);
			return;
		}
	}

	vector<int> wallPoss(int day, int div) {
		set<int> poss;
		auto& s = days[day].ss[div];
		for (auto wall : s.walls) {
			auto range = wall->range;
			auto w = wall->child;
			while (w) {
				range = intersection(range, w->range);
				w = w->child;
			}
			poss.insert(range[0] + range[1] >> 1);
		}
		vector<int> res;
		for (int pos : poss) {
			res.push_back(pos);
		}
		return res;
	}

	void addWall(int day, int div, int start, int end) {
		auto& s = days[day].ss[div];
		auto wall = walls.pick(day, div, start, end);
		assert(!wall->parent && !wall->child);

		// record add event
		his.push_back(walls.toIndex(wall));
		his.push_back(OP_ADD);

		cost += ((day > 0) + (day < len(days) - 1)) * s.width;
		assert(ranges::count(s.walls, wall) == 0);
		s.walls.push_back(wall);
		assert(!wall->child);
		findParent(wall);
		assert(!wall->child);
		findChild(wall);
	}

	void removeWall(Wall* wall) {
		int day = wall->day;
		int div = wall->div;
		auto& s = days[day].ss[div];
		if (wall->parent) // unlink parent first because this might update wall->child
			unlink(wall->parent, wall, false);
		if (wall->child)
			unlink(wall, wall->child, false);
		assert(!wall->child);
		assert(!wall->parent);

		// record remove event
		his.push_back(day);
		his.push_back(div);
		his.push_back(rangeToInt(wall->range));
		his.push_back(OP_REMOVE);

		cost -= ((day > 0) + (day < len(days) - 1)) * s.width;
		assert(ranges::count(s.walls, wall) == 1);
		erase(s.walls, wall);
		walls.pool(wall);
	}

	void initWalls() {
		int d = len(days);
		repd(day) {
			if (ignore[day])
				continue;
			for (auto& s : days[day].ss) {
				for (auto wall : s.walls) {
					walls.pool(wall);
				}
				s.walls.clear();
			}
		}
		his.clear();
		repd(day) {
			if (ignore[day])
				continue;
			// trace("day ", day);
			auto& ss = days[day].ss;
			int divs = len(ss);
			rep0(div, divs) {
				// trace("div ", div);
				auto& s = ss[div];
				int num = len(s.bs);
				// assume bs is sorted
				rep(i, 1, num) {
					auto& pb = s.bs[i - 1];
					auto& nb = s.bs[i];
					int start = pb.end();
					int end = nb.start();
					// trace("adding wall ", start, " ", end);
					assert(start <= end);
					addWall(day, div, start, end);
				}
			}
		}
	}

	void renderStrips() {
		int divs = len(days[aDay()].ss);
		int d = len(days);
		double scale = 50.0 / W;
		constexpr double EPS = 1e-2;
		rep0(div, divs) {
			mov.target("strip " + tos(div));
			repd(day) {
				if (ignore[day])
					continue;
				mov.no_stroke();
				mov.fill(0.2);
				auto& s = days[day].ss[div];
				double width = 1.0;
				double x = day * width * 1.2;
				for (auto& b : s.bs) {
					int st = b.start();
					int en = b.end();
					double y1 = st * scale;
					double y2 = en * scale;
					mov.tooltip("idx=" + tos(b.idx) + " area=" + tos(b.height * s.width));
					mov.rect(x, y1, width, y2 - y1);
				}
				for (auto w : s.walls) {
					mov.fill(1, 0, 0);
					double y1 = w->origRange[0] * scale;
					double y2 = w->origRange[1] * scale;
					mov.rect(x, y1 - EPS, width, y2 - y1 + EPS * 2);
					if (w->child || w->parent) {
						mov.fill(0, 1, 0);
						double y1 = w->range[0] * scale;
						double y2 = w->range[1] * scale;
						mov.rect(x, y1 - EPS, width, y2 - y1 + EPS * 2);
					}
				}
			}
			mov.end_frame();
		}
		mov.target("all");
		repd(day) {
			if (ignore[day])
				continue;
			double x = 0;
			rep0(div, divs) {
				mov.no_stroke();
				mov.fill(0.2);
				auto& s = days[day].ss[div];
				double width = s.width * scale;
				for (auto& b : s.bs) {
					int st = b.start();
					int en = b.end();
					double y1 = st * scale;
					double y2 = en * scale;
					mov.rect(x, y1, width, y2 - y1);
				}
				for (auto w : s.walls) {
					mov.fill(1, 0, 0);
					double y1 = w->origRange[0] * scale;
					double y2 = w->origRange[1] * scale;
					mov.rect(x, y1 - EPS, width, y2 - y1 + EPS * 2);
					if (w->child || w->parent) {
						mov.fill(0, 1, 0);
						double y1 = w->range[0] * scale;
						double y2 = w->range[1] * scale;
						mov.rect(x, y1 - EPS, width, y2 - y1 + EPS * 2);
					}
				}
				x += (s.width + 0.002 * W) * scale;
			}
			mov.end_frame();
		}
	}

private:
	WallPool walls;
	easy_stack<int> his;

	static constexpr int OP_LINK = 0x10000; // parent, child
	static constexpr int OP_UNLINK = 0x10001; // parent, child
	static constexpr int OP_ADD = 0x10002; // wall, day, div, range
	static constexpr int OP_REMOVE = 0x10003; // wall, day, div, range
	static constexpr int OP_RANGE = 0x10004; // wall, from, to

	string dumpWall(Wall* wall) {
		return "wall(index=" + tos(walls.toIndex(wall)) + " range=[" + tos(wall->range[0]) + "," +
		    tos(wall->range[1]) + "] origRange=[" + tos(wall->origRange[0]) + "," + tos(wall->origRange[1]) +
		    "] parent=" + tos(wall->parent ? walls.toIndex(wall->parent) : -1) +
		    " child=" + tos(wall->child ? walls.toIndex(wall->child) : -1) + ")";
	}

	void removeWalls(int day, int div) {
		static vector<Wall*> tmp;
		tmp = days[day].ss[div].walls;
		for (auto wall : tmp) {
			removeWall(wall);
		}
	}

	void undoLastEvent() {
		auto pop = [&]() {
			return his.pop_back();
		};
		int kind = pop();
		switch (kind) {
		case OP_LINK: {
			// trace("undo link");
			auto child = walls.fromIndex(pop());
			auto parent = walls.fromIndex(pop());
			// trace("parent: ", dumpWall(parent));
			// trace("child: ", dumpWall(child));
			assert(child->parent == parent && parent->child == child);
			child->parent = nullptr;
			parent->child = nullptr;
			assert(days[parent->day].ss[parent->div].width == days[child->day].ss[child->div].width);
			cost += 2 * days[parent->day].ss[parent->div].width;
		} break;
		case OP_UNLINK: {
			// trace("undo unlink");
			auto child = walls.fromIndex(pop());
			auto parent = walls.fromIndex(pop());
			// trace("parent: ", dumpWall(parent));
			// trace("child: ", dumpWall(child));
			assert(!child->parent && !parent->child);
			child->parent = parent;
			parent->child = child;
			assert(days[parent->day].ss[parent->div].width == days[child->day].ss[child->div].width);
			cost -= 2 * days[parent->day].ss[parent->div].width;
		} break;
		case OP_ADD: {
			// trace("undo add");
			auto wall = walls.fromIndex(pop());
			// trace("wall: ", dumpWall(wall));
			int day = wall->day;
			int div = wall->div;
			auto& s = days[day].ss[div];
			cost -= ((day > 0) + (day < len(days) - 1)) * s.width;
			assert(ranges::count(s.walls, wall) == 1);
			erase(s.walls, wall);
			walls.pool(wall);
		} break;
		case OP_REMOVE: {
			// trace("undo remove");
			Range range = intToRange(pop());
			int div = pop();
			int day = pop();
			auto wall = walls.pick(day, div, range[0], range[1]);
			// trace("wall: ", dumpWall(wall));
			auto& s = days[day].ss[div];
			cost += ((day > 0) + (day < len(days) - 1)) * s.width;
			assert(ranges::count(s.walls, wall) == 0);
			s.walls.push_back(wall);
		} break;
		case OP_RANGE: {
			// trace("undo range");
			Range from = intToRange(pop());
			auto wall = walls.fromIndex(pop());
			wall->range = from;
		} break;
		default: {
			trace("unknown event: ", kind);
			assert(false);
		}
		}
	}

	void setRange(Wall* wall, Range range) {
		// record range event
		his.push_back(walls.toIndex(wall));
		his.push_back(rangeToInt(wall->range));
		his.push_back(OP_RANGE);

		wall->range = range;
	}

	void link(Wall* parent, Wall* child) {
		assert(!parent->child && !child->parent);
		assert(child->range == child->origRange);
		assert(intersect(parent->range, child->range));

		// record link event
		his.push_back(walls.toIndex(parent));
		his.push_back(walls.toIndex(child));
		his.push_back(OP_LINK);

		parent->child = child;
		child->parent = parent;
		assert(abs(parent->day - child->day) == 1);
		assert(parent->div == child->div);
		assert(days[parent->day].ss[parent->div].width == days[child->day].ss[child->div].width);
		cost -= 2 * days[parent->day].ss[parent->div].width;

		auto range = intersection(parent->range, child->range);
		if (range != child->range) {
			setRange(child, range);
			onShrink(child);
		}
	}

	void unlink(Wall* parent, Wall* child, bool findNewChild) {
		assert(parent->child == child && child->parent == parent);
		// trace("unlinked! ", parent->origRange, " -> ", child->origRange);

		// record unlink event
		his.push_back(walls.toIndex(parent));
		his.push_back(walls.toIndex(child));
		his.push_back(OP_UNLINK);

		parent->child = nullptr;
		child->parent = nullptr;
		assert(days[parent->day].ss[parent->div].width == days[child->day].ss[child->div].width);
		cost += 2 * days[parent->day].ss[parent->div].width;

		if (child->range != child->origRange) {
			setRange(child, child->origRange);
			onExpand(child);
		}
		if (findNewChild) {
			findChild(parent);
		}
	}

	void onShrink(Wall* wall) {
		auto child = wall->child;
		if (child) {
			auto range = intersection(wall->range, child->range);
			if (range[0] > range[1]) {
				unlink(wall, child, true);
			} else if (range != child->range) {
				setRange(child, range);
				onShrink(child);
			}
		}
	}

	void onExpand(Wall* wall) {
		auto child = wall->child;
		if (child) {
			auto range = intersection(wall->range, child->origRange);
			if (range != child->range) {
				setRange(child, range);
				onExpand(child);
			}
		} else {
			findChild(wall);
		}
	}

	void findChild(Wall* wall) {
		assert(!wall->child);
		if (wall->day == len(days) - 1 || ignore[wall->day + 1])
			return;
		auto& ns = days[wall->day + 1].ss[wall->div];
		assert(days[wall->day].ss[wall->div].width == ns.width);
		// TODO: randomize?
		Wall* child = nullptr;
		for (auto c : ns.walls) {
			if (!c->parent && intersect(wall->range, c->range)) {
				child = c;
				break;
			}
		}
		if (child) {
			link(wall, child);
		}
	}

	void findParent(Wall* wall) {
		assert(!wall->parent);
		if (wall->day == 0 || ignore[wall->day - 1])
			return;
		auto& ps = days[wall->day - 1].ss[wall->div];
		assert(days[wall->day].ss[wall->div].width == ps.width);
		// TODO: randomize?
		Wall* parent = nullptr;
		for (auto c : ps.walls) {
			if (!c->child && intersect(wall->range, c->range)) {
				parent = c;
				break;
			}
		}
		assert(parent != wall);
		if (parent) {
			link(parent, wall);
		}
	}

	bool intersect(Range range1, Range range2) const {
		return range1[0] <= range2[1] && range1[1] >= range2[0];
	}

	bool includes(Range range1, Range range2) const {
		return range1[0] <= range2[0] && range1[1] >= range2[1];
	}

	Range intersection(Range range1, Range range2) const {
		return {max(range1[0], range2[0]), min(range1[1], range2[1])};
	}
};

struct Solution {
	vector<vector<array<int, 4>>> rects;

	void init(int d, int n) {
		rects.clear();
		rects.resize(d);
		for (auto& rs : rects) {
			rs.resize(n);
		}
	}
};

class Selector {
private:
	vector<int> opts;
	vector<double> ws;
	double wsum = 0;

public:
	Selector() {
	}

	void clear() {
		opts.clear();
		ws.clear();
		wsum = 0;
	}

	void add(int option, double weight) {
		opts.push_back(option);
		ws.push_back(weight);
		wsum += weight;
	}

	void update(int index, double weight) {
		wsum += weight - ws[index];
		ws[index] = weight;
	}

	int choose(rngen& rng) {
		double f = rng.next_float(0, wsum);
		int n = len(opts);
		rep0(i, n) {
			if ((f -= ws[i]) <= 0) {
				return opts[i];
			}
		}
		return opts.back();
	}

	int size() const {
		return len(opts);
	}
};

struct Params {
	static constexpr int FLP = 0;
	static constexpr int SFL = 1;
	static constexpr int MIX = 2;
	static constexpr int MOV = 3;
	static constexpr int SWP = 4;

	// array<double, 2> flp = {0.1, 0.1};
	// array<double, 2> sfl = {0.27, 0.27};
	// array<double, 2> mix = {0.25, 0.25};
	// array<double, 2> mov = {0.2, 0.2};
	// array<double, 2> swp = {0.2, 0.2};
	array<double, 2> flp = {0.5383353942191178, 0.0948919399747433};
	array<double, 2> sfl = {0.5502435234176464, 0.28975602252128757};
	array<double, 2> mix = {0.9743114751002061, 0.8877099761001839};
	array<double, 2> mov = {0.3111749482246951, 0.6330540373350902};
	array<double, 2> swp = {0.3677380363242754, 0.6515483627126992};

	Params() {
	}
};

struct SolverResult {
	ll score = 0;
};

class Solver {
public:
	int seed;
	rngen rng;
	SolverResult result;
	Problem p;
	Params params;
	int d = 0;
	int n = 0;

	Solver() : rng(12345) {
	}

	void load(istream& in, int seed = -1) {
		p.load(in);
		d = p.d;
		n = p.n;
		mov.set_file(movie_file_name(seed));
		init();
	}

	void load(int seed) {
		this->seed = seed;
		istringstream in(read_text_assert(input_file_name(seed)));
		isLocal = true;
		load(in, seed);
	}

	void solve() {
		if (isLocal) {
			ostringstream out;
			solveMain(out);
			mov.close_file();
			write_text(output_file_name(seed), out.str());
		} else {
			solveMain(cout);
		}
	}

private:
	vector<Day> days;
	State st;
	Solution sol;
	int maxNth[MAX_N] = {};
	vector<double> usageRatios;
	vector<int> sortedByUsageRatios;
	double eps;
	vector<double> epsOfDay;
	static constexpr int RANDOM_TABLE_SIZE = 0x10000;

	void init() {
		days.clear();
		days.resize(d);
		int areaSum = 0;
		int maxArea = 0;
		repd(day) {
			sortedByUsageRatios.push_back(day);
			int areaSumDay = 0;
			repn(idx) {
				int a = p.as[day][idx];
				days[day].es.emplace_back(idx, a);
				maxArea = max(maxArea, a);
				maxNth[idx] = max(maxNth[idx], a);
				areaSumDay += a;
			}
			areaSum += areaSumDay;
			usageRatios.push_back(areaSumDay / (double) (W * W));
			double ratio = areaSumDay / 1e6;
			epsOfDay.push_back(sqrt(1 - ratio));
		}
		ranges::sort(sortedByUsageRatios, [&](int a, int b) {
			return usageRatios[a] > usageRatios[b];
		});

		trace("D=", d, " N=", n);
		double ratio = areaSum / (1e6 * d);
		trace("usage ratio: ", areaSum / (1e6 * d));
		eps = sqrt(1 - ratio);
		trace("estimated eps: ", eps);
		trace("max area: ", maxArea);
	}

	void sa() {
		bool kicksEnabled = d < 20 && n < eps * 150;
		repd(day) {
			if (st.ignore[day])
				continue;
			for (auto& s : st.days[day].ss) {
				s.initialize();
				{
					int a = s.room;
					s.updateRoom();
					assert(a == s.room);
				}
			}
		}
		st.initWalls();

		double progress = 0;
		constexpr double T_FROM = 50;
		constexpr double T_TO = 2;
		double temp = T_FROM;
		int minCost = st.cost;
		trace("initial cost: ", st.cost);

		buildSolution();
		State bestState;
		bestState = st;
		bestState.fixPointers(st);
		int nohit = 0;

		Selector sel;

		sel.add(Params::FLP, params.flp[0]);
		sel.add(Params::SFL, params.sfl[0]);
		sel.add(Params::MIX, params.mix[0]);
		sel.add(Params::MOV, params.mov[0]);
		sel.add(Params::SWP, params.swp[0]);

		auto updateHeights = [&]() {
			vector<int> widths;
			repd(day) {
				if (st.ignore[day])
					continue;
				if (widths.empty()) {
					for (auto& s : st.days[day].ss) {
						widths.push_back(s.width);
					}
				}
				repn(idx) {
					days[day].es[idx].initHeights(widths);
				}
			}
		};

		float tolsBase[0x1000] = {};
		rep0(i, len(tolsBase)) {
			tolsBase[i] = -log2(rng.next_float());
		}
		bool bestBuilt = false;
		bool noIgnore = true;
		repd(day) {
			if (st.ignore[day]) {
				noIgnore = false;
				break;
			}
		}
		double start = timer();
		int end = noIgnore ? TL3 : TL2;
		bool allowEmpty = n <= 25 && eps >= 0.25;
		rep0(iter, 100000000) {
			if ((iter & 0xfff) == 0) {
				progress = linearstep(start, end, timer());
				if (progress >= 1) {
					trace("alert2 ", iter);
					break;
				}
				sel.update(Params::FLP, noIgnore * lerp(params.flp[0], params.flp[1], progress));
				sel.update(Params::SFL, lerp(params.sfl[0], params.sfl[1], progress));
				sel.update(Params::MIX, lerp(params.mix[0], params.mix[1], progress));
				sel.update(Params::MOV, lerp(params.mov[0], params.mov[1], progress));
				sel.update(Params::SWP, lerp(params.swp[0], params.swp[1], progress));
				temp = exp_interp(T_FROM, T_TO, progress);
				if (progress >= 0.8 && allowEmpty) {
					st.allowEmpty = true;
				}
			}
			st.clearHistory();

			int day;
			do {
				day = rng.next_int(d);
			} while (st.ignore[day]);
			double tol = tolsBase[iter & 0xfff] * temp;
			if (kicksEnabled && nohit >= 100000) {
				trace("kick");
				if (rng.next_float() < 0.5) {
					// restore best
					st = bestState;
					st.fixPointers(bestState);
					updateHeights();
				}
				// update strips
				st.mixStrips(days, rng);
				if (allowEmpty)
					st.allowEmpty = true;
				// st.sanityCheck(days);
				nohit = 0;
			} else {
				switch (sel.choose(rng)) {
				case Params::FLP: {
					st.randomFlip(rng, tol);
				} break;
				case Params::SFL: {
					st.shuffleStrip(day, rng, tol);
				} break;
				case Params::MIX: {
					st.randomMix(day, days[day], rng, tol);
				} break;
				case Params::MOV: {
					st.randomMove(day, days[day], rng, tol);
				} break;
				case Params::SWP: {
					st.randomSwap(day, days[day], rng, tol);
				} break;
				}
			}
			nohit++;
			if (update_min(st.cost, minCost)) {
				nohit = 0;
				trace("best updated! ", minCost);
				if (kicksEnabled) {
					bestState = st;
					bestState.fixPointers(st);
				} else if (progress > 0.75) {
					buildSolution();
					bestBuilt = true;
				}
			}
		}
		if (kicksEnabled) {
			st = bestState;
			st.fixPointers(bestState);
			updateHeights();
			if (render)
				st.renderStrips();
			buildSolution();
		} else {
			if (render)
				st.renderStrips();
			if (!bestBuilt)
				buildSolution();
		}
	}

	int computeScore(const Solution& sol) const {
		fast_iset<W * W> hs;
		fast_iset<W * W> vs;
		int score = 1;
		repd(day) {
			fast_iset<W * W> nhs;
			fast_iset<W * W> nvs;
			auto& rects = sol.rects[day];
			int penalty = 0;
			repn(idx) {
				auto& rect = rects[idx];
				int a = p.as[day][idx];
				int i1 = rect[0];
				int j1 = rect[1];
				int i2 = rect[2];
				int j2 = rect[3];
				int area = (i2 - i1) * (j2 - j1);
				if (area < a) {
					penalty += 100 * (a - area);
				}
				rep(i, i1, i2) {
					if (j1 > 0)
						nvs.insert(ivec2(i, j1 - 1).pack(W));
					if (j2 < W)
						nvs.insert(ivec2(i, j2 - 1).pack(W));
				}
				rep(j, j1, j2) {
					if (i1 > 0)
						nhs.insert(ivec2(i1 - 1, j).pack(W));
					if (i2 < W)
						nhs.insert(ivec2(i2 - 1, j).pack(W));
				}
			}
			// tracen("day ", day, " pena=", penalty);
			score += penalty;
			if (day > 0) {
				int l = 0;
				for (int h : hs)
					if (!nhs.contains(h))
						l++;
				for (int v : vs)
					if (!nvs.contains(v))
						l++;
				for (int h : nhs)
					if (!hs.contains(h))
						l++;
				for (int v : nvs)
					if (!vs.contains(v))
						l++;
				score += l;
				// tracen(" L=", l);
			}
			// trace(" score=", score);
			hs = nhs;
			vs = nvs;
		}
		return score;
	};

	pii getMaxTrialsAndKicks(double eps, double scale) const {
		int maxTrials = clamp((int) round(exp(-30.59 * eps + 10.72) * 0.1), 1, 4000);
		int sqrtTrials = (int) round(sqrt(maxTrials));
		int kicksPerTrial = (int) round(maxTrials / (double) sqrtTrials);
		return pii(sqrtTrials, kicksPerTrial);
	}

	vector<pair<int, fast_iset<MAX_N>>> setupStripsOfDay(int day, int tl) {
		array<fast_iset<MAX_N>, MAX_N> plan;

		double randomTable[RANDOM_TABLE_SIZE];
		rep0(i, len(randomTable)) {
			randomTable[i] = rng.next_float();
		}
		int randomIdx = 0;
		bool found = false;

		auto costForWidths = [&](vector<int>& widths) {
			int divs = len(widths);
			rep0(div, divs) {
				if (widths[div] * W < days[day].es[div].area) {
					return INF;
				}
			}
			if (!isPossiblyFeasible(days[day], widths))
				return INF;
			static fast_iset<MAX_N> idxs[MAX_N];
			int res = 0;
			auto [trials, kicks] = getMaxTrialsAndKicks(epsOfDay[day], 1);
			double randomness = trials == 1 ? 0 : 1;
			int cost = INF;
			rep0(iter, trials) {
				rep0(div, divs) {
					idxs[div].clear();
				}
				randomIdx = rng.next_int(RANDOM_TABLE_SIZE);
				cost = roughDivCostOnDay(days[day], widths, idxs, randomness, randomTable, randomIdx, kicks);
				if (cost < INF)
					break;
			}
			if (cost == INF)
				return INF;
			rep0(div, divs) {
				plan[div] = idxs[div];
			}
			return cost + W * (divs - 1);
		};
		vector<int> widths;
		widths.push_back(W);
		vector<int> tmpWidths;
		vector<int> bestWidths = widths;
		int minCost = costForWidths(widths);
		auto bestPlan = plan;

		int cost = minCost;
		int iter = 0;
		auto record = [&](vector<int>& widths, int cost, int kind) {
			if (update_min(cost, minCost)) {
				// trace("best cost for day ", day, " updated! ", minCost, " kind=", kind, " num=",
				// len(widths),
				//     " time=", timer(), " iter=", iter);
				bestWidths = widths;
				bestPlan = plan;
			}
		};
		while (true) {
			iter++;
			if (len(widths) > 1 && !found) {
				found = true;
			}
			if (timer() > tl)
				break;
			tmpWidths = widths;
			if (len(widths) == 1) { // divide into 6-9 randomly
				set<int> ws;
				int num = rng.next_int(6, 9) - 1;
				rep0(i, num) {
					int p = (int) (W * (i + 1 + rng.next_float(-0.2, 0.2)) / (num + 1));
					ws.insert(p);
				}
				ws.insert(W);
				widths.clear();
				int prevW = 0;
				for (int w : ws) {
					widths.push_back(w - prevW);
					prevW = w;
				}
				ranges::sort(widths);
				int newCost = costForWidths(widths);
				if (newCost <= cost) {
					cost = newCost;
					record(widths, cost, 0);
					continue;
				} else {
					widths = tmpWidths;
				}
			}
			if (len(widths) < n) { // split
				int divs = len(widths);
				int i = divs - 1 - (int) (pow(rng.next_float(), 10) * divs);
				int w = widths[i];
				int w1 = max(1, (int) (w * rng.next_float(0.01, 0.5)));
				int w2 = w - w1;
				widths[i] = w1;
				widths.push_back(w2);
				ranges::sort(widths);
				int newCost = costForWidths(widths);
				if (newCost <= cost) {
					cost = newCost;
					record(widths, cost, 1);
					continue;
				} else {
					widths = tmpWidths;
				}
			}
			if (len(widths) >= 2) { // mix
				int divs = len(widths);
				int i = rng.next_int(divs);
				int j = rng.next_int(divs - 1);
				if (j >= i)
					j++;
				int sum = widths[i] + widths[j];
				int w1 = max(1, (int) (sum * rng.next_float(0.01, 0.5)));
				int w2 = sum - w1;
				widths[i] = w1;
				widths[j] = w2;
				ranges::sort(widths);
				int newCost = costForWidths(widths);
				if (newCost <= cost) {
					cost = newCost;
					record(widths, cost, 2);
					continue;
				} else {
					widths = tmpWidths;
				}
			}
		}

		vector<pair<int, fast_iset<MAX_N>>> res;

		if (!found)
			return res; // failed

		// restore best
		widths = bestWidths;
		plan = bestPlan;

		int divs = len(widths);
		rep0(div, divs) {
			res.emplace_back(widths[div], plan[div]);
		}
		return res;
	}

	void setupStrips() {
		struct Plans {
			int idxs[MAX_D][MAX_N][MAX_N];
			int nums[MAX_D][MAX_N];
		};

		Plans plans;

		double randomTable[RANDOM_TABLE_SIZE];
		rep0(i, len(randomTable)) {
			randomTable[i] = rng.next_float();
		}
		int randomIdx = 0;
		bool found = false;
		bool ignore[MAX_D] = {};

		auto costForWidths = [&](vector<int>& widths) {
			int divs = len(widths);
			rep0(div, divs) {
				if (widths[div] * W < maxNth[div]) {
					return INF;
				}
			}
			// bool possiblyFeasible = true;
			double t = pow(linearstep(TL1, MAX_TL1, timer()), 1);
			int MAX_FAILS = found ? 0 : (int) round(t * d * 0.5);
			// int MAX_FAILS = 0;
			int numFails = 0;
			bool failed[MAX_D];
			repd(i) {
				int day = sortedByUsageRatios[i];
				if (ignore[day])
					continue;
				failed[day] = !isPossiblyFeasible(days[day], widths);
				if (failed[day]) {
					if (++numFails > MAX_FAILS)
						return INF;
				}
			}
			static fast_iset<MAX_N> idxs[MAX_N];
			int res = 0;
			// constexpr int MAX_TRIALS = 500;
			repd(i) {
				int day = sortedByUsageRatios[i];
				if (ignore[day] || failed[day])
					continue;
				auto [trials, kicks] = getMaxTrialsAndKicks(epsOfDay[day], found ? 1 : 50);
				double randomness = trials == 1 ? 0 : 1;
				int cost = INF;
				rep0(iter, trials) {
					rep0(div, divs) {
						idxs[div].clear();
					}
					randomIdx = rng.next_int(RANDOM_TABLE_SIZE);
					cost =
					    roughDivCostOnDay(days[day], widths, idxs, randomness, randomTable, randomIdx, kicks);
					if (cost < INF)
						break;
				}
				if (cost == INF) {
					if (++numFails > MAX_FAILS)
						return INF;
					failed[day] = true;
					continue;
				}
				rep0(div, divs) {
					int num = len(idxs[div]);
					plans.nums[day][div] = num;
					int* p = plans.idxs[day][div];
					for (int idx : idxs[div]) {
						*p++ = idx;
					}
				}
				res += cost * (i == 0 || i == d - 1 ? 1 : 2);
			}
			if (numFails > 0) {
				assert(!found);
				repd(day) {
					ignore[day] = failed[day];
				}
				trace("oshii ", numFails, " ", widths, " ", res);
			}
			return res;
		};
		vector<int> widths;
		widths.push_back(W);
		vector<int> tmpWidths;
		vector<int> bestWidths = widths;
		int minCost = costForWidths(widths);
		auto bestPlans = plans;

		double progress = 0;
		constexpr double T_FROM = 100;
		constexpr double T_TO = 10;
		double temp = T_FROM;
		int cost = minCost;
		int iter = 0;
		auto record = [&](vector<int>& widths, int cost, int kind) {
			if (update_min(cost, minCost)) {
				trace("best rough cost updated! ", minCost, " kind=", kind, " num=", len(widths),
				    " time=", timer(), " iter=", iter);
				bestWidths = widths;
				bestPlans = plans;
			}
		};
		double tl1 = MAX_TL1;
		while (true) {
			if (len(widths) > 1 && !found) {
				found = true;
				tl1 = min(timer() + TL1, MAX_TL1);
			}
			iter++;
			if ((iter & 0xf) == 0) {
				progress = timer() / tl1;
				if (progress >= 1) {
					trace("alert1 ", iter);
					break;
				}
				temp = exp_interp(T_FROM, T_TO, progress);
			}
			double tol = -log2(rng.next_float()) * temp;
			tmpWidths = widths;
			if (len(widths) == 1) { // divide randomly
				set<int> ws;
				int divMin = min(n - 2, 6);
				int divMax = min(n - 1, 9);
				int num = rng.next_int(divMin, divMax) - 1;
				rep0(i, num) {
					int p = (int) (W * (i + 1 + rng.next_float(-0.2, 0.2)) / (num + 1));
					ws.insert(p);
				}
				ws.insert(W);
				widths.clear();
				int prevW = 0;
				for (int w : ws) {
					widths.push_back(w - prevW);
					prevW = w;
				}
				ranges::sort(widths);
				int newCost = costForWidths(widths);
				if (newCost <= cost + tol) {
					cost = newCost;
					record(widths, cost, 0);
					continue;
				} else {
					widths = tmpWidths;
				}
			}
			if (len(widths) < n) { // split
				int divs = len(widths);
				int i = divs - 1 - (int) (pow(rng.next_float(), 10) * divs);
				int w = widths[i];
				int w1 = max(1, (int) (w * rng.next_float(0.01, 0.5)));
				int w2 = w - w1;
				widths[i] = w1;
				widths.push_back(w2);
				ranges::sort(widths);
				int newCost = costForWidths(widths);
				if (newCost <= cost + tol) {
					cost = newCost;
					record(widths, cost, 1);
					continue;
				} else {
					widths = tmpWidths;
				}
			}
			if (len(widths) >= 2) { // mix
				int divs = len(widths);
				int i = rng.next_int(divs);
				int j = rng.next_int(divs - 1);
				if (j >= i)
					j++;
				int sum = widths[i] + widths[j];
				int w1 = max(1, (int) (sum * rng.next_float(0.01, 0.5)));
				int w2 = sum - w1;
				widths[i] = w1;
				widths[j] = w2;
				ranges::sort(widths);
				int newCost = costForWidths(widths);
				if (newCost <= cost + tol) {
					cost = newCost;
					record(widths, cost, 2);
					continue;
				} else {
					widths = tmpWidths;
				}
			}
		}

		// restore best
		widths = bestWidths;
		plans = bestPlans;

		trace(len(widths), " ", widths, " cost=", minCost, " cost/day=", minCost / d);

		// init state
		st.days.resize(d);
		int divs = len(widths);
		bool failed = divs == 1;
		if (failed) {
			repd(day) {
				ignore[day] = true;
			}
		}
		repd(day) {
			if (ignore[day]) {
				st.days[day].ss.clear();
				st.ignore[day] = true;
				continue;
			}
			repn(idx) {
				days[day].es[idx].initHeights(widths); // compute heights for the final widths
			}
			st.days[day].ss.resize(divs);
			rep0(div, divs) {
				int num = plans.nums[day][div];
				auto& idxs = plans.idxs[day][div];
				auto& s = st.days[day].ss[div];
				s.width = widths[div];
				s.room = W;
				// place blocks
				s.bs.clear();
				int heightSum = 0;
				rep0(i, num) {
					int idx = idxs[i];
					int height = days[day].es[idx].heights[div];
					s.bs.emplace_back(idx, height);
					heightSum += height;
				}
				assert(failed || heightSum <= W);
			}
		}
	}

	bool isPossiblyFeasible(Day& day, const vector<int>& widths) {
		int divs = len(widths);
		if (divs == 1)
			return true;
		int rooms[MAX_N];
		rep0(div, divs) {
			rooms[div] = W;
		}
		constexpr int MAX_COMBS = 64;
		static fast_iset<MAX_N> possibleDivsOf[MAX_N];
		int combs = 1;
		int tryNums = 0;
		rep0r(idx, n) {
			int area = day.es[idx].area;
			auto& possibleDivs = possibleDivsOf[idx];
			possibleDivs.clear();
			rep0(div, divs) {
				int height = getHeight(area, widths[div]);
				if (rooms[div] >= height) {
					possibleDivs.insert(div);
				}
			}
			int num = len(possibleDivs);
			if (num == 0)
				return false;
			// trace("possible places of ", idx, " are ", possibleDivs);
			if (num == 1) {
				int div = possibleDivs[0];
				rooms[div] -= getHeight(area, widths[div]);
			}
			if (combs * num <= MAX_COMBS) {
				tryNums++;
				combs *= num;
			} else {
				break;
			}
		}
		if (tryNums == 0)
			return true;
		rep0(div, divs) {
			rooms[div] = W;
		}
		auto placeDfs = [&](auto self, int i) -> bool {
			int idx = n - 1 - i;
			int area = day.es[idx].area;
			for (int div : possibleDivsOf[idx]) {
				int height = getHeight(area, widths[div]);
				if (rooms[div] >= height) {
					rooms[div] -= height;
					bool ok = i + 1 == tryNums || self(self, i + 1);
					rooms[div] += height;
					if (ok)
						return true;
				}
			}
			return false;
		};
		return placeDfs(placeDfs, 0);
	}

	int roughDivCostOnDay(Day& day, const vector<int>& widths, fast_iset<MAX_N> (&idxs)[MAX_N],
	    double randomness, double randomTable[RANDOM_TABLE_SIZE], int& randomIdx, int maxTrials) {
		int divs = len(widths);
		if (divs == 1) {
			repn(idx) {
				idxs[0].insert(idx);
			}
			return W * (n - 1);
		}
		int rooms[MAX_N];
		rep0(div, divs) {
			rooms[div] = W;
			assert(idxs[div].empty());
		}
		auto heightOf = [&](int idx, int div) {
			return getHeight(day.es[idx].area, widths[div]);
		};

		int idxToDiv[MAX_N];
		clear_with(idxToDiv, -1);

		static fast_iset<MAX_N> ngs;
		ngs.clear();
		auto place = [&](int idx, int div) {
			assert(idxToDiv[idx] == -1);
			idxToDiv[idx] = div;
			idxs[div].insert(idx);
			rooms[div] -= heightOf(idx, div);
			if (rooms[div] < 0)
				ngs.insert(div);
		};
		auto unplace = [&](int idx) {
			int div = idxToDiv[idx];
			assert(div != -1);
			idxToDiv[idx] = -1;
			idxs[div].erase(idx);
			rooms[div] += heightOf(idx, div);
			if (rooms[div] >= 0)
				ngs.erase(div);
		};
		auto divOf = [&](int idx) {
			return idxToDiv[idx];
		};
		auto computePenalty = [&]() {
			int res = 0;
			rep0(div, divs) {
				res += -min(0, rooms[div]);
			}
			return res;
		};
		// initialize greedily
		repn(i) {
			int idx = n - 1 - i;
			int bestDiv = -1;
			double maxScore = -INF;
			rep0(div, divs) {
				if (ngs.contains(div))
					continue;
				// this part is run incredibly many times so use precomputed random values
				int leftArea = rooms[div] * widths[div];
				double score = leftArea *
				    (1 + (randomTable[++randomIdx & (RANDOM_TABLE_SIZE - 1)] - 0.5) * randomness * 0.4);
				// double score = leftArea;
				if (update_max(score, maxScore)) {
					bestDiv = div;
				}
			}
			if (bestDiv == -1)
				return INF;
			int div = bestDiv;
			place(idx, div);
		}
		// try to fix
		if (!ngs.empty()) {
			assert(divs >= 2);
			static vector<int> oks;
			oks.clear();
			rep0(div, divs) {
				if (rooms[div] > 0)
					oks.push_back(div);
			}
			if (oks.empty())
				return INF;
			int count = 0;
			while (true) {
				// minimize the penalty (= sum of negative rooms)
				int idx1 = -1;
				int idx2 = -1;
				int moveToDiv = -1;
				int maxPenaltyDecrease = 0;
				for (int div1 : ngs) {
					for (int i1 : idxs[div1]) {
						int h1d1 = heightOf(i1, div1);
						rep0(div2, divs) {
							if (div2 == div1)
								continue;
							int h1d2 = heightOf(i1, div2);
							int penalty2 = max(0, -(rooms[div2] - h1d2));
							int penaltyDec1 = min(-rooms[div1], h1d1);
							int penaltyDec = penaltyDec1 - penalty2;
							if (update_max(penaltyDec, maxPenaltyDecrease)) {
								idx1 = i1;
								idx2 = -1;
								moveToDiv = div2;
							}
						}
						rep0(i2, i1) {
							int div2 = divOf(i2);
							int room2 = rooms[div2];
							int h1d2 = heightOf(i1, div2);
							int h2d1 = heightOf(i2, div1);
							int h2d2 = heightOf(i2, div2);
							if (room2 <= 0)
								continue;
							int div1Diff = h2d1 - h1d1;
							int div2Diff = h1d2 - h2d2;
							assert(div1Diff <= 0);
							assert(div2Diff >= 0);

							int penalty2 = max(0, -(rooms[div2] - div2Diff));
							int penaltyDec1 = min(-rooms[div1], -div1Diff);
							int penaltyDec = penaltyDec1 - penalty2;
							if (update_max(penaltyDec, maxPenaltyDecrease)) {
								idx1 = i1;
								idx2 = i2;
							}
						}
					}
				}
				if (maxPenaltyDecrease > 0) {
					// int ppen = computePenalty();
					if (idx2 == -1) {
						int div1 = divOf(idx1);
						int div2 = moveToDiv;
						unplace(idx1);
						place(idx1, div2);
					} else {
						int div1 = divOf(idx1);
						int div2 = divOf(idx2);
						unplace(idx1);
						unplace(idx2);
						place(idx2, div1);
						place(idx1, div2);
					}
					// int pen = computePenalty();
					// assert(pen < ppen);
					// assert(ngs.empty() == (pen == 0));
					if (ngs.empty())
						break;
				} else { // stuck in local optimal or simply infeasible
					if (++count >= maxTrials)
						return INF; // prolly infeasible
					// kick some and try again
					rep0(iter, 1) {
						int div1, div2;
						int idx1, idx2;
						do {
							idx1 = rng.next_int(n);
							idx2 = rng.next_int(n - 1);
							if (idx2 >= idx1)
								idx2++;
							div1 = divOf(idx1);
							div2 = divOf(idx2);
						} while (div1 == div2);
						unplace(idx1);
						unplace(idx2);
						place(idx1, div2);
						place(idx2, div1);
					}
					ngs.clear();
					rep0(div, divs) {
						if (rooms[div] < 0)
							ngs.insert(div);
					}
					if (ngs.empty())
						break; // not likely to happen but anyways
				}
			}
		}
		while (true) {
			rep0(div, divs) {
				if (idxs[div].empty()) { // place at least one
					int idx = div;
					unplace(idx);
					place(idx, div);
				}
			}
			bool ng = false;
			rep0(div, divs) {
				ng |= idxs[div].empty();
			}
			if (!ng)
				break;
		}
		// pack to smaller strips
		rep0(div, divs) {
			rep(div2, div + 1, divs) {
				if (len(idxs[div2]) == 1)
					continue;
				int bestIdx = -1;
				int maxHeight = 0;
				for (int idx : idxs[div2]) {
					int height = heightOf(idx, div);
					if (rooms[div] >= height) {
						if (update_max(height, maxHeight)) {
							bestIdx = idx;
						}
					}
				}
				if (bestIdx != -1) {
					unplace(bestIdx);
					place(bestIdx, div);
				}
			}
		}
		// sanity check
		rep0(div, divs) {
			int sum = 0;
			for (int idx : idxs[div]) {
				sum += heightOf(idx, div);
			}
			// if (rooms[div] != W - sum || rooms[div] < 0) {
			// 	trace(div, " ", widths, " ", rooms[div], " ", W - sum);
			// }
			assert(rooms[div] >= 0);
			assert(rooms[div] == W - sum);
		}
		int cost = 0;
		rep0(div, divs) {
			cost += widths[div] * (len(idxs[div]) - 1);
		}
		return cost;
	}

	void buildSolution() {
		repd(day) {
			auto& ss = st.days[day].ss;
			int divs = len(ss);
			int widthSum = 0;
			vector<int> sorted;
			rep0(div, divs) {
				sorted.push_back(div);
			}
			ranges::sort(sorted, [&](int a, int b) {
				return ss[a].width < ss[b].width;
			});
			auto& rects = sol.rects[day];
			assert(len(rects) == n);
			rep0(order, divs) {
				int div = order % 2 == 0 ? sorted[order >> 1] : sorted[divs - 1 - (order >> 1)];
				auto& s = ss[div];
				int width = s.width;
				auto& bs = s.bs;
				auto wposs = st.wallPoss(day, div);

				ranges::sort(bs, [](auto& a, auto& b) {
					return a.pos < b.pos;
				});

				int i = 0;
				for (auto& b : bs) {
					int st = i == 0 ? 0 : wposs[i - 1];
					int en = i == len(bs) - 1 ? W : wposs[i];
					assert(en - st >= b.height);
					int idx = b.idx;
					rects[idx] = {st, widthSum, en, widthSum + width};
					i++;
				}
				widthSum += width;
			}
		}
	}

	bool solveTheoretical() {
		vector<int> sizes;
		repn(idx) {
			sizes.push_back(maxNth[idx]);
		}
		vector<int> sorted;
		repn(i) {
			sorted.push_back(i);
		}
		int heightLeft = W;
		int widthLeft = W;
		int widthSum = 0;
		bool ng = false;
		auto& rects = sol.rects[0];
		while (!sorted.empty()) {
			if (widthLeft <= 0 || heightLeft <= 0) {
				ng = true;
				break;
			}
			if (len(sorted) == 1) {
				int idx = sorted[0];
				int a = sizes[idx];
				if (widthLeft * heightLeft < a) {
					ng = true;
					break;
				}
				rects[idx] = {0, widthSum, heightLeft, widthSum + widthLeft};
				break;
			}
			// determine next
			int loss[MAX_N][2] = {};
			for (int idx : sorted) {
				int a = sizes[idx];
				int w = (a + heightLeft - 1) / heightLeft;
				int h = (a + widthLeft - 1) / widthLeft;
				loss[idx][0] = w * heightLeft - a;
				loss[idx][1] = h * widthLeft - a;
			}
			ranges::sort(sorted, [&](int a, int b) {
				return min(loss[a][0], loss[a][1]) > min(loss[b][0], loss[b][1]);
			});
			int idx = sorted.back();
			sorted.pop_back();

			// put next
			int a = sizes[idx];
			int w = (a + heightLeft - 1) / heightLeft;
			int h = (a + widthLeft - 1) / widthLeft;
			if (loss[idx][0] < loss[idx][1]) {
				rects[idx] = {0, widthSum + widthLeft - w, heightLeft, widthSum + widthLeft};
				widthLeft -= w;
			} else {
				rects[idx] = {heightLeft - h, widthSum, heightLeft, widthSum + widthLeft};
				heightLeft -= h;
			}
		}
		if (ng)
			return false;
		rep(day, 1, d) {
			sol.rects[day] = sol.rects[0];
		}
		return true;
	}

	void solveMain(ostream& out) { // write answer to out
		sol.init(d, n);

		int maxNthSum = 0;
		vector<int> accumSum;
		repn(idx) {
			maxNthSum += maxNth[idx];
			accumSum.push_back(maxNthSum);
		}
		trace(accumSum);
		trace("max nth sum: ", maxNthSum, "/", W * W);
		if (maxNthSum <= W * W) {
			if (solveTheoretical()) {
				writeSolution(out);
				return;
			}
			// assert(("failed to generate theoretical solution", false));
			trace("failed to generate theoretical solution");
		}

		setupStrips();
		int day0 = st.aDay();
		if (day0 != -1) {
			sa();
		}
		vector<int> leftDays;
		repd(day) {
			if (st.ignore[day])
				leftDays.push_back(day);
		}
		int numLeftDays = len(leftDays);
		if (numLeftDays > 0) {
			int start = timer();
			int end = TL3;
			rep0(i, numLeftDays) {
				int day = leftDays[i];
				auto& ss = st.days[day].ss;
				auto& rects = sol.rects[day];
				auto res = setupStripsOfDay(day, (int) lerp(start, end, (i + 1) / (double) numLeftDays));
				if (!res.empty()) {
					trace("split successful for day ", day);
					int widthSum = 0;
					for (auto& [width, idxs] : res) {
						int heightSum = 0;
						int last = idxs[idxs.size() - 1];
						for (int idx : idxs) {
							int height = getHeight(days[day].es[idx].area, width);
							rects[idx] = {
							    heightSum, widthSum, idx == last ? W : heightSum + height, widthSum + width};
							heightSum += height;
						}
						widthSum += width;
					}
				} else {
					trace("failed to split for day ", day, " exterme mode");
					assert(len(rects) == n);
					vector<int> sorted;
					repn(idx) {
						sorted.push_back(idx);
					}
					int widthLeft = W;
					int heightLeft = W;
					bool ng = false;
					while (!sorted.empty()) {
						if (widthLeft <= 0 || heightLeft <= 0) {
							ng = true;
							break;
						}
						if (len(sorted) == 1) {
							int idx = sorted[0];
							int a = p.as[day][idx];
							if (widthLeft * heightLeft < a) {
								ng = true;
								break;
							}
							rects[idx] = {0, 0, heightLeft, 0 + widthLeft};
							break;
						}
						// determine next
						int loss[MAX_N][2] = {};
						for (int idx : sorted) {
							int a = p.as[day][idx];
							int w = (a + heightLeft - 1) / heightLeft;
							int h = (a + widthLeft - 1) / widthLeft;
							loss[idx][0] = w * heightLeft - a;
							loss[idx][1] = h * widthLeft - a;
						}
						ranges::sort(sorted, [&](int a, int b) {
							return min(loss[a][0], loss[a][1]) > min(loss[b][0], loss[b][1]);
						});
						int idx = sorted.back();
						sorted.pop_back();

						// put next
						int a = p.as[day][idx];
						int w = (a + heightLeft - 1) / heightLeft;
						int h = (a + widthLeft - 1) / widthLeft;
						if (loss[idx][0] < loss[idx][1]) {
							rects[idx] = {0, 0 + widthLeft - w, heightLeft, 0 + widthLeft};
							widthLeft -= w;
						} else {
							rects[idx] = {heightLeft - h, 0, heightLeft, 0 + widthLeft};
							heightLeft -= h;
						}
					}

					if (ng) { // exterme mode 2, allow to violate reservation request
						trace("failed again, exterme mode 2");
						int widthLeft = W;
						int heightLeft = W;
						sorted.clear();
						repn(idx) {
							sorted.push_back(idx);
						}
						repn(iter) {
							if (iter == n - 1) {
								assert(len(sorted) == 1);
								int idx = sorted[0];
								rects[idx] = {0, 0, heightLeft, 0 + widthLeft};
								continue;
							}
							// determine next
							int loss[MAX_N][2] = {};
							for (int idx : sorted) {
								int a = p.as[day][idx];
								int w = (a + heightLeft - 1) / heightLeft;
								int h = (a + widthLeft - 1) / widthLeft;
								loss[idx][0] = w * heightLeft - a;
								loss[idx][1] = h * widthLeft - a;
							}
							ranges::sort(sorted, [&](int a, int b) {
								return min(loss[a][0], loss[a][1]) > min(loss[b][0], loss[b][1]);
							});
							int idx = sorted.back();
							sorted.pop_back();

							// put next
							int a = p.as[day][idx];
							int w = (a + heightLeft / 2) / heightLeft;
							int h = (a + widthLeft / 2) / widthLeft;
							if (loss[idx][0] < loss[idx][1]) {
								rects[idx] = {0, 0 + widthLeft - w, heightLeft, 0 + widthLeft};
								widthLeft -= w;
							} else {
								rects[idx] = {heightLeft - h, 0, heightLeft, 0 + widthLeft};
								heightLeft -= h;
							}
						}
					}
				}
			}
		}
		writeSolution(out);
	}

	void writeSolution(ostream& out) {
		trace("writing...");
		repd(day) {
			repn(idx) {
				auto& r = sol.rects[day][idx];
				out << r[0] << " " << r[1] << " " << r[2] << " " << r[3] << endl;
			}
		}
#ifndef ONLINE_JUDGE
		int st = timer();
		trace("computing score...");
		int score = computeScore(sol);
		trace("DONE ", timer() - st);
		result.score = score;
#endif
	}
};

// #define MASTERS
#ifdef MASTERS

// for masters stuff
void makeMovie(int argc, char* argv[]) {
	if (argc != 4) {
		trace("usage: [input file] [output file] [movie output]");
		return;
	}
	ifstream in(argv[1]);
	ifstream out(argv[2]);
	if (!in) {
		trace("couldn't open input file");
		return;
	}
	if (!out) {
		trace("couldn't open output file");
		return;
	}
	movie mov;
	mov.set_file(string(argv[3]));
}
#endif

constexpr int SCORES[100] = {282, 44076, 33601, 5698, 6947, 7239, 623, 1882, 15727, 967, 70207, 12201, 5495,
    31551, 12852, 10810, 81079, 5946, 2230, 45410, 31415, 4084, 4406, 8978, 29309, 3626, 1536, 14013, 1726,
    2416, 15339, 55629, 139, 103155, 1437278, 2540, 7709, 12380, 6537, 128125, 40539, 1286, 9004, 13455,
    11108, 32428, 17126, 174813, 2458, 17882, 18109, 48947, 6253, 16033, 27835, 21967, 1221, 9188, 16218,
    7795, 402159, 217455, 194026, 3071, 29919, 4516, 287749, 3273, 3228, 68849, 3078, 5528, 15043, 32689,
    57880, 138926, 76385, 29068, 14197, 40934, 12895, 7740, 21152, 4006, 93755, 21263, 23532, 5741, 1654,
    17953, 1017567, 1174, 11516, 5515, 47917, 47723, 2653, 503, 35482, 25344};

int main(int argc, char* argv[]) {
#ifdef MASTERS
	if (argc > 1) {
		makeMovie(argc, argv);
		return 0;
	}
#endif

#if 0 || ONLINE_JUDGE
	isLocal = false;
	render = false;
	timer(true);
	Solver sol;
	sol.load(cin);
	sol.solve();
#elif 0
	makeMovie(argc, argv);
#elif 0
	// write metadata
	ostringstream oss;
	oss << "seed D N eps" << endl;
	rep0(seed, 5000) {
		Solver sol;
		sol.load(seed);
		int d = sol.d;
		int n = sol.n;
		int areaSum = 0;
		int maxArea = 0;
		repd(i) {
			repn(j) {
				areaSum += sol.p.as[i][j];
			}
		}
		constexpr int MAX_DIV = MAX_N;
		constexpr int INF = 5000000;
		int eps100 = (int) round(sqrt(1 - areaSum / (1e6 * d)) * 100);
		oss << seed << " " << d << " " << n << " " << eps100 / 100.0 << endl;
	}
	write_text("scores/input.txt", oss.str());
#elif 1
	// for local/remote testers
	debug = true;
	render = true;
	int seed;
	cin >> seed;
	cin >> time_scale;

	Params params;
	cin >> params.flp[0] >> params.flp[1];
	cin >> params.sfl[0] >> params.sfl[1];
	cin >> params.mix[0] >> params.mix[1];
	cin >> params.mov[0] >> params.mov[1];
	cin >> params.swp[0] >> params.swp[1];

	timer(true);
	Solver sol;
	sol.params = params;
	sol.load(seed);
	sol.solve();
	// double relScore = min(1e9, SCORES[seed] / (double) sol.result.score * 1e9);
	// cout << (int) (relScore + 0.5) << " " << timer() << endl;
	cout << sol.result.score << " " << timer() << endl;
#elif 1
	// single-threaded test, handy but slow
	time_scale = 1.0;
	int num = 10;
	int from = 0;
	int stride = 1;
	int single = -1;
	debug = true;
	render = true;

	struct TestCase {
		int seed;
		int time;
		ll score;
	};
	vector<TestCase> cases;

	vector<int> seedList = {};
	// vector<int> seedList = {306, 378, 1049, 1353, 1359, 2782, 2879};
	// vector<int> seedList = {34, 39, 60, 62, 66, 90, 100, 103, 111, 116};
	// vector<int> seedList = {146, 160, 165, 171, 211, 213, 222, 233, 243, 308};
	// vector<int> seedList = {312, 320, 344, 354, 363, 374, 377, 432, 458, 466};
	if (seedList.empty() || single != -1) {
		seedList.clear();
		if (single == -1) {
			rep0(t, num) {
				seedList.push_back(from + t * stride);
			}
		} else {
			seedList.push_back(single);
		}
	}

	bool doTrace = debug;
	debug = true;
	for (int seed : seedList) {
		timer(true);
		trace("------------ SOLVING SEED ", seed, " ------------");
		debug = doTrace;
		Solver s;
		s.load(seed);
		s.solve();
		debug = true;
		int time = timer();
		trace("score: ", s.result.score, " (time ", time, " ms)\n");
		if (s.result.score != -1)
			cases.emplace_back(seed, time, s.result.score);
	}

	auto print = [&](const TestCase& c) {
		int seed = c.seed;
		string space = seed < 10 ? "   " : seed < 100 ? "  " : seed < 1000 ? " " : "";
		trace("  seed ", space, seed, ": ", c.score, " (time ", c.time, " ms)");
	};

	if (len(cases) > 1) {
		trace("------------ summary ------------");

		trace("sort by seed:");
		sort(cases.begin(), cases.end(), [&](auto a, auto b) {
			return a.seed < b.seed;
		});
		for (auto& c : cases)
			print(c);

		trace("sort by score:");
		sort(cases.begin(), cases.end(), [&](auto a, auto b) {
			return a.score > b.score;
		});
		for (auto& c : cases)
			print(c);

		ll scoreSum = 0;
		double logScoreSum = 0;
		for (auto& c : cases) {
			scoreSum += c.score;
			logScoreSum += log(c.score);
		}
		double invDenom = 1.0 / len(cases);
		trace("total score: ", scoreSum, ", mean: ", (ll) (scoreSum * invDenom * 100 + 0.5) / 100.0,
		    ", mean(log2): ", (ll) (logScoreSum * invDenom * 1000 + 0.5) / 1000.0);
	}
#endif
}
