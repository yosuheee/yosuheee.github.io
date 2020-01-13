#include <bits/stdc++.h>

#define rep(i, n) for (int i = 0; i < int(n); i++)
#define rrep(i, n) for (int i = int(n) - 1; i >= 0; i--)
#define reps(i, n) for (int i = 1; i <= int(n); i++)
#define rreps(i, n) for (int i = int(n); i >= 1; i--)
#define repc(i, n) for (int i = 0; i <= int(n); i++)
#define rrepc(i, n) for (int i = int(n); i >= 0; i--)
#define repi(i, a, b) for (int i = int(a); i < int(b); i++)
#define repic(i, a, b) for (int i = int(a); i <= int(b); i++)
#define each(x, y) for (auto &x : y)
#define all(a) (a).begin(), (a).end()
#define bit(b) (1ll << (b))

using namespace std;

using i32 = int;
using i64 = long long;
using u64 = unsigned long long;
using f80 = long double;
using vi32 = vector<i32>;
using vi64 = vector<i64>;
using vu64 = vector<u64>;
using vf80 = vector<f80>;
using vstr = vector<string>;

inline void yes() { cout << "Yes" << '\n'; exit(0); }
inline void no() { cout << "No" << '\n'; exit(0); }
inline i64 gcd(i64 a, i64 b) { if (min(a, b) == 0) return max(a, b); if (a % b == 0) return b; return gcd(b, a % b); }
inline i64 lcm(i64 a, i64 b) { return a / gcd(a, b) * b; }
inline u64 xorshift() { static u64 x = 88172645463325252ull; x = x ^ (x << 7); return x = x ^ (x >> 9); }
void solve(); int main() { ios::sync_with_stdio(0); cin.tie(0); cout << fixed << setprecision(16); solve(); return 0; }
template <typename T> class pqasc : public priority_queue<T, vector<T>, greater<T>> {};
template <typename T> class pqdesc : public priority_queue<T, vector<T>, less<T>> {};
template <typename T> inline void amax(T &x, T y) { if (x < y) x = y; }
template <typename T> inline void amin(T &x, T y) { if (x > y) x = y; }
template <typename T> inline T power(T x, i64 n, T e = 1) { T r = e; while (n > 0) { if (n & 1) r *= x; x *= x; n >>= 1; } return r; }
template <typename T> istream& operator>>(istream &is, vector<T> &v) { each(x, v) is >> x; return is; }
template <typename T> ostream& operator<<(ostream &os, vector<T> &v) { rep(i, v.size()) { if (i) os << ' '; os << v[i]; } return os; }
template <typename T, typename S> istream& operator>>(istream &is, pair<T, S> &p) { is >> p.first >> p.second; return is; }
template <typename T, typename S> ostream& operator<<(ostream &os, pair<T, S> &p) { os << p.first << ' ' << p.second; return os; }

void solve() {
  {
    vi32 arr = {1, 1, 2, 3, 3, 4, 5};
    function<int(int)> func = [&](int x) {
      return lower_bound(all(arr), x) - arr.begin();
    };
    assert(func(2) == 2);
    assert(func(3) == 3);
    assert(func(5) == 6);
    assert(func(100) == 7);
    assert(func(0) == 0);
    assert(func(1) == 0);
  }
  {
    struct element {
      int a, b;
      bool operator<(const element &that) const {
        if (a != that.a) return a < that.a;
        if (b != that.b) return b < that.b;
        return false;
      }
    };
    vector<element> v;
    v.push_back({ 3, 2 });
    v.push_back({ 3, 3 });
    v.push_back({ 5, 3 });
    v.push_back({ 1, 1 });
    v.push_back({ 3, 1 });
    sort(all(v));
    assert(v[0].a == 1 && v[0].b == 1);
    assert(v[1].a == 3 && v[1].b == 1);
    assert(v[2].a == 3 && v[2].b == 2);
    assert(v[3].a == 3 && v[3].b == 3);
    assert(v[4].a == 5 && v[4].b == 3);
  }
  {
    // If you want the type of a priority queue element to be a structure,
    // '>' operator must be defined.
    // It is not enough to just define '<' operator!
    struct element {
      int a, b;
      bool operator<(const element &that) const {
        if (a != that.a) return a < that.a;
        if (b != that.b) return b < that.b;
        return false;
      }
      bool operator>(const element &that) const {
        return that < *this;
      }
    };
    pqasc<element> pq;
    pq.push({ 3, 2 });
    pq.push({ 3, 3 });
    pq.push({ 5, 3 });
    pq.push({ 1, 1 });
    pq.push({ 3, 1 });
    assert(pq.top().a == 1 && pq.top().b == 1); pq.pop();
    assert(pq.top().a == 3 && pq.top().b == 1); pq.pop();
    assert(pq.top().a == 3 && pq.top().b == 2); pq.pop();
    assert(pq.top().a == 3 && pq.top().b == 3); pq.pop();
    assert(pq.top().a == 5 && pq.top().b == 3); pq.pop();
  }
  {
    struct element {
      int a, b;
      bool operator<(const element &that) const {
        if (a != that.a) return a < that.a;
        if (b != that.b) return b > that.b;
        return false;
      }
      bool operator>(const element &that) const {
        return that < *this;
      }
    };
    pqasc<element> pq;
    pq.push({ 3, 2 });
    pq.push({ 3, 3 });
    pq.push({ 5, 3 });
    pq.push({ 1, 1 });
    pq.push({ 3, 1 });
    assert(pq.top().a == 1 && pq.top().b == 1); pq.pop();
    assert(pq.top().a == 3 && pq.top().b == 3); pq.pop();
    assert(pq.top().a == 3 && pq.top().b == 2); pq.pop();
    assert(pq.top().a == 3 && pq.top().b == 1); pq.pop();
    assert(pq.top().a == 5 && pq.top().b == 3); pq.pop();
  }
  {
    // tempura's sort
    vector<string> names = {"a", "b", "c"};
    vector<int> ages = {3, 2, 4};

    vector<int> ord(3);
    iota(all(ord), 0);
    sort(all(ord), [&](int i, int j) {
      // When first element less than second element, return true.
      return ages[i] < ages[j];
    });

    assert(ord[0] == 1);
    assert(ord[1] == 0);
    assert(ord[2] == 2);
  }
  {
    // string is comparable
    assert("abc" < "def");
    assert("abc" < "abd");
    assert("abd" > "abc");
    assert("abc" == "abc");
  }
  {
    // immediately invoked function expression
    int a = 1, b = 2;
    bool ok = [&]{
      return a < b;
    }();
    assert(ok);

    bool ok2 = [&](){
      return a < b;
    }();
    assert(ok2);

    bool ng = [](int x, int y){
      return x > y;
    }(a, b);
    assert(!ng);
  }
}
