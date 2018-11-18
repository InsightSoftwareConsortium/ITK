#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

// ------------------------------------------------

template <class T>
struct X
{
  int x;
  X();
  // declaration of static template member.
  static X<T> *pl;
};

template <class T>
X<T>::X() : x(1728) { }

// definition (not specialization) of static template member.
template <class T>
X<T> *X<T>::pl = nullptr;

// explicit instantiation of class also instantiates statics.
template struct X<int>;

// ------------------------------------------------

struct A
{
  int x;
  mutable int y;
  A() : x(0), y(0) { }
  void f() { ++ x; }
  void g() const { ++ y; }
};

// ------------------------------------------------


void vcl_test_implicit_instantiation(int n);


int test_compiler_main(int /*argc*/,char* /*argv*/[])
{
  int result = 0;

  std::cout << "Testing static template member..." << std::flush;
  if ( X<int>::pl == nullptr ) {
    std::cout << "  PASSED" << std::endl;
  }
  else {
    std::cout << "**FAILED**" << std::endl;
    result = 1;
  }

  // If it links, it passed!
  std::cout << "Testing implicit instantiation..." << std::flush;
  vcl_test_implicit_instantiation(100);
  std::cout << "  PASSED" << std::endl;

  return result;
}

struct mystery_type
{
  mystery_type();
  mystery_type(int, float);
  mystery_type(mystery_type const &);
  mystery_type &operator=(mystery_type const &);
  int a;
  float b;
};

bool operator==(mystery_type const &, mystery_type const &);
bool operator< (mystery_type const &, mystery_type const &);

void vcl_test_implicit_instantiation(int n)
{
  std::vector<mystery_type> v;
  v.resize(n);
  for (int i=0; i<n; ++i) {
    v[i].a = i;
    v[i].b = i/float(n);
  }
  v.reserve(2*n);
  v.resize(n/2);
  std::sort(v.begin(), v.end());
  v = v;
  v.clear();

  typedef std::map<int, mystery_type, std::less<int> > map_t;
  map_t m;
  for (int i=0; i<n; ++i)
    m.insert(map_t::value_type(0, mystery_type(i, i/float(n))));
  m.clear();
}

mystery_type::mystery_type() = default;

mystery_type::mystery_type(int a_, float b_)
  : a(a_), b(b_) { }

mystery_type::mystery_type(mystery_type const &that) = default;

mystery_type &mystery_type::operator=(mystery_type const &that) = default;

bool operator==(mystery_type const &x, mystery_type const &y)
{ return (x.a == y.a) && (x.b == y.b); }

bool operator< (mystery_type const &x, mystery_type const &y)
{ return (x.a <  y.b) || ((x.a == y.a) && (x.b < y.b)); }
