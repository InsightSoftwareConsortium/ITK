#include <vcl_compiler.h>

// ------------------------------------------------

// this *does* work for SGI CC 7.2.1 -- fsm

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
X<T> *X<T>::pl = 0;

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

#include <vcl_iostream.h>

void vcl_test_implicit_instantiation(int n);


int test_compiler_main(int /*argc*/,char* /*argv*/[])
{
  int result = 0;

  vcl_cout << "Testing static template member..." << vcl_flush;
  if ( X<int>::pl == 0 ) {
    vcl_cout << "  PASSED" << vcl_endl;
  } else {
    vcl_cout << "**FAILED**" << vcl_endl;
    result = 1;
  }

  // If it links, it passed!
  vcl_cout << "Testing implicit instantation..." << vcl_flush;
  vcl_test_implicit_instantiation(100);
  vcl_cout << "  PASSED" << vcl_endl;

  return result;
}

#if defined(VCL_USE_IMPLICIT_TEMPLATES) && VCL_USE_IMPLICIT_TEMPLATES
#include <vcl_vector.h>
#include <vcl_map.h>
#include <vcl_algorithm.h>

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
  vcl_vector<mystery_type> v;
  v.resize(n);
  for (int i=0; i<n; ++i) {
    v[i].a = i;
    v[i].b = i/float(n);
  }
  v.reserve(2*n);
  v.resize(n/2);
  vcl_sort(v.begin(), v.end());
  v = v;
  v.clear();

  typedef vcl_map<int, mystery_type, vcl_less<int> > map_t;
  map_t m;
  for (int i=0; i<n; ++i)
    m.insert(map_t::value_type(0, mystery_type(i, i/float(n))));
  m.clear();
}

mystery_type::mystery_type()
{ }
mystery_type::mystery_type(int a_, float b_)
  : a(a_), b(b_) { }
mystery_type::mystery_type(mystery_type const &that)
  : a(that.a), b(that.b) { }
mystery_type &mystery_type::operator=(mystery_type const &that)
{ a = that.a; b = that.b; return *this; }

bool operator==(mystery_type const &x, mystery_type const &y)
{ return (x.a == y.a) && (x.b == y.b); }
bool operator< (mystery_type const &x, mystery_type const &y)
{ return (x.a <  y.b) || ((x.a == y.a) && (x.b < y.b)); }

#else
void vcl_test_implicit_instantiation(int) { }
#endif
