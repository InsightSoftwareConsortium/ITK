//-------------------------------------

#ifdef VCL_HAS_BOOL

void function(int i, void *ptr, bool v) {}

int main() { return 0; }
#endif // VCL_HAS_BOOL

//-------------------------------------

#ifdef VCL_HAS_TYPENAME

template <typename T>
class bingo { public: void bongo(T **); };

int main() { return 0; }
#endif // VCL_HAS_TYPENAME

//-------------------------------------

#ifdef VCL_HAS_EXPORT

export
template <class T, int N>
struct plither
{
  plither(){}
  ~plither(){}
  void f(T *, int){}
};

void g()
{
  double x;
  int y;
  plither<double, 3> obj;
  obj.f(&x, y);
}

int main() { return 0; }
#endif // VCL_HAS_EXPORT

//-------------------------------------

#ifdef VCL_HAS_MUTABLE

class X {
 public:
  mutable int const *p;
};

int main() { return 0; }
#endif // VCL_HAS_MUTABLE

//-------------------------------------

#ifdef VCL_HAS_EXPLICIT

class X { public: explicit X(int) {} };

int main() { return 0; }
#endif // VCL_HAS_EXPLICIT

//-------------------------------------

#ifdef VCL_HAS_DYNAMIC_CAST

struct foo { foo(){} virtual ~foo(){} virtual void f()=0; };
struct boo : public foo { void f() { *(int*)0 = 1; } };
boo *try_dynamic_cast() { boo *b = 0; foo *f = b; return dynamic_cast<boo*>(f); }

int main() { return 0; }
#endif // VCL_HAS_DYNAMIC_CAST

//-------------------------------------

#ifdef VCL_HAS_RTTI

#include <typeinfo>
class A { public: virtual ~A() {} virtual void f() {} };
class B : public A { public: void f() {} };
bool try_rtti() { B*b=0; A*a1=b,*a2=b; return typeid(a1)==typeid(a2); }

int main() { return 0; }
#endif // VCL_HAS_RTTI

//-------------------------------------

#ifdef VCL_FOR_SCOPE_HACK
// VCL_FOR_SCOPE_HACK will be set to "1" if this fails to compile
class A { public: void f() { } };

void fn() {
  for (int i=0; i<100; ++i) {}
  for (long i=0; i<1000; ++i) {}
  for (double i = 3.141; i<100.0; i += 1.0) { }
  // VC7 only raises warnings for previous tests
  A i; i.f();
}
#endif // VCL_FOR_SCOPE_HACK

//-------------------------------------

#ifdef VCL_DEFAULT_VALUE
// VCL_DEFAULT_VALUE(x) will be set to "= x" if this test fails, to "" otherwise

// declaration
void function(int x, char *ptr = "foo");

// definition
void function(int x, char *ptr) { ++ ptr[x]; }

int main() { return 0; }
#endif // VCL_DEFAULT_VALUE

//-------------------------------------

#ifdef VCL_HAS_MEMBER_TEMPLATES

template <class S>
class blip {
 public:
  S *ptr;
  template <class T> void klor(T *p) { *ptr = *p; }
};
void function()
{
  blip<double> b;
  int s;
  b.klor(&s);
}

int main() { return 0; }
#endif // VCL_HAS_MEMBER_TEMPLATES

//-------------------------------------

#ifdef VCL_CAN_DO_PARTIAL_SPECIALIZATION

template <class T>
class victor
{
  T data[256];
 public:
  victor() {}
  T &operator[](unsigned i) { return data[i]; }
};

template <class T>
class victor<T *>
{
  T *data[256];
 public:
  T * &operator[](unsigned i) { return data[i]; }
  void slarf() { data[0] += (data[2] - data[1]); }
};

template <class A, class R>
struct foo {
  typedef A a;
  typedef R r;
};

template <class T> struct foo<T *, T *> { void bar() {} };
template <class T> struct foo<int *, T> { void baz() {} };

int main() { return 0; }
#endif // VCL_CAN_DO_PARTIAL_SPECIALIZATION

//-------------------------------------

#ifdef VCL_DEFINE_SPECIALIZATION
// VCL_DEFINE_SPECIALIZATION is set to "template <>" if this compiles, to "" otherwise

// declaration
template <class T> class traits {};

// specialization
template <>
class traits<double> {
 public:
  typedef double abs_t;
  typedef double float_t;
};

int main() { return 0; }
#endif // VCL_DEFINE_SPECIALIZATION

//-------------------------------------

#ifdef VCL_ALLOWS_INLINE_INSTANTIATION

template <class T>
inline
T dot(T const *a, T const *b)
{
  return a[0]*b[0];
}

template double dot(double const *, double const *);

int main() { return 0; }
#endif // VCL_ALLOWS_INLINE_INSTANTIATION

//-------------------------------------

#ifdef VCL_NEEDS_INLINE_INSTANTIATION
// VCL_NEEDS_INLINE_INSTANTIATION is set to 1 if this fails to compile

template <class T>
inline T dot(T const *a, T const *b) { return a[0]*b[0]+a[1]*b[1]+a[2]*b[2]; }
int function();
int call_this() { function(); return 0; }
int function()
{
  double a[3] = {1.0, 2.0, 3.0};
  double b[3] = {4.0, 5.0, 6.0};
  double a_b = dot(a, b);
  return int(a_b);
}
// If the program links, the compiler inlined the function template.

int main() { return 0; }
#endif // VCL_NEEDS_INLINE_INSTANTIATION

//-------------------------------------

#ifdef VCL_STATIC_CONST_INIT_INT

class A {
 public:
  static const int x = 27;
  static const bool y = false;
};

int main() { return A::x == 27 && !A::y ? 0 : 1; }
#endif // VCL_STATIC_CONST_INIT_INT

//-------------------------------------

#ifdef VCL_STATIC_CONST_INIT_NO_DEFN

// This should not compile.  C++ requires storage to be allocated for
// the constant to use it at runtime.  Some compilers do compile this,
// though, and if a definition is given, it becomes a multiply defined
// symbol.  If this does compile, we should not give a definition for
// such constants.
class A
{
 public:
  static const int x = 27;
};

void f(const int&) {}
int main() { f(A::x); return 0; }
#endif // VCL_STATIC_CONST_INIT_NO_DEFN

//-------------------------------------

#ifdef VCL_STATIC_CONST_INIT_FLOAT

class A {
 public:
  static const float x = 27.0f;
  static const double y = 27.0;
};

int main() { return A::x == 27.0f && A::y == 27.0 ? 0 : 1; }
#endif // VCL_STATIC_CONST_INIT_FLOAT

//-------------------------------------

#ifdef VCL_CAN_DO_STATIC_TEMPLATE_MEMBER

template <class T> struct A { A() {} static char *fmt; };
template <class T> char *A<T>::fmt = 0;

int main() { return 0; }
#endif // VCL_CAN_DO_STATIC_TEMPLATE_MEMBER

//-------------------------------------

#ifdef VCL_CAN_DO_NON_TYPE_FUNCTION_TEMPLATE_PARAMETER

template <class T, int n> struct splek { T data[n]; };

template <class T, int n>
void splok_that_splek(splek<T, n> &s)
{
  for (int i=0; i<n; ++i)
    s.data[i] = T(27);
}

template struct splek<double, 3>;
template void splok_that_splek(splek<double, 3> &);

int main() { return 0; }
#endif // VCL_CAN_DO_NON_TYPE_FUNCTION_TEMPLATE_PARAMETER

//-------------------------------------

#ifdef VCL_NEED_FRIEND_FOR_TEMPLATE_OVERLOAD
// VCL_NEED_FRIEND_FOR_TEMPLATE_OVERLOAD is set to 1 if this fails to compile

template <class T>
class victor_base {
 public:
  T &operator[](unsigned i) { return data[i]; }

 protected:
  victor_base(T *p, unsigned n) : data(p), size(n) {}

 private:
  T *data;
  unsigned size;
};

template <class T>
bool operator==(victor_base<T> const&, victor_base<T> const&) { return false; }

template <class T, int n>
class victor_fixed : public victor_base<T> {
 public:
  T data_fixed[n];

  victor_fixed() : victor_base<T>(data_fixed, n) {}
};

int function(victor_fixed<double, 3> const &a,
             victor_fixed<double, 3> const &b)
{
  if (a == b) // 2.7 fails to resolve this.
    return 3141;
  else
    return 2718;
}

int main() { return 0; }
#endif // VCL_NEED_FRIEND_FOR_TEMPLATE_OVERLOAD

//-------------------------------------

#ifdef VCL_OVERLOAD_CAST
// VCL_OVERLOAD_CAST(x) is set to "(x)" if this compiles, to "((T)(x))" otherwise

template <class T>
class vnl_vector {
 public:
  unsigned size;
  T *data;
  vnl_vector(unsigned n, T *ptr) : size(n), data(ptr) {}
};

template <class T>
bool operator==(vnl_vector<T> const&, vnl_vector<T> const&) { return false; }

//
template <unsigned n, class T>
class vnl_vector_fixed : public vnl_vector<T> {
 public:
  T data_fixedn;
  vnl_vector_fixed() : vnl_vector<T>(n, data_fixed) {}
};

//
void print_it(vnl_vector<double> const &){}

void try_it(vnl_vector_fixed<3, double> const &u,
            vnl_vector_fixed<3, double> const &v)
{
  // gcc 2.7 fails in this function.
  if (u == v)
    print_it(u);
  else {
    print_it(u);
    print_it(v);
  }
}

//
template <class S, class T>
void copy_image(S const * const *src, T * const *dst, int, int) {}

typedef unsigned char byte;

void do_vision(int w, int h, byte **image_i, float **image_f) {
  // SGI CC 7.21 fails here.
  copy_image(image_i, image_f, w, h);
}

int main() { return 0; }
#endif // VCL_OVERLOAD_CAST

//-------------------------------------

#ifdef VCL_NULL_TMPL_ARGS
// VCL_NULL_TMPL_ARGS is set to "<>" if this fails to compile, to "" otherwise

template <class T> class victor;
template <class T> T dot(victor<T> const &u, victor<T> const &v);

template <class T> class victor {
 public:
  // Without -fguiding-decls, egcs and 2.95 will rightly think
  // this declares a non-template and so the program will fail
  // due to access violation below (and missing symbols at link time).
  friend T dot(victor<T> const &, victor<T> const &);

 private:
  T data[3];
};

template <class T> T dot(victor<T> const &u, victor<T> const &v)
{
  return  // access violation here:
    u.data[0] * v.data[0] +
    u.data[1] * v.data[1] +
    u.data[2] * v.data[2];
}

template double dot(victor<double> const &, victor<double> const &);

double function(victor<double> const &u,
                victor<double> const &v)
{
  double uu = dot(u, u);
  double uv = dot(u, v);
  double vv = dot(v, v);
  return (uv*uv)/(uu*vv);
}

int main() { return 0; }
#endif // VCL_NULL_TMPL_ARGS

//-------------------------------------

#ifdef VCL_NO_STATIC_DATA_MEMBERS
// VCL_NO_STATIC_DATA_MEMBERS is set to 1 if this fails to compile

template <class T> class vvv { static T xxx; };
template class vvv<int>;

int main() { return 0; }
#endif // VCL_NO_STATIC_DATA_MEMBERS

//-------------------------------------

#ifdef VCL_HAS_TEMPLATE_SYMBOLS
// VCL_HAS_TEMPLATE_SYMBOLS is set to 1 if this fails to link

// Declare a function template.
template <class T> void function(T *ptr, int n);

int caller()
{
  double array[3];
  function(array, 0); // This should call function<double>(double *, int);
  return 0;
}

// Define a non-template function with the same name and signature.
void function(double *, int) {}

// If the program links, the compiler didn't make a distinction.

int main() { return 0; }
#endif // VCL_HAS_TEMPLATE_SYMBOLS

//-------------------------------------

#ifdef VCL_CAN_DO_IMPLICIT_TEMPLATES

# ifdef _MSC_VER
// Use template typing to figure out correct method, because
// several MSVC versions can't cope with overloaded return types
template <class S> struct ims_what;

template <>
struct ims_what<double *> {
  typedef double type; };

template <class S>
struct ims_what {
  typedef int type; };


template <class I, class T>
void fsm_plop(I b, I e, T x, int)
{
  for (I p=b; p!=e; ++p)
    *p = x;
}

template <class T>
void fsm_plop(double *b, double *e, T x, double)
{
  for (double *p=b; p<e; ++p)
    *p = x;
}

template <class I, class T>
inline void fsm_plip(I b, I e, T x)
{
  if (b != e)
    fsm_plop(b, e, x, ims_what<I>::type());
}

# else
// FSM: The code is imitating the way the gcc STL chooses (or did choose, way
// back) between algorithms for different iterator types. A very brief look
// at the 3.2.2 <algorithm> header suggests they no longer use that mechanism
// so maybe it was deemed non-standard and abandoned.

struct fsm_plap_normal {};

template <class I>
inline fsm_plap_normal fsm_plap(I) { return fsm_plap_normal(); }

struct fsm_plap_double_star {};
inline fsm_plap_double_star fsm_plap(double *) { return fsm_plap_double_star(); }


template <class I, class T>
void fsm_plop(I b, I e, T x, fsm_plap_normal)
{
  for (I p=b; p!=e; ++p)
    *p = x;
}

template <class T>
void fsm_plop(double *b, double *e, T x, fsm_plap_double_star)
{
  for (double *p=b; p<e; ++p)
    *p = x;
}

template <class I, class T>
inline void fsm_plip(I b, I e, T x)
{
  if (b != e)
    fsm_plop(b, e, x, fsm_plap(b));
}

# endif

void f()
{
  int iarray[20];
  fsm_plip(iarray, iarray+20, 3141);

  double darray[20];
  fsm_plip(darray, darray+20, 2718);
}

int main() { return 0; }
#endif // VCL_CAN_DO_IMPLICIT_TEMPLATES

//-------------------------------------

#ifdef VCL_CAN_DO_COMPLETE_DEFAULT_TYPE_PARAMETER

template <class T> struct less {};

template <class T, class C=less<int> >
struct X
{
  typedef X<T,C> self;
  self foo(self const & t) {
    if ( t.a == 0 )
      return *this;
    else
      return t;
  }
 private:
  int a;
};

X<int> a;
X<int, less<short> > b;

int main() { return 0; }
#endif // VCL_CAN_DO_COMPLETE_DEFAULT_TYPE_PARAMETER

//-------------------------------------

#ifdef VCL_CAN_DO_TEMPLATE_DEFAULT_TYPE_PARAMETER

template <class T> struct less {};
template <class T, class C=less<T> > struct X { C t1; };
X<int> a;
X<int, less<short> > b;

int main() { return 0; }
#endif // VCL_CAN_DO_TEMPLATE_DEFAULT_TYPE_PARAMETER

//-------------------------------------

#ifdef VCL_SUNPRO_CLASS_SCOPE_HACK
// VCL_SUNPRO_CLASS_SCOPE_HACK(A) is set to ", A" if this fails to compile, to "" otherwise

template < class T >
struct allocator
{
  allocator() {}
  allocator(const allocator<T>& ) {}
};

template < class T , class Allocator = allocator < T > >
struct vector
{
  vector() {}
  ~vector() {}
};

template < class T >
struct spoof
{
  void set_row( unsigned , vector < T /*, allocator<T>*/ > const & );
};

template < class T >
void spoof < T > :: set_row( unsigned , vector < T /*, allocator<T>*/ > const & )
{
}

template class spoof < double >;

// If the program compiles, we don't need the hack

int main() { return 0; }
#endif // VCL_SUNPRO_CLASS_SCOPE_HACK

//-------------------------------------

#ifdef VCL_HAS_EXCEPTIONS

struct bizop {};

int functionella(char const *a, char const *b)
{
  if (!a &&  b)
    throw "a is no good";
  if ( a && !b)
    throw "b is no better";
  if (!a && !b)
    throw bizop();

  return *a - *b;
}

void monkeylette()
{
  try {
    functionella(  0,   0);
    functionella("a",   0);
    functionella(  0, "b");
    functionella("a", "b");
  }
  catch (char const *s) {
    // oops.
  }
  catch (bizop b) {
    // outch
  }
  catch (...) {
    // phew
  }
}

int main() { return 0; }
#endif // VCL_HAS_EXCEPTIONS

//-------------------------------------

#ifdef VCL_HAS_NAMESPACES

namespace foo {
  int hello() { return 10; }
};

// 7.3.1
namespace Outer {
  int i;
  namespace Inner {
    void f() { i++; } // Outer::i
    int i;
    void g() { i++; } // Inner::i
  }
}

// 7.3.1.1
namespace { int i; } // unique::i
void f() { i++; }    // unique::i  (gcc 2.7.2 fails here).

namespace A {
  namespace {
    int i;           // A::unique::i
    int j;           // A::unique::j
  }
  void g() { i++; }  // A::unique::i
}

using namespace A;
void h() {
  //i++;               // error: unique::i or A::unique::i
  A::i++;              // A::unique::i
  j++;                 // A::unique::j
}

extern "C" double vxl_sqrt(double){return 0;}

namespace foo {
  template <class T> struct complex { T re, im; };
  template <class T> T abs(complex<T> const &z) { return T(::vxl_sqrt(double(z.re*z.re + z.im+z.im))); }
}

namespace bar {
  int abs(int){return 0;}
  long abs(long){return 0;}
  float abs(float){return 0;}
  double abs(double){return 0;}
}

namespace diced {
  using foo::complex; // <-- I'm told vc60 fails here.
  using foo::abs;
  using bar::abs;
}

extern "C" int printf(char const *, ...);

void flegg() {
  int a = -1;
  long b = -2;
  float c = -3;
  double d = -4;
  diced::complex<double> e = { 3, 4 };
  printf("%d\n",  diced::abs(a)); // 1
  printf("%ld\n", diced::abs(b)); // 2
  printf("%f\n",  diced::abs(c)); // 3
  printf("%lf\n", diced::abs(d)); // 4
  printf("%lf\n", diced::abs(e)); // 5
}

int main() { return 0; }
#endif // VCL_HAS_NAMESPACES

//-------------------------------------

#ifdef VCL_ALLOWS_NAMESPACE_STD

#include <cmath>
#include <vector>
#include <iostream>
void function() {
  std::vector<double> flaz;
  flaz.push_back(std::sqrt(2.0));
  flaz.push_back(std::fabs(1.0f));
  std::cerr << "hello, std::world" << std::endl;
}

int main() { return 0; }
#endif // VCL_ALLOWS_NAMESPACE_STD

//-------------------------------------

#ifdef VCL_NEEDS_NAMESPACE_STD
// VCL_NEEDS_NAMESPACE_STD is set to 1 if this fails to compile

#include <cmath>
#include <vector>
//#include <iostream>
void function() {
  vector<double> flaz; // correct should be:  std::vector<double>
  flaz.push_back(sqrt(2.0));   // should be:  std::sqrt(2.0)
  flaz.push_back(fabs(1.0f));  // should be:  std::fabs(1.0)
  //cerr << "hello, world" << endl;
}

int main() { return 0; }
#endif // VCL_NEEDS_NAMESPACE_STD

//-------------------------------------

#ifdef VXL_UNISTD_USLEEP_IS_VOID
// VXL_UNISTD_USLEEP_IS_VOID is set to 1 if this test fails
#include <unistd.h>

int main() { int x = usleep(0); return x*0; }
#endif // VXL_UNISTD_USLEEP_IS_VOID

//-------------------------------------

#ifdef VXL_STDLIB_HAS_QSORT

// This is not a C++ header, strictly speaking.
// Actually, it is normative but deprecated, strictly speaking :)
#include <stdlib.h>
int f(const void *a,const void *b) { return 1; }

int main() { int a[5]; qsort(a, 5, sizeof(int), f); return 0; }
#endif // VXL_STDLIB_HAS_QSORT

//-------------------------------------

#ifdef VCL_NUMERIC_LIMITS_HAS_INFINITY
// Does vcl_numeric_limits<float>::has_infinity == 1?

// Several versions of gcc (3.0, 3.1, and 3.2) come with a
// numeric_limits that reports that they have no infinity.
#include <limits>
int main() {
  return std::numeric_limits<double>::has_infinity &&
         std::numeric_limits<float>::has_infinity ? 0 : 1;
}
#endif // VCL_NUMERIC_LIMITS_HAS_INFINITY

//-------------------------------------

#ifdef VCL_PROCESSOR_HAS_INFINITY
// Does the processor actually have an infinity?

// The Borland 5.5 defines DBL_MAX as _max_dble but only declares
// _max_dble in the std namespace if we include <cfloag>.  Including
// <float.h> moves _max_dble to the global namespace and allows the
// DBL_MAX macro to work.
#include <float.h>

union u {  double d;  unsigned char c[8]; };

int main()
{
  if (sizeof(double) != 8) return 1; // If you have an odd machine, then add
  // your own construction of infinity.

  u v;
  // Can we generate an IEEE infinity artifically on a big-endian machine?
  v.c[0] = 0x7f; v.c[1] = 0xf0;
  v.c[2] = v.c[3] = v.c[4] = v.c[5] = v.c[6] = v.c[7] = 0x00;
  if (v.d > DBL_MAX)
    return 0;

  // Can we generate an IEEE infinity artifically on a little-endian machine?
  v.c[7] = 0x7f; v.c[6] = 0xf0;
  v.c[0] = v.c[1] = v.c[2] = v.c[3] = v.c[4] = v.c[5] = 0x00;
  if (v.d > DBL_MAX)
    return 0;
  return 1;
}
#endif // VCL_PROCESSOR_HAS_INFINITY

//-------------------------------------

#ifdef VCL_CANNOT_SPECIALIZE_CV
// VCL_CANNOT_SPECIALIZE_CV is set to 1 if this fails to compile

// Some compilers do not distinguish between A<int> and A<int const>.

template <class T> struct A;
#if !defined(NOT_CONFORMING_SPECIALIZATION)
template <> struct A<int> {};
template <> struct A<int const> {};
#else
struct A<int> {};
struct A<int const> {};
#endif // VCL_CANNOT_SPECIALIZE_CV

int main() { return 0; }

#endif

//-------------------------------------

#ifdef VCL_TEMPLATE_MATCHES_TOO_OFTEN
// VCL_TEMPLATE_MATCHES_TOO_OFTEN is set to 1 if this fails to compile

// Some compilers will incorrectly choose the template over the
// non-template.  This will not compile if the template is chosen,
// which will reveal the bug.

class A {};
template <class T> void f(T t) { t.compiler_selected_wrong_overload(); }
void f(const A&) {}

int main()
{
  f(A());
  return 0;
}

#endif // VCL_TEMPLATE_MATCHES_TOO_OFTEN

//-------------------------------------

#ifdef VCL_HAS_SLICED_DESTRUCTOR_BUG
// VCL_HAS_SLICED_DESTRUCTOR_BUG is set to 1 if this program exist(1)s

// Some compilers (at least Intel C++ 7) will create a B temporary on
// the f(c) line below and call both the A and B constructors, but
// then destroy the temporary by calling only ~A() and not calling
// ~B() first (or ever).  This program will return 1 if the bug exists
// and 0 otherwise.

#include <stdlib.h>

struct A
{
  A(): mark(0) {}
  A(const A&): mark(0) {}
  ~A() { if (mark) { exit(1); } }
  int mark;
};

struct B: public A
{
  B(): A() {}
  B(const B& b): A(b) { mark = 1; }
  ~B() { mark = 0; }
};

struct C
{
  operator B () { return B(); }
};

void f(A) {}

int main()
{
  C c;
  f(c);
  return 0;
}

#endif // VCL_HAS_SLICED_DESTRUCTOR_BUG

//-------------------------------------

#ifdef VXL_HAS_TYPE_OF_SIZE
// This is used to check if (1) a type exists, (2) is has the required
// size in bytes, and (3) it is functional. The last requirement is
// driven by MSCV 6 which has __int64, but it is not fully
// functional. (It can't be cast to a double, for example.)

// CHAR_BIT is the number of bits per char.
#include <limits.h>
#ifndef CHAR_BIT
# define CHAR_BIT 8
#endif

#include "config.h"

#if INTEGRAL_TYPE
double cast( THE_TYPE a, unsigned THE_TYPE b, signed THE_TYPE c )
{
  return double( a ) + double( b ) + double( c );
}
#else // INTEGRAL_TYPE
double cast( THE_TYPE a )
{
  return double( a );
}
#endif // INTEGRAL_TYPE

int main()
{
  return sizeof(THE_TYPE) * CHAR_BIT == THE_SIZE ? 0 : 1;
}

#endif // VXL_HAS_TYPE_OF_SIZE

//-------------------------------------
