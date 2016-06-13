//
// Check if the compoler support the GNU attribute extension for
// alignment, and does not contain a bug which causes internal
// compiler segfault.
//

struct A
{
  char a;
};

struct B
{
  char b;
} __attribute__ ((aligned (64)));

// fail for gcc 4.1
#if __GNUC__ == 4 && __GNUC_MINOR__ == 1
#error This version of GCC is know to have a internal compilation error with this feature in ITK.
#endif

// BUG DETECTION: This following usage may generate a segfault during
// compilation.
//
// The following block of code causes an internal compiler error with
// Apple's (GCC) 4.2.1 (Apple Inc. build5666) (dot 3) compiler when
// compiled in debug mode with the -g flag.
//
template <typename T>
class foo
{
  // NOTE: implicit constructor is required for this test

  struct A
  {
    char a;
  };
  typedef A AlignedA __attribute__ ((aligned(64)));
  AlignedA *AlignedElementsOfA;
};


// This structure will generate a compiler error if the template
// argument is false
template<bool t> struct OnlyTrue;
template<> struct OnlyTrue<true> { static bool Result = true; };


int main()
{
  foo<int> f;

  typedef A AlignedA __attribute__ ((aligned(64)));

  return OnlyTrue<__alignof__( AlignedA ) == 64>::Result
    && OnlyTrue<__alignof__( B ) == 64>::Result;


  return 0;
}
