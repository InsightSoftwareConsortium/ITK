//
// Check if the compiler support the C++11 alignas specifier
//

struct A
{
  char a;
};


struct alignas(64) B
{
  char a;
};

// This structure will generate a compiler error if the template
// argument is false
template<bool t> struct OnlyTrue;
template<> struct OnlyTrue<true> { static ITK_CONSTEXPR bool Result = true; };


int main(void)
{
  typedef A AlignedA alignas(64);
  return OnlyTrue<alignof( AlignedA ) == 64>::Result
    && OnlyTrue<alignof( B ) == 64>::Result;
}
