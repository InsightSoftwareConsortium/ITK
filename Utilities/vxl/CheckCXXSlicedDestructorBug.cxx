#include <stdlib.h>

struct A
{
  A(): mark(0) {}
  A(const A&): mark(0) {}
  ~A() { if(mark) { exit(1); } }
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
