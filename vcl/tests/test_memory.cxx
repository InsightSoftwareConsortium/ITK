#include <vcl_cstdio.h>
#include <vcl_memory.h>

#define ASSERT(x,y) if (!(x)) { vcl_printf("FAIL: " y "\n"); status = 1; }

static int instances = 0;

struct A
{
  A() { ++instances; }
  ~A() { --instances; }
  A* self() {return this; }
};

struct B: public A {};

static int function_call(vcl_auto_ptr<A> a)
{
  return a.get()? 1:0;
}

static A* get_A(A& a) { return &a; }

static vcl_auto_ptr<A> generate_auto_ptr () { return vcl_auto_ptr<A>(new A); }

int test_memory_main(int /*argc*/,char* /*argv*/[])
{
  int status = 0;

  // Keep everything in a subscope so we can detect leaks.
  {
    vcl_auto_ptr<A> pa0;
    vcl_auto_ptr<A> pa1(new A());
    vcl_auto_ptr<B> pb1(new B());
    vcl_auto_ptr<A> pa2(new B());
    vcl_auto_ptr<A> pa3(pb1);

    A* ptr = get_A(*pa1);
    ASSERT(ptr == pa1.get(),
           "auto_ptr does not return correct object when dereferenced");
    ptr = pa1->self();
    ASSERT(ptr == pa1.get(),
           "auto_ptr does not return correct pointer from operator->");

    A* before = pa0.get();
    pa0.reset(new A());
    ASSERT(pa0.get() && pa0.get() != before,
           "auto_ptr does not hold a new object after reset(new A())");

    before = pa0.get();
    pa0.reset(new B());
    ASSERT(pa0.get() && pa0.get() != before,
           "auto_ptr does not hold a new object after reset(new B())");

    delete pa0.release();
    ASSERT(!pa0.get(), "auto_ptr holds an object after release()");

    pa1 = pa3;
    ASSERT(!pa3.get(), "auto_ptr holds an object after assignment to another");
    ASSERT(pa1.get(),
           "auto_ptr does not hold an object after assignment from another");

    int copied = function_call(pa2);
    ASSERT(copied, "auto_ptr did not receive ownership in called function");
    ASSERT(!pa2.get(), "auto_ptr did not release ownership to called function");


    pa3 = generate_auto_ptr();
    ASSERT(pa3.get(),
           "auto_ptr does not hold an object after assignment from factory function");
  }

  ASSERT(instances == 0, "auto_ptr leaked an object");

  // Test parts of <memory> related to C++ 0x (if available)
#if VCL_INCLUDE_CXX_0X
  // reset instance count for shared pointer tests
  instances = 0;
  {
    vcl_shared_ptr<A> spa0;
    vcl_shared_ptr<A> spa1(new A());
    vcl_shared_ptr<B> spb1(new B());
    vcl_shared_ptr<A> spa2(new B());
    vcl_shared_ptr<A> spa3(spb1);
    vcl_weak_ptr<A> wpa1(spa1);

    A* ptr = get_A(*spa1);
    ASSERT(ptr == spa1.get(),
           "shared_ptr does not return correct object when dereferenced");
    ptr = spa1->self();
    ASSERT(ptr == spa1.get(),
           "shared_ptr does not return correct pointer from operator->");

    // FIXME several more tests are needed here
  }

#endif //VCL_INCLUDE_CXX_0X

  return status;
}
