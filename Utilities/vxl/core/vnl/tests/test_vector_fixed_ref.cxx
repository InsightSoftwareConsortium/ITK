// This is core/vnl/tests/test_vector_fixed_ref.cxx
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_vector_fixed_ref.h>

#include <vcl_algorithm.h> // for vcl_generate()
#include <vcl_cstdlib.h> // for vcl_rand()
#include <testlib/testlib_test.h>

void test_vector_fixed_ref()
{
  enum{size = 4};
  typedef vnl_vector_fixed<double,size> vf;
  typedef vnl_vector_fixed_ref<double,size> vfr;
  typedef vnl_vector_fixed_ref_const<double,size> vfrc;

  int i;
  vf vec; // copy in
  for (i=0;i<size;++i)
  {
    vec(i) = i;
  }

  // vector fixed_ref tests


  // fixed_ref_const
  const vf & cvf = vec;
  vfrc cref(cvf);
  // check address
  for (i=0;i<size;++i)
  {
    TEST("const_address",&cref(i),&vec(i));
  }


  // fixed_ref (non-const)
  // wrap around vec
  vfr ref(vec);
  // check address
  for (i=0;i<size;++i)
  {
    TEST("nonconst_address",&ref(i),&vec(i));
  }


  //    assign from vec
  vf other;
  vcl_generate(other.begin(),other.end(),vcl_rand);
#if 0 // assignment is ambiguous
  ref = other;
  TEST("assign_vf", ref, other);
  // test different adresses
  TEST("assign_vf address", (ref.begin() != other.begin()), true);
#endif // 0

  {
  //    assign from const vfr
  vcl_generate(other.begin(),other.end(),vcl_rand);
  vfrc cref(other);
  ref = cref;
  TEST("assign_const_ref", ref, other);
  // test different adresses
  TEST("assign_const_ref address", (ref.begin() != other.begin()), true);
  }

  {
#if 0 // cannot assign to a vnl_vector_fixed_ref_const
  //    assign from vfr
  vcl_generate(other.begin(),other.end(),vcl_rand);
  vfr ref2(other);
  ref = ref2;
  TEST("assign_ref", ref, other);
  // test different adresses
  TEST("assign_ref address", (ref.begin() != other.begin()), true);
#endif // 0
  }

  // arithmetic
  {
    // plus
    vf a,b;
    vcl_generate(a.begin(),a.end(),vcl_rand);
    vcl_generate(b.begin(),b.end(),vcl_rand);
    vfrc arefc(a), brefc(b);
    vf mc = arefc + brefc;

    vfr aref(a), bref(b);
    vf m = aref + bref;

    vf m2 = arefc + bref;
    vf m3 = arefc + brefc;
    TEST("plus", mc, m);
    TEST("plus", mc, m2);
    TEST("plus", mc, m3);
  }
  {
    // times
    vf a,b;
    vcl_generate(a.begin(),a.end(),vcl_rand);
    vcl_generate(b.begin(),b.end(),vcl_rand);
    vfrc arefc(a), brefc(b);
    vf mc = arefc + brefc;

    vfr aref(a), bref(b);
    vf m = aref + bref;

    vf m2 = arefc + bref;
    vf m3 = arefc + brefc;
    TEST("plus", mc, m);
    TEST("plus", mc, m2);
    TEST("plus", mc, m3);

     aref.is_zero();
     arefc.is_zero();
  }
}

TESTMAIN(test_vector_fixed_ref)
