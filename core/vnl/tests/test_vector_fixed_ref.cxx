// This is core/vnl/tests/test_vector_fixed_ref.cxx
#include <algorithm>
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_vector_fixed_ref.h"

#include "testlib/testlib_test.h"

void
test_vector_fixed_ref()
{
  { // exercise conversions and verify const behavior
    const double numbers[4]{ 0, 11, 22, 33 };
    vnl_vector<double> v{ numbers, 4 };
    // vnl_vector_ref(numbers, 4); // Should fail to compile due to const double numbers
    vnl_vector_ref<double> memory_access_vnl_ref{ v.size(), v.data_block() };
    const vnl_vector_ref<double> const_cpprefvector{ memory_access_vnl_ref };

    const_cpprefvector.as_ref();
    // const_cpprefvector.fill(-99); //<- This should fail to compile, but it modifies the data
  }

  // Test conversion behaviors in the presence of move constructors/move assignments
  {
    constexpr double bulk_data_array[4]{ 1.0, 2.0, 3.0, 4.0 };
    vnl_vector_fixed<double, 4> initial_fixed_size_matrix(bulk_data_array);
    vnl_vector_ref<double> ref_to_data( initial_fixed_size_matrix.as_ref() );

    TEST("vnl_vector_ref{ vnl_vector_fixed } share data pointer",
         initial_fixed_size_matrix.data_block() == ref_to_data.data_block(),
         true);

    vnl_vector<double> new_independant_matrix{ ref_to_data };
    TEST("vnl_vector{ vnl_vector_ref } creates new memory",
         ref_to_data.data_block() != new_independant_matrix.data_block(),
         true);

    vnl_vector<double> rval_initialized_independant_matrix{ initial_fixed_size_matrix.as_ref() };
    TEST("vnl_vector{ .as_ref rval}) creates new memory",
         initial_fixed_size_matrix.data_block() != rval_initialized_independant_matrix.data_block(),
         true);
    //    std::cout << static_cast<void *>(initial_fixed_size_matrix.data_block()) << "\n"
    //      << static_cast<void *>(ref_to_data.data_block()) << "\n"
    //      << static_cast<void *>(new_independant_matrix.data_block()) << "\n"
    //      << static_cast<void *>(rval_initialized_independant_matrix.data_block()) << "\n";
  }

  enum
  {
    size = 4
  };
  typedef vnl_vector_fixed<double, size> vf;
  typedef vnl_vector_fixed_ref<double, size> vfr;
  typedef vnl_vector_fixed_ref_const<double, size> vfrc;

  int i;
  vf vec; // copy in
  for (i = 0; i < size; ++i)
  {
    vec(i) = i;
  }

  // vector fixed_ref tests


  // fixed_ref_const
  const vf & cvf = vec;
  vfrc cref(cvf);
  // check address
  for (i = 0; i < size; ++i)
  {
    TEST("const_address", &cref(i), &vec(i));
  }


  // fixed_ref (non-const)
  // wrap around vec
  vfr ref(vec);
  // check address
  for (i = 0; i < size; ++i)
  {
    TEST("nonconst_address", &ref(i), &vec(i));
  }


  //    assign from vec
  vf other;
  std::generate(other.begin(), other.end(), std::rand);

  {
    //    assign from const vfr
    std::generate(other.begin(), other.end(), std::rand);
    vfrc cref(other);
    ref = cref;
    TEST("assign_const_ref", ref, other);
    // test different addresses
    TEST("assign_const_ref address", (ref.begin() != other.begin()), true);
  }

  // arithmetic
  {
    // plus
    vf a, b;
    std::generate(a.begin(), a.end(), std::rand);
    std::generate(b.begin(), b.end(), std::rand);
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
    vf a, b;
    std::generate(a.begin(), a.end(), std::rand);
    std::generate(b.begin(), b.end(), std::rand);
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
