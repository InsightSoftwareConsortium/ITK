// This is core/vnl/tests/test_vector.cxx
#include <iostream>
#include <sstream>
#include <vnl/vnl_math.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_float_3.h>
#include <vnl/vnl_float_4.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_cross.h>
#include <testlib/testlib_test.h>

void vnl_vector_test_int()
{

  std::cout << "***********************\n"
           << "Testing vnl_vector<int>\n"
           << "***********************\n";

  //////////////////
  // CONSTRUCTORS //
  //////////////////

  //  vnl_vector();
  vnl_vector<int> v0;
  TEST("vnl_vector<int> v0()", v0.size(), 0);

  //  vnl_vector(unsigned int len);
  vnl_vector<int> v1(2);
  TEST("vnl_vector<int> v1(2)", v1.size(), 2);

  //  vnl_vector(unsigned int len, T const& v0);
  vnl_vector<int> v2(2,2);
  TEST("vnl_vector<int> v2(2,2)", (v2.get(0)==2 && v2.get(1)==2), true);

  //  vnl_vector(unsigned int len, int n, T const values[]);
  int vcvalues[] = {1,0};
  vnl_vector<int> vc(2,2,vcvalues);
  TEST("vnl_vector<int> vc(2,2,int[])", (vc(0)==1 && vc(1)==0), true);

  //  vnl_vector(T const* data_block,unsigned int n);
  vnl_vector<int> vb(vcvalues,2);
  TEST("vnl_vector<int> vb(int[],2)", (vb(0)==1 && vb(1)==0), true);

  //  vnl_vector(vnl_vector<T> const&);
  vnl_vector<int> v_copy(vb);
  TEST("vnl_vector<int> v_copy(vb)", (v_copy(0)==1 && v_copy(1)==0), true);

  ///////////////
  // ACCESSORS //
  ///////////////

#if VNL_CONFIG_CHECK_BOUNDS

  {
  bool exceptionThrownAndCaught = false;
  try { v0.get(25); }  // Raise out of bounds exception.
  catch(...) { exceptionThrownAndCaught = true; }
  TEST("Out of bounds get()", exceptionThrownAndCaught, true);

  exceptionThrownAndCaught = false;
  try { v0.put(25,0); }  // Raise out of bounds exception.
  catch(...) { exceptionThrownAndCaught = true; }
  TEST("Out of bounds put()", exceptionThrownAndCaught, true);
  }

#endif

  //// test constructors, accessors
//   TEST("v0.set_compare", (v0.set_compare(int_equal), true), true);
  TEST("v1=2", (v1=2, (v1.get(0)==2 && v1.get(1)==2)), true);
  TEST("v1 == v2", (v1 == v2), true);
  TEST("v0 = v2", ((v0 = v2), (v0 == v2)), true);
  TEST("v2.put(1,3)", (v2.put(1,3),v2.get(1)), 3);
  TEST("v2.get(1)", v2.get(1), 3);
  TEST("v0 == v2", (v0 == v2), false);
  TEST("v0 != v2", (v0 != v2), true);
  TEST("(v0 == v2)", (v0 == v2), false);
  TEST("v1.fill(3)", (v1.fill(3), (v1.get(0)==3 && v1.get(1)==3)), true);
  TEST("v2.fill(2)", (v2.fill(2), (v2.get(0)==2 && v2.get(1)==2)), true);
  int v3values [] = {1,2,3};
  vnl_vector<int> v3(3,3,v3values);
  TEST("v3(3,3,{1,2,3})",(v3.get(0)==1 && v3.get(1)==2 && v3.get(2)==3), true);
  vnl_vector<int> v4(v3);
  TEST("vnl_vector<int> v4(v3)", v3, v4);
  TEST("v0=v2", (v0=v2, v0), v2);

  //// test additions and subtractions
  TEST("v0=v2+3", ((v0=v2+3), (v0.get(0)==5 && v0.get(1)==5)), true);
  TEST("v0=3+v2", ((v0=3+v2), (v0.get(0)==5 && v0.get(1)==5)), true);
  TEST("v0+=(-3)", (v0+=(-3), (v0.get(0)==2 && v0.get(1)==2)), true);
  TEST("v0-=(-3)", (v0-=(-3), (v0.get(0)==5 && v0.get(1)==5)), true);
  TEST("v0=v2-3", ((v0=v2-3), (v0.get(0)==-1 && v0.get(1)==-1)), true);
  TEST("v0=3-v2", ((v0=3-v2), (v0.get(0)==1 && v0.get(1)==1)), true);
  TEST("v0= -v2", (v0= -v2, (v0.get(0)==-2 && v0.get(1)==-2)), true);

  vnl_vector<int> v5(2);
  v0 = v2;
  TEST("v5=v0+v2", ((v5=v0+v2), (v5.get(0)==4 && v5.get(1)==4)), true);
  TEST("v5=v0-v2", ((v5=v0-v2), (v5.get(0)==0 && v5.get(1)==0)), true);
  TEST("v0+=v2", ((v0+=v2), (v0.get(0)==4 && v0.get(1)==4)), true);
  TEST("v0-=v2", ((v0-=v2), (v0.get(0)==2 && v0.get(1)==2)), true);

  //// test multiplications and divisions
  TEST("v4=v3*5", ((v4=v3*5), (v4.get(0)==5 && v4.get(1)==10 && v4.get(2)==15)), true);

  TEST("v4=5*v3", ((v4=5*v3), (v4.get(0)==5 && v4.get(1)==10 && v4.get(2)==15)), true);
  TEST("v3*=5",((v3*=5), (v3== v4)), true);
  TEST("v4=v3/5", ((v4=v3/5), (v4.get(0)==1 && v4.get(1)==2 && v4.get(2)==3)), true);
  TEST("v3/=5", ((v3/=5), (v3==v4)), true);

  //// additional tests
  int vvalues [] = {0,-2,2,0};
  vnl_vector<int> v(4,4,vvalues);
  v0 = v; v1 = v; v2 = v;
  TEST("v(i)", (v(0)==0 && v(1)==-2 && v(2)==2 && v(3)==0), true);

  TEST("v.max_value()", v.max_value(),  2);
  TEST("v.min_value()", v.min_value(), -2);
  TEST("v.arg_max()",   v.arg_max(),   2);
  TEST("v.arg_min()",   v.arg_min(),   1);
  TEST("element_product(v,v)",
       ((v1 = element_product(v,v)),
        (v1(0)==0 && v1(1)==4 && v1(2)==4 && v1(3)==0)), true);
  TEST("element_quotient(v,[2])",
       ((v2 = 2),
        (v1 = element_quotient(v,v2)),
        (v1(0)==0 && v1(1)==-1 && v1(2)==1 && v1(3)==0)), true);
  TEST("v.extract(1,3)",
       ((v1 = v.extract(1,3)),
        (v1.size()==1 && v1(0)==v(3))), true);
  TEST("v.update([4],3)",
       ((v1=4),
        (v.update(v1,3)),
        (v(0)==0 && v(1)==-2 && v(2)==2 && v(3)==4)), true);

  {                                             // new scope to reuse variables
    int vvalues [] = {1,0,0,0};
    vnl_vector<int> v (4,4,vvalues);
    int v1values [] = {1,0,0};
    int v2values [] = {0,1,0};
    int v3values [] = {0,0,1};
    vnl_vector<int> v1(3,3,v1values);
    vnl_vector<int> v2(3,3,v2values);
    vnl_vector<int> v3(3,3,v3values);
    TEST("dot_product(v1,v2)",
         (dot_product(v1,v2)==0 && dot_product(v1,v3)==0 && dot_product(v2,v3)==0), true);
    TEST("dot_product(v1,v1)",
         (dot_product(v1,v1)==1 && dot_product(v2,v2)==1 && dot_product(v3,v3)==1), true);
    TEST("4d-v=3d-v", ((v = v3), v.size()==3 && v==v3), true);
    TEST("vnl_cross_3d(v1,v2)", vnl_cross_3d(v1,v2), v3);
    TEST("vnl_cross_3d(v2,v3)", vnl_cross_3d(v2,v3), v1);
    TEST("vnl_cross_3d(v1,v3)", vnl_cross_3d(v1,v3), -v2);
    vnl_vector<int> vv(2,0);
    v1 = vv; v1[0]=1;
    v2 = vv; v2[1]=1;
    TEST("vnl_cross_2d(v1,v2)", vnl_cross_2d(v1,v2)==1, true);
  }

  {
    int vvalues [] = {1, 2, 3};
    vnl_vector<int> v (3, 3, vvalues);
    vnl_matrix<int> m = outer_product(v, v);
    TEST("outer_product",
         (m(0,0)==1 && m(0,1)==2 && m(0,2)==3 &&
          m(1,0)==2 && m(1,1)==4 && m(1,2)==6 &&
          m(2,0)==3 && m(2,1)==6 && m(2,2)==9), true);
  }
  {
    int vvalues [] = {1,0,0,0};
    vnl_vector<int> v(4,4,vvalues);
    TEST("v.squared_magnitude", (v.squared_magnitude()==1), true);
    TEST("v.magnitude", (v.magnitude()==1), true);
    // normalize not sensible for ints
    //TEST("v.normalize", (v1 = 3 * v, v1.normalize(), v1), v);
  }

  //////////
  // FLIP //
  //////////

  {
  int vvalues [] = {0,1,2,3};
  vnl_vector<int> v(4,4,vvalues);
  TEST("v",
       (0 == v[0] && 1 == v[1] && 2 == v[2] && 3 == v[3]), true);
  v.flip();
  TEST("v.flip()",
       (3 == v[0] && 2 == v[1] && 1 == v[2] && 0 == v[3]), true);
  v.flip(0,v.size());
  TEST("v.flip(0,v.size())",
       (0 == v[0] && 1 == v[1] && 2 == v[2] && 3 == v[3]), true);
  v.flip(0,3);
  TEST("v.flip(0,3)",
       (2 == v[0] && 1 == v[1] && 0 == v[2] && 3 == v[3]), true);
  v.flip(1,4);
  TEST("v.flip(1,4)",
       (2 == v[0] && 3 == v[1] && 0 == v[2] && 1 == v[3]), true);
  }

  //////////
  // ROLL //
  //////////

  {
  int vvalues [] = {0,1,2,3};
  vnl_vector<int> v(4,4,vvalues);
  vnl_vector<int> v_temp;

  //
  // Check special cases
  //

  v_temp = v;
  TEST("v[0]",
       (0 == v_temp[0] && 1 == v_temp[1] && 2 == v_temp[2] && 3 == v_temp[3]), true);
  v_temp = v.roll(0);
  TEST("v.roll(0)",
       (0 == v_temp[0] && 1 == v_temp[1] && 2 == v_temp[2] && 3 == v_temp[3]), true);
  v_temp = v.roll(v.size());
  TEST("v.roll(v.size())",
       (0 == v_temp[0] && 1 == v_temp[1] && 2 == v_temp[2] && 3 == v_temp[3]), true);
  v_temp = v.roll(-1*static_cast<long signed int>(v.size()));
  TEST("v.roll(-v.size())",
       (0 == v_temp[0] && 1 == v_temp[1] && 2 == v_temp[2] && 3 == v_temp[3]), true);

  //
  // Check:
  // -- Positive, in range
  // -- Positive, out of range
  // -- Negative, in range
  // -- Negative, out of range
  //

  v_temp = v.roll(1); // Positive, in range
  TEST("v.roll(1)",
       (3 == v_temp[0] && 0 == v_temp[1] && 1 == v_temp[2] && 2 == v_temp[3]), true);
  v_temp = v.roll(5); // Positive, in range
  TEST("v.roll(5)",
       (3 == v_temp[0] && 0 == v_temp[1] && 1 == v_temp[2] && 2 == v_temp[3]), true);
  v_temp = v.roll(-1); // Positive, in range
  TEST("v.roll(-1)",
       (1 == v_temp[0] && 2 == v_temp[1] && 3 == v_temp[2] && 0 == v_temp[3]), true);
  v_temp = v.roll(-5); // Positive, in range
  TEST("v.roll(-5)",
       (1 == v_temp[0] && 2 == v_temp[1] && 3 == v_temp[2] && 0 == v_temp[3]), true);
  }

  //////////////////
  // ROLL INPLACE //
  //////////////////

  {
  int vvalues [] = {0,1,2,3};
  vnl_vector<int> v(4,4,vvalues);
  vnl_vector<int> v_temp = v;

  //
  // Check special cases
  //

  TEST("v[0]",
       (0 == v[0] && 1 == v[1] && 2 == v[2] && 3 == v[3]), true);
  v.roll_inplace(0);
  TEST("v.roll_inplace(0)",
       (0 == v[0] && 1 == v[1] && 2 == v[2] && 3 == v[3]), true);
  v.roll_inplace(v.size());
  TEST("v.roll_inplace(v.size())",
       (0 == v[0] && 1 == v[1] && 2 == v[2] && 3 == v[3]), true);
  v.roll_inplace(-1*static_cast<long signed int>(v.size()));
  TEST("v.roll_inplace(-v.size())",
       (0 == v[0] && 1 == v[1] && 2 == v[2] && 3 == v[3]), true);

  //
  // Check:
  // -- Positive, in range
  // -- Positive, out of range
  // -- Negative, in range
  // -- Negative, out of range
  //

  v = v_temp, v.roll_inplace(1); // Positive, in range
  TEST("v.roll_inplace(1)",
       (3 == v[0] && 0 == v[1] && 1 == v[2] && 2 == v[3]), true);
  v = v_temp, v.roll_inplace(5); // Positive, in range
  TEST("v.roll_inplace(5)",
       (3 == v[0] && 0 == v[1] && 1 == v[2] && 2 == v[3]), true);
  v = v_temp, v.roll_inplace(-1); // Positive, in range
  TEST("v.roll_inplace(-1)",
       (1 == v[0] && 2 == v[1] && 3 == v[2] && 0 == v[3]), true);
  v = v_temp, v.roll_inplace(-5); // Positive, in range
  TEST("v.roll_inplace(-5)",
       (1 == v[0] && 2 == v[1] && 3 == v[2] && 0 == v[3]), true);
  }
}

bool float_equal(const float& f1, const float& f2)
{
  return std::fabs(f1 - f2) < 1.0e-6;
}

void vnl_vector_test_float()
{
  std::cout << "*************************\n"
           << "Testing vnl_vector<float>\n"
           << "*************************\n";
  //// test constructors, accessors
  vnl_vector<float> v0;
  TEST("vnl_vector<float> v0()", v0.size(), 0);
  vnl_vector<float> v1(2);
  TEST("vnl_vector<float> v1(2)", v1.size(), 2);
  vnl_vector<float> v2(2,2);
  TEST("vnl_vector<float> v2(2,2)", (v2.get(0)==2 && v2.get(1)==2), true);

  float vcvalues[2] = {1};
  vnl_vector<float> vc(2,2,vcvalues);
  TEST("vnl_vector<float> vc(2,2,float[])", (vc(0)==1 && vc(1)==0), true);
  TEST("v1=2", (v1=2, (v1.get(0)==2 && v1.get(1)==2)), true);
  TEST("v1 == v2", (v1 == v2), true);
  TEST("v0 = v2", ((v0 = v2), (v0 == v2)), true);
  TEST("v2.put(1,3)", (v2.put(1,3),v2.get(1)), 3);
  TEST("v2.get(1)", v2.get(1), 3);
  TEST("v0 == v2", (v0 == v2), false);
  TEST("v0 != v2", (v0 != v2), true);
  TEST("(v0 == v2)", (v0 == v2), false);
  TEST("v1.fill(3)", (v1.fill(3), (v1.get(0)==3 && v1.get(1)==3)), true);
  TEST("v2.fill(2)", (v2.fill(2), (v2.get(0)==2 && v2.get(1)==2)), true);
  vnl_vector<float> v3 = vnl_float_3(1.f,2.f,3.f).as_vector();
  TEST("v3(3)",(v3.get(0)==1 && v3.get(1)==2 && v3.get(2)==3), true);
  vnl_vector<float> v4(v3);
  TEST("vnl_vector<float> v4(v3)", v3, v4);
  TEST("v0=v2", (v0=v2, (v0==v2)), true);
  std::cout << &v0 << " == " << v0 << std::endl;
  TEST("<<", 1, 1);

  //// test additions and subtractions
  TEST("v0=v2+3", ((v0=v2+3), (v0.get(0)==5 && v0.get(1)==5)), true);
  TEST("v0=3+v2", ((v0=3.0f+v2), (v0.get(0)==5 && v0.get(1)==5)), true);
  TEST("v0+=(-3)", (v0+=(-3), (v0.get(0)==2 && v0.get(1)==2)), true);
  TEST("v0-=(-3)", (v0-=(-3), (v0.get(0)==5 && v0.get(1)==5)), true);
  TEST("v0=v2-3", ((v0=v2-3), (v0.get(0)==-1 && v0.get(1)==-1)), true);
  TEST("v0=3-v2", ((v0=3.0f-v2), (v0.get(0)==1 && v0.get(1)==1)), true);
  TEST("v0= -v2", (v0= -v2, (v0.get(0)==-2 && v0.get(1)==-2)), true);

  vnl_vector<float> v5(2);
  v0 = v2;
  TEST("v5=v0+v2", ((v5=v0+v2), (v5.get(0)==4 && v5.get(1)==4)), true);
  TEST("v5=v0-v2", ((v5=v0-v2), (v5.get(0)==0 && v5.get(1)==0)), true);
  TEST("v0+=v2", ((v0+=v2), (v0.get(0)==4 && v0.get(1)==4)), true);
  TEST("v0-=v2", ((v0-=v2), (v0.get(0)==2 && v0.get(1)==2)), true);

  //// test multiplications and divisions
  TEST("v4=v3*5", ((v4=v3*5), (v4.get(0)==5 && v4.get(1)==10 && v4.get(2)==15)), true);

  TEST("v4=5*v3", ((v4=5.0f*v3), (v4.get(0)==5 && v4.get(1)==10 && v4.get(2)==15)), true);
  TEST("v3*=5",((v3*=5), (v3== v4)), true);
  TEST("v4=v3/5", ((v4=v3/5), (v4.get(0)==1 && v4.get(1)==2 && v4.get(2)==3)), true);
  TEST("v3/=5", ((v3/=5), (v3==v4)), true);

  //// additional tests
//  vnl_vector<float> v(4,4,0,-2,2,0); no var args with floats
  float vvalues [] = {0,-2,2,0};
  vnl_vector<float> v(4,4,vvalues);
  v[0] = 0;
  v[1] = -2;
  v[2] = 2;
  v[3] = 0;
  v0 = v; v1 = v; v2 = v;
  TEST("v(i)", (v(0)==0 && v(1)==-2 && v(2)==2 && v(3)==0), true);

  TEST("v.max_value()", v.max_value(),  2);
  TEST("v.min_value()", v.min_value(), -2);
  TEST("v.arg_max()",   v.arg_max(),   2);
  TEST("v.arg_min()",   v.arg_min(),   1);
  TEST("element_product(v,v)",
       ((v1 = element_product(v,v)),
        (v1(0)==0 && v1(1)==4 && v1(2)==4 && v1(3)==0)), true);
  TEST("element_quotient(v,[2])",
       ((v2 = 2),
        (v1 = element_quotient(v,v2)),
        (v1(0)==0 && v1(1)==-1 && v1(2)==1 && v1(3)==0)), true);
  TEST("v.extract(1,3)",
       ((v1 = v.extract(1,3)),
        (v1.size()==1 && v1(0)==v(3))), true);
  TEST("v.update([4],3)",
       ((v1=4),
        (v.update(v1,3)),
        (v(0)==0 && v(1)==-2 && v(2)==2 && v(3)==4)), true);

  {                                             // new scope to reuse variables
    float vvalues [] = {1,0,0,0};
    vnl_vector<float> v (4,4,vvalues);
    v[0] = 1;
    v[1] = 0;
    v[2] = 0;
    v[3] = 0;
    TEST("v(i)",
         (v(0)==v[0] && v[0]==1 &&
          v(1)==v[1] && v[1]==0 &&
          v(2)==v[2] && v[2]==0 &&
          v(3)==v[3] && v[3]==0), true);
    vnl_vector<float> v1(3,0.f); v1[0] = 1.f;
    vnl_vector<float> v2(3,0.f); v2[1] = 1.f;
    vnl_vector<float> v3(3,0.f); v3[2] = 1.f;
    TEST("dot_product(v1,v2)",
         (dot_product(v1,v2)==0 && dot_product(v1,v3)==0 && dot_product(v2,v3)==0), true);
    TEST("4d-v=3d-v", ((v = v3), v.size()==3 && v==v3), true);
    TEST("vnl_cross_3d(v1,v2)", vnl_cross_3d(v1,v2), v3);
    TEST("vnl_cross_3d(v2,v3)", vnl_cross_3d(v2,v3), v1);
    TEST("vnl_cross_3d(v1,v3)", vnl_cross_3d(v1,v3), -v2);
    vnl_vector<float> vv(2,0);
    v1 = vv; v1[0]=1;
    v2 = vv; v2[1]=1;
    TEST("vnl_cross_2d(v1,v2)", vnl_cross_2d(v1,v2), 1);
  }

  {
    vnl_float_3 v(1.f,2.f,3.f);
    vnl_float_4 v2(1.f,2.f,3.f,4.f);
    vnl_matrix_fixed<float,3,4> m = outer_product(v, v2);
    TEST("outer_product -> fixed 3 x fixed 4",
         (m(0,0)==1 && m(0,1)==2 && m(0,2)==3 && m(0,3) == 4 &&
          m(1,0)==2 && m(1,1)==4 && m(1,2)==6 && m(1,3) == 8 &&
          m(2,0)==3 && m(2,1)==6 && m(2,2)==9 && m(2,3) == 12), true);
  }
  {
    vnl_float_3 v(1.f,2.f,3.f);
    TEST("vnl_float_3 v(1.f,2.f,3.f)", v.size(), 3);
    v[0] = 1.f; v[1] = 2.f; v[2] = 3.f;
    TEST("v[0]=1 and v[0]", v[0], 1);
    TEST("v[1]=2 and v[1]", v[1], 2);
    TEST("v[2]=3 and v[2]", v[2], 3);
    vnl_vector<float> v1(3, 0.f); v1[0]=1.f;
    std::cout << "v1 = " << v1 << std::endl;
    vnl_vector<float> v2(3, 0.f); v2[1]=1.f;
    std::cout << "v2 = " << v2 << std::endl;
    vnl_vector<float> v3(3, 0.f); v3[0]=-0.5f; v3[2]=0.5f;
    std::cout << "v3 = " << v3 << std::endl
             << "v1 - v2 = " << v1 - v2 << std::endl;
    double ang = angle(v1,v2);
    std::cout << "angle(v1,v2) = " << ang << std::endl;
    ang *= vnl_math::deg_per_rad; // == 180/pi
    std::cout << "angle(v1,v2) in degrees = " << ang << std::endl
             << "v1.size()=" << v1.size() << std::endl
             << "v2.size()=" << v2.size() << std::endl
             << "vnl_cross_2d(v1,v2) = " << vnl_cross_2d(v1,v2) << std::endl
             << "vnl_cross_3d(v1,v2) = " << vnl_cross_3d(v1,v2) << std::endl;
    TEST_NEAR("angle(v1,v2)", ang, 90.0, 1e-15);
    double ang2 = angle(v1,v3);
    std::cout << "angle(v1,v3) = " << ang << std::endl;
    ang2 *= vnl_math::deg_per_rad; // == 180/pi
    std::cout << "angle(v1,v3) in degrees = " << ang2 << std::endl;
    TEST_NEAR("angle(v1,v3)", ang2, 135.0, 1e-6);
    TEST_NEAR("mean", vnl_c_vector<float>::mean(v.begin(), v.size()), 2.0, 1e-6);
    TEST_NEAR("std", vnl_c_vector<float>::std(v.begin(), v.size()), 1.0, 1e-6);
  }

  {
    double vvalues [] = {1,0,0,0};
    vnl_vector<double> vd (4);
    vnl_vector<double> v (4,4,vvalues);
    v[0] = 1;
    v[1] = 0;
    v[2] = 0;
    v[3] = 0;
    TEST("v.squared_magnitude", v.squared_magnitude(), 1);
    TEST("v.magnitude", v.magnitude(), 1);
// Trying to track down test failure in Intel 10.0 compiler
    vd = 4.0 * v;
    std::cout << "vd.normalize() is " << vd.normalize() << " and v is " << v << "\n" << std::flush;
    std::cout << "vd.normalize() - v is " << vd.normalize() - v << "\n" << std::flush;
    TEST("v.normalize", (vd = 4.0 * v, vd.normalize(), vd), v);
  }

  {
    float vvalues [] = {-7,-2,-3,-4};
    vnl_vector<float> v(4,4,vvalues);
    v[0] = -7;
    v[1] = -2;
    v[2] = -3;
    v[3] = -4;
    TEST("v(i)", (v(0)==-7 && v(1)==-2 && v(2)==-3 && v(3)==-4), true);

    TEST("v.max_value()", v.max_value(),  -2);
    TEST("v.min_value()", v.min_value(), -7);
    TEST("v.arg_max()",   v.arg_max(),   1);
    TEST("v.arg_min()",   v.arg_min(),   0);
  }

  {
    double vvalues [] = {-7,-2,-3,-4};
    vnl_vector<double> v(4,4,vvalues);
    v[0] = -7;
    v[1] = -2;
    v[2] = -3;
    v[3] = -4;
    TEST("v(i)", (v(0)==-7 && v(1)==-2 && v(2)==-3 && v(3)==-4), true);

    TEST("v.max_value()", v.max_value(),  -2);
    TEST("v.min_value()", v.min_value(), -7);
    TEST("v.arg_max()",   v.arg_max(),   1);
    TEST("v.arg_min()",   v.arg_min(),   0);
  }

  // We should test odd sized vector's for the special SSE2 handling of
  // different sizes.
  {
    float vvalues [] = {-7,-2,-3,-4, 5};
    vnl_vector<float> v(5,5,vvalues);
    v[0] = -7;
    v[1] = -2;
    v[2] = -3;
    v[3] = -4;
    v[4] = 5;
    TEST("v(i)", (v(0)==-7 && v(1)==-2 && v(2)==-3 && v(3)==-4 && v(4)==5), true);

    TEST("v.max_value()", v.max_value(),  5);
    TEST("v.min_value()", v.min_value(), -7);
    TEST("v.arg_max()",   v.arg_max(),   4);
    TEST("v.arg_min()",   v.arg_min(),   0);
  }

  {
    float vvalues [] = {-7,-2,-3,-4, 5, 2};
    vnl_vector<float> v(6,6,vvalues);
    v[0] = -7;
    v[1] = -2;
    v[2] = -3;
    v[3] = -4;
    v[4] = 5;
    v[5] = 2;
    TEST("v(i)", (v(0)==-7 && v(1)==-2 && v(2)==-3 && v(3)==-4 && v(4)==5 && v(5)==2), true);

    TEST("v.max_value()", v.max_value(),  5);
    TEST("v.min_value()", v.min_value(), -7);
    TEST("v.arg_max()",   v.arg_max(),   4);
    TEST("v.arg_min()",   v.arg_min(),   0);
  }
  {
    double vvalues [] = {-7,-2,-3,-4, 5};
    vnl_vector<double> v(5,5,vvalues);
    v[0] = -7;
    v[1] = -2;
    v[2] = -3;
    v[3] = -4;
    v[4] = 5;
    TEST("v(i)", (v(0)==-7 && v(1)==-2 && v(2)==-3 && v(3)==-4 && v(4)==5), true);

    TEST("v.max_value()", v.max_value(),  5);
    TEST("v.min_value()", v.min_value(), -7);
    TEST("v.arg_max()",   v.arg_max(),   4);
    TEST("v.arg_min()",   v.arg_min(),   0);
  }

  TEST("vnl_vector_ssd",
       vnl_vector_ssd(vnl_vector<float>(4, 0.0f), vnl_vector<float>(4, 1.0f)),
       4.0);
}

void vnl_vector_test_matrix()
{
  int mvalues[] = {1,2,3,
                   4,5,6};                      // product with matrices
  vnl_matrix<int> m(2,3,6, mvalues);

  int v2values [] = {1,0};
  int v3values [] = {1,0,0};
  vnl_vector<int> v, v2(2,2,v2values), v3(3,3,v3values);
  TEST("v.pre_multiply(m)",
       ((v = v3),
        (v.pre_multiply(m)),
        (v.size()==2 && v(0)==1 && v(1)==4)), true);
  TEST("v.post_multiply(m)",
       ((v = v2),
        (v.post_multiply(m)),
        (v.size()==3 && v(0)==1 && v(1)==2 && v(2)==3)), true);
  TEST("v*=m",
       ((v = v2),
        (v *= m),
        (v.size()==3 && v(0)==1 && v(1)==2 && v(2)==3)), true);
  TEST("v2*m",
       ((v = v2 * m),
        (v.size()==3 && v(0)==1 && v(1)==2 && v(2)==3)), true);
  TEST("m*v3",
       ((v = m * v3),
        (v.size()==2 && v(0)==1 && v(1)==4)), true);
}

void vnl_vector_test_conversion()
{
  bool check;
  {
    // convert from a vnl_vector to a block array:
    int v1values[] = {1,2,3, 4,5,6, 7,8,9, 10,11,12};
    vnl_vector<int> v1 (12, 12, v1values);
    const int* data = v1.data_block();
    {
      check = true;
      for (int d = 0; d < 12; d++)
        if (data[d] != d+1)
          check = false;
    }
    TEST("(const int*) m.data_block", check, true);

    typedef int block [12];
    const block& v2 = *((const block*) data);
    {
      check = true;
      for (int i = 0; i < 12; i++)
        if (v1(i) != v2[i])
          check = false;
    }
    TEST("matrix(i)==block[i]", check, true);

    // convert from a block array to a vnl_vector:
    block b1;
    for (int i = 0; i < 12; i++)
      b1[i] = i;
    data = ((const int*) b1);
    {
      check = true;
      for (int d = 0; d < 12; d++)
        if (data[d] != d)
          check = false;
    }
    TEST("(const int*) block", check, true);
    vnl_vector<int> b2(data, 12);
    {
      check = true;
      for (int i = 0; i < 12; i++)
        if (b1[i] != b2(i))
          check = false;
    }
    TEST("block[i]==matrix(i)", check, true);
  }
}

static void vnl_vector_test_io()
{
  double expected_data[] = {1.0, 2.0, 3.0};
  vnl_vector<double> expected(expected_data, 3);
  {
    std::stringstream ss;
    ss << "";
    vnl_vector<double> p;
    ss >> p;
    TEST("number of values read from stream, empty", p.size(), 0);
  }
  {
    std::stringstream ss;
    ss << "\n ";
    vnl_vector<double> p;
    ss >> p;
    TEST("number of values read from stream, just WS", p.size(), 0);
  }
  {
    std::stringstream ss;
    ss << "1 2 3.0";
    vnl_vector<double> p;
    ss >> p;
    TEST("number of values read from stream, no newline", p, expected);
  }
  {
    std::stringstream ss;
    ss << "1 2 3.0\n";
    vnl_vector<double> p;
    ss >> p;
    TEST("number of values read from stream, newline", p, expected);
  }
  {
    std::stringstream ss;
    ss << "1 2 3.0\n ";
    vnl_vector<double> p;
    ss >> p;
    TEST("number of values read from stream, newline + WS", p, expected);
  }
  {
    std::stringstream ss;
    ss << "5";
    vnl_vector<double> p;
    ss >> p;
    TEST("single value read from stream, no newline or ws", p.size() == 1 && p(0)==5, true);
  }
}

#ifndef TIMING
#define TIMING 0
#endif

#if TIMING
#include <vul/vul_timer.h>
void vnl_vector_test_two_nrm2_timing(unsigned size, unsigned long num)
{
  vnl_vector<double> a(size);
  for (unsigned i= 0; i < size; i++)
    a(i) = i / size;

  double c=0;
  vul_timer t;
  for (unsigned i = 0; i <num;i++)
    c+= vnl_c_vector<double>::two_nrm2(a.begin(), size);
  double time = t.real();
  std::cout <<" Time for finding the two_nrm2 of " << size
           <<"-D vectors " << num << "times  = " << time / 1000.0 << "s.\n";
}

void vnl_vector_test_euclid_dist_sq_timing(unsigned size, unsigned long num)
{
  vnl_vector<double> a(size);
  vnl_vector<double> b(size);

  vnl_vector< std::complex<double> > cmpxa(size);
  vnl_vector< std::complex<double> > cmpxb(size);
  vnl_vector< std::complex<double> > cmpxc =
            vnl_c_vector<std::complex<double> >::euclid_dist_sq(cmpxa.begin(), cmpxb.begin(), size);

  for (unsigned i= 0; i < size; i++)
  {
    a(i) = i / size;
    b(i) = i * i / size;
  }

  double c=0;
  vul_timer t;
  for (unsigned i = 0; i <num;i++)
    c+= vnl_c_vector<double>::euclid_dist_sq(a.begin(), b.begin(), size);
  double time = t.real();
  std::cout <<" Time for finding the euclid_dist_sq of " << size
           <<"-D vectors " << num << "times  = " << time / 1000.0 << "s.\n";
}

void vnl_vector_test_timing()
{
  vnl_vector_test_two_nrm2_timing(20000,20000ul);
  vnl_vector_test_two_nrm2_timing(1000,400000ul);
  vnl_vector_test_two_nrm2_timing(100,4000000ul);
  vnl_vector_test_two_nrm2_timing(4,100000000ul);
  vnl_vector_test_euclid_dist_sq_timing(40000,10000ul);
  vnl_vector_test_euclid_dist_sq_timing(1000,400000ul);
  vnl_vector_test_euclid_dist_sq_timing(100,4000000ul);
  vnl_vector_test_euclid_dist_sq_timing(4,100000000ul);
}
#endif

void vnl_vector_test_leak()           // use top4.1 to watch for memory.
{                                     // remember to kill process.
  while (true) {
    vnl_vector_test_int();
    vnl_vector_test_matrix();
    vnl_vector_test_conversion();
  }
}

#ifndef LEAK
#define LEAK 0
#endif

void test_vector()
{
  vnl_vector_test_int();
  vnl_vector_test_float();
  vnl_vector_test_matrix();
  vnl_vector_test_conversion();
  vnl_vector_test_io();
#if TIMING
  vnl_vector_test_timing();
#endif
#if LEAK
  vnl_vector_test_leak();
#endif
}


TESTMAIN(test_vector);
