#include <vcl_iostream.h>
#include <vcl_utility.h>

#include <vnl/vnl_test.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

void vnl_vector_test_int() {
  vcl_cout << "***********************" << vcl_endl;
  vcl_cout << "Testing Vector<int>" << vcl_endl;
  vcl_cout << "***********************" << vcl_endl;
  //// test constructors, accessors
  vnl_vector<int> v0;
  TEST ("vnl_vector<int> v0()", (v0.size()==0), true);
  vnl_vector<int> v1(2);
  TEST ("vnl_vector<int> v1(2)", (v1.size()==2), true);
  vnl_vector<int> v2(2,2);
  TEST ("vnl_vector<int> v2(2,2)",
        (v2.get(0)==2 && v2.get(1)==2), true);
//   TEST ("v0.set_compare", (v0.set_compare(int_equal), true), true);
  int vcvalues[] = {1,0};
  vnl_vector<int> vc(2,2,vcvalues);
  TEST ("vnl_vector<int> vc(2,2,int[])",
        (vc(0)==1 && vc(1)==0), true);
  TEST ("v1=2",
        (v1=2, (v1.get(0)==2 && v1.get(1)==2)), true);
  TEST ("v1 == v2", (v1 == v2), true);
  TEST ("v0 = v2", ((v0 = v2), (v0 == v2)), true);
  TEST ("v2.put(1,3)", (v2.put(1,3),v2.get(1)), 3);
  TEST ("v2.get(1)", v2.get(1), 3);
  TEST ("v0 == v2", (v0 == v2), false);
  TEST ("v0 != v2", (v0 != v2), true);
  TEST ("(v0 == v2)", (v0 == v2), false);
  TEST ("v1.fill(3)",
        (v1.fill(3), (v1.get(0)==3 && v1.get(1)==3)), true);
  TEST ("v2.fill(2)",
        (v2.fill(2), (v2.get(0)==2 && v2.get(1)==2)), true);
  int v3values [] = {1,2,3};
  vnl_vector<int> v3(3,3,v3values);
  TEST ("v3(3,3,{1,2,3})",(v3.get(0)==1 && v3.get(1)==2 && v3.get(2)==3),
        true);
  vnl_vector<int> v4(v3);
  TEST ("vnl_vector<int> v4(v3)", (v3==v4), true);
  TEST ("v0=v2", (v0=v2, (v0==v2)), true);

  //// test additions and substractions
  TEST ("v0=v2+3",
        ((v0=v2+3), (v0.get(0)==5 && v0.get(1)==5)), true);
  TEST ("v0=3+v2",
        ((v0=3+v2), (v0.get(0)==5 && v0.get(1)==5)), true);
  TEST ("v0+=(-3)",
        (v0+=(-3), (v0.get(0)==2 && v0.get(1)==2)), true);
  TEST ("v0-=(-3)",
        (v0-=(-3), (v0.get(0)==5 && v0.get(1)==5)), true);
  TEST ("v0=v2-3",
        ((v0=v2-3), (v0.get(0)==-1 && v0.get(1)==-1)), true);
  TEST ("v0=3-v2",
        ((v0=3-v2), (v0.get(0)==1 && v0.get(1)==1)), true);
  TEST ("v0= -v2",
        (v0= -v2, (v0.get(0)==-2 && v0.get(1)==-2)), true);

  vnl_vector<int> v5(2);
  v0 = v2;
  TEST ("v5=v0+v2",
        ((v5=v0+v2), (v5.get(0)==4 && v5.get(1)==4)), true);
  TEST ("v5=v0-v2",
        ((v5=v0-v2), (v5.get(0)==0 && v5.get(1)==0)), true);
  TEST ("v0+=v2",
        ((v0+=v2), (v0.get(0)==4 && v0.get(1)==4)), true);
  TEST ("v0-=v2",
        ((v0-=v2), (v0.get(0)==2 && v0.get(1)==2)), true);

  //// test multiplications and divisions
  TEST ("v4=v3*5",
        ((v4=v3*5), (v4.get(0)==5 && v4.get(1)==10 && v4.get(2)==15)), true);

  TEST ("v4=5*v3",
        ((v4=5*v3), (v4.get(0)==5 && v4.get(1)==10 && v4.get(2)==15)), true);
  TEST ("v3*=5",((v3*=5), (v3== v4)), true);
  TEST ("v4=v3/5",
        ((v4=v3/5), (v4.get(0)==1 && v4.get(1)==2 && v4.get(2)==3)), true);
  TEST ("v3/=5", ((v3/=5), (v3==v4)), true);

  //// additional tests
  int vvalues [] = {0,-2,2,0};
  vnl_vector<int> v(4,4,vvalues);
  v0 = v; v1 = v; v2 = v;
  TEST ("v(i)",
        (v(0)==0 && v(1)==-2 && v(2)==2 && v(3)==0), true);
//  TEST ("v.abs()",
//        ((v1 = v.abs()),
//         (v1(0)==0 && v1(1)==2 && v1(2)==2 && v1(3)==0)), true);
//  TEST ("v.sign()",
//        ((v1 = v.sign()),
//         (v1(0)==0 && v1(1)==-1 && v1(2)==1 && v1(3)==0)), true);
  TEST ("element_product(v,v)",
        ((v1 = element_product(v,v)),
         (v1(0)==0 && v1(1)==4 && v1(2)==4 && v1(3)==0)), true);
  TEST ("element_quotient(v,[2])",
        ((v2 = 2),
         (v1 = element_quotient(v,v2)),
         (v1(0)==0 && v1(1)==-1 && v1(2)==1 && v1(3)==0)), true);
//  TEST ("v.update(v.abs())",
//        ((v1 = v.abs()),
//         (v2.update(v1)),
//         (v2==v1)), true);
  TEST ("v.extract(1,3)",
        ((v1 = v.extract(1,3)),
         (v1.size()==1 && v1(0)==v(3))), true);
  TEST ("v.update([4],3)",
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
    TEST ("dot_product(v1,v2)",
          (dot_product(v1,v2)==0 && dot_product(v1,v3)==0 && dot_product(v2,v3)==0), true);
    TEST ("4d-v=3d-v",
          ((v = v3), v.size()==3 && v==v3), true);
    TEST ("cross_3d(v1,v2)", (v=cross_3d(v1,v2), v == v3), true);
    TEST ("cross_3d(v2,v3)", (v=cross_3d(v2,v3), v == v1), true);
    TEST ("cross_3d(v1,v3)", (v=cross_3d(v1,v3), v == -v2), true);
    vnl_vector<int> vv(2,0);
    v1 = vv; v1[0]=1;
    v2 = vv; v2[1]=1;
    TEST ("cross_2d(v1,v2)", cross_2d(v1,v2)==1, true);
  }

  {
    int vvalues [] = {1, 2, 3};
    vnl_vector<int> v (3, 3, vvalues);
    vnl_matrix<int> m;
    m = outer_product(v, v);
    TEST ("outer_product",
          (m(0,0)==1 && m(0,1)==2 && m(0,2)==3 &&
           m(1,0)==2 && m(1,1)==4 && m(1,2)==6 &&
           m(2,0)==3 && m(2,1)==6 && m(2,2)==9), true);
  }
  {
    int vvalues [] = {1,0,0,0};
    vnl_vector<int> v (4,4,vvalues);
    TEST ("v.squared_magnitude",
          (v.squared_magnitude()==1), true);
    TEST ("v.magnitude",
          (v.magnitude()==1), true);
    // normalize not sensible for ints
    //TEST ("v.normalize", ((v1 = 3 * v), (v1.normalize()), v1 == v), true);
  }
}


bool float_equal(const float& f1, const float& f2){
  return (fabs(f1 - f2) < 1.0e-6);
}

void vnl_vector_test_float() {
  vcl_cout << "***********************" << vcl_endl;
  vcl_cout << "Testing Vector<float>" << vcl_endl;
  vcl_cout << "***********************" << vcl_endl;
  //// test constructors, accessors
  vnl_vector<float> v0;
  TEST ("vnl_vector<float> v0()", (v0.size()==0), true);
  vnl_vector<float> v1(2);
  TEST ("vnl_vector<float> v1(2)", (v1.size()==2), true);
  vnl_vector<float> v2(2,2);
  TEST ("vnl_vector<float> v2(2,2)",
        (v2.get(0)==2 && v2.get(1)==2), true);
//  TEST ("v0.set_compare", (v0.set_compare(float_equal), true), true);

  float vcvalues[2] = {1};
  vnl_vector<float> vc(2,2,vcvalues);
  TEST ("vnl_vector<float> vc(2,2,float[])",
        (vc(0)==1 && vc(1)==0), true);
  TEST ("v1=2",
        (v1=2, (v1.get(0)==2 && v1.get(1)==2)), true);
  TEST ("v1 == v2", (v1 == v2), true);
  TEST ("v0 = v2", ((v0 = v2), (v0 == v2)), true);
  TEST ("v2.put(1,3)", (v2.put(1,3),v2.get(1)), 3);
  TEST ("v2.get(1)", v2.get(1), 3);
  TEST ("v0 == v2", (v0 == v2), false);
  TEST ("v0 != v2", (v0 != v2), true);
  TEST ("(v0 == v2)", (v0 == v2), false);
  TEST ("v1.fill(3)",
        (v1.fill(3), (v1.get(0)==3 && v1.get(1)==3)), true);
  TEST ("v2.fill(2)",
        (v2.fill(2), (v2.get(0)==2 && v2.get(1)==2)), true);
//  vnl_vector<float> v3(3,3,1,2,3); var args do not work with floats
  vnl_vector<float> v3((float)1,(float)2,(float)3);
  TEST ("v3(1,2,3)",(v3.get(0)==1 && v3.get(1)==2 && v3.get(2)==3), true);
  vnl_vector<float> v4(v3);
  TEST ("vnl_vector<float> v4(v3)", (v3==v4), true);
  TEST ("v0=v2", (v0=v2, (v0==v2)), true);
  vcl_cout << &v0 << " == " << v0 << vcl_endl;
  TEST ("<<", 1, 1);

  //// test additions and substractions
  TEST ("v0=v2+3",
        ((v0=v2+3), (v0.get(0)==5 && v0.get(1)==5)), true);
  TEST ("v0=3+v2",
        ((v0=3.0f+v2), (v0.get(0)==5 && v0.get(1)==5)), true);
  TEST ("v0+=(-3)",
        (v0+=(-3), (v0.get(0)==2 && v0.get(1)==2)), true);
  TEST ("v0-=(-3)",
        (v0-=(-3), (v0.get(0)==5 && v0.get(1)==5)), true);
  TEST ("v0=v2-3",
        ((v0=v2-3), (v0.get(0)==-1 && v0.get(1)==-1)), true);
  TEST ("v0=3-v2",
        ((v0=3.0f-v2), (v0.get(0)==1 && v0.get(1)==1)), true);
  TEST ("v0= -v2",
        (v0= -v2, (v0.get(0)==-2 && v0.get(1)==-2)), true);

  vnl_vector<float> v5(2);
  v0 = v2;
  TEST ("v5=v0+v2",
        ((v5=v0+v2), (v5.get(0)==4 && v5.get(1)==4)), true);
  TEST ("v5=v0-v2",
        ((v5=v0-v2), (v5.get(0)==0 && v5.get(1)==0)), true);
  TEST ("v0+=v2",
        ((v0+=v2), (v0.get(0)==4 && v0.get(1)==4)), true);
  TEST ("v0-=v2",
        ((v0-=v2), (v0.get(0)==2 && v0.get(1)==2)), true);

  //// test multiplications and divisions
  TEST ("v4=v3*5",
        ((v4=v3*5), (v4.get(0)==5 && v4.get(1)==10 && v4.get(2)==15)), true);

  TEST ("v4=5*v3",
        ((v4=5.0f*v3), (v4.get(0)==5 && v4.get(1)==10 && v4.get(2)==15)), true);
  TEST ("v3*=5",((v3*=5), (v3== v4)), true);
  TEST ("v4=v3/5",
        ((v4=v3/5), (v4.get(0)==1 && v4.get(1)==2 && v4.get(2)==3)), true);
  TEST ("v3/=5", ((v3/=5), (v3==v4)), true);

  //// additional tests
//  vnl_vector<float> v(4,4,0,-2,2,0); no var args with floats
  float vvalues [] = {0,-2,2,0};
  vnl_vector<float> v(4,4,vvalues);
  v[0] = 0;
  v[1] = -2;
  v[2] = 2;
  v[3] = 0;
  v0 = v; v1 = v; v2 = v;
  TEST ("v(i)",
        (v(0)==0 && v(1)==-2 && v(2)==2 && v(3)==0), true);
//  TEST ("v.abs()",
//        ((v1 = v.abs()),
//         (v1(0)==0 && v1(1)==2 && v1(2)==2 && v1(3)==0)), true);
//  TEST ("v.sign()",
//        ((v1 = v.sign()),
//         (v1(0)==0 && v1(1)==-1 && v1(2)==1 && v1(3)==0)), true);
  TEST ("element_product(v,v)",
        ((v1 = element_product(v,v)),
         (v1(0)==0 && v1(1)==4 && v1(2)==4 && v1(3)==0)), true);
  TEST ("element_quotient(v,[2])",
        ((v2 = 2),
         (v1 = element_quotient(v,v2)),
         (v1(0)==0 && v1(1)==-1 && v1(2)==1 && v1(3)==0)), true);
//  TEST ("v.update(v.abs())",
//        ((v1 = v.abs()),
//         (v2.update(v1)),
//         (v2==v1)), true);
  TEST ("v.extract(1,3)",
        ((v1 = v.extract(1,3)),
         (v1.size()==1 && v1(0)==v(3))), true);
  TEST ("v.update([4],3)",
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
    TEST ("v(i)",
          (v(0)==v[0] && v[0]==1 &&
           v(1)==v[1] && v[1]==0 &&
           v(2)==v[2] && v[2]==0 &&
           v(3)==v[3] && v[3]==0), true);
    vnl_vector<float> v1((float)1,(float)0,(float)0);
    vnl_vector<float> v2((float)0,(float)1,(float)0);
    vnl_vector<float> v3((float)0,(float)0,(float)1);
    TEST ("dot_product(v1,v2)",
          (dot_product(v1,v2)==0 && dot_product(v1,v3)==0 && dot_product(v2,v3)==0), true);
    TEST ("4d-v=3d-v",
          ((v = v3), v.size()==3 && v==v3), true);
    TEST ("cross_3d(v1,v2)", (v=cross_3d(v1,v2), v == v3), true);
    TEST ("cross_3d(v2,v3)", (v=cross_3d(v2,v3), v == v1), true);
    TEST ("cross_3d(v1,v3)", (v=cross_3d(v1,v3), v == -v2), true);
    vnl_vector<float> vv(2,0);
    v1 = vv; v1[0]=1;
    v2 = vv; v2[1]=1;
    TEST ("cross_2d(v1,v2)", cross_2d(v1,v2)==1, true);
  }

  {
    vnl_vector<float> v ((float)1, (float)2, (float)3);
    vnl_matrix<float> m = outer_product(v, v);
    TEST ("outer_product",
          (m(0,0)==1 && m(0,1)==2 && m(0,2)==3 &&
           m(1,0)==2 && m(1,1)==4 && m(1,2)==6 &&
           m(2,0)==3 && m(2,1)==6 && m(2,2)==9), true);
  }
  {
    vnl_vector<float> v ((float)1, (float)2, (float)3);
    TEST("vnl_vector<float> v ((float)1, (float)2, (float)3)", (v.size()==3), true);
    v.set_x(1);
    v.set_y(2);
    v.set_z(3);
    TEST("v.set_x(1) and v[0]", (v[0] == 1), true);
    TEST("v.set_y(2) and v[1]", (v[1] == 2), true);
    TEST("v.set_z(3) and v[2]", (v[2] == 3), true);
    vnl_vector<float> v1((float)1, (float)0, (float)0);
    vnl_vector<float> v2((float)0, (float)1, (float)0);
    vcl_cout << "v1 - v2 = " << v1 - v2 << vcl_endl;
    vcl_cout << "angle(v1,v2) = " << angle(v1,v2) << vcl_endl;
    float ang = angle(v1,v2) * (360/(2*vnl_math::pi));
    vcl_cout << "angle(v1,v2) in degrees = " << ang << vcl_endl;
    vcl_cout << "cross_2d(v1,v2) = " << cross_2d(v1,v2) << vcl_endl;
    vcl_cout << "cross_3d(v1,v2) = " << cross_3d(v1,v2) << vcl_endl;
    TEST("angle", (ang == 90.0f), true);
//    TEST("squared_distance_2d", (squared_distance_2d(v1,v2) == 2), true);
//    TEST("squared_distance_3d", (squared_distance_3d(v1,v2) == 2), true);
  }

  {
    float vvalues [] = {1,0,0,0};
    vnl_vector<float> v (4,4,vvalues);
    v[0] = 1;
    v[1] = 0;
    v[2] = 0;
    v[3] = 0;
    TEST ("v.squared_magnitude",
          (v.squared_magnitude()==1), true);
    TEST ("v.magnitude",
          (v.magnitude()==1), true);
    TEST ("v.normalize",
          ((v1 = 3.0f * v),
           (v1.normalize()), v1 == v), true);
  }
}


void vnl_vector_test_matrix() {
  int mvalues[] = {1,2,3,
                   4,5,6};                      // product with matrices
  vnl_matrix<int> m(2,3,6, mvalues);

  int v2values [] = {1,0};
  int v3values [] = {1,0,0};
  vnl_vector<int> v, v2(2,2,v2values), v3(3,3,v3values);
  TEST ("v.pre_multiply(m)",
        ((v = v3),
         (v.pre_multiply(m)),
         (v.size()==2 && v(0)==1 && v(1)==4)), true);
  TEST ("v.post_multiply(m)",
        ((v = v2),
         (v.post_multiply(m)),
         (v.size()==3 && v(0)==1 && v(1)==2 && v(2)==3)), true);
  TEST ("v*=m",
        ((v = v2),
         (v *= m),
         (v.size()==3 && v(0)==1 && v(1)==2 && v(2)==3)), true);
  TEST ("v2*m",
        ((v = v2 * m),
         (v.size()==3 && v(0)==1 && v(1)==2 && v(2)==3)), true);
  TEST ("m*v3",
        ((v = m * v3),
         (v.size()==2 && v(0)==1 && v(1)==4)), true);
}

void vnl_vector_test_conversion() {
  int i, d;
  bool check;
  {
    // convert from a vnl_vector to a block array:
    int v1values [] = {1,2,3, 4,5,6, 7,8,9, 10,11,12};
    vnl_vector<int> v1 (12, 12, v1values);
    const int* data = v1.data_block();
    {
      check = true;
      for (d = 0; d < 12; d++)
        if (data[d] != d+1)
          check = false;
    }
    TEST ("(const int*) m.data_block", check, true);

    typedef int block [12];
    block& v2 = *((block*) data);
    {
      check = true;
      for (i = 0; i < 12; i++)
        if (v1(i) != v2[i])
          check = false;
    }
    TEST ("matrix(i)==block[i]", check, true);

    // convert from a block array to a vnl_vector:
    block b1;
    for (i = 0; i < 12; i++)
      b1[i] = i;
    data = ((const int*) b1);                   // & in ((const int*) &b1)
    {                                           // is not needed.
      check = true;
      for (d = 0; d < 12; d++)
        if (data[d] != d)
          check = false;
    }
    TEST ("(const int*) block", check, true);
    vnl_vector<int> b2(data, 12);
    {
      check = true;
      for (i = 0; i < 12; i++)
        if (b1[i] != b2(i))
          check = false;
    }
    TEST ("block[i]==matrix(i)", check, true);
  }
//   {
//     // convert from a vnl_vector to a block array:
//     vnl_vector<double> v1 (12, 12,
//                           1.0,2.0,3.0, 4.0,5.0,6.0,
//                           7.0,8.0,9.0, 10.0,11.0,12.0);
//     const double* data = v1.data_block();
//     {
//       check = true;
//       for (d = 0; d < 12; d++)
//      if (data[d] != d+1)
//        check = false;
//     }
//     TEST ("(const double*) m.data_block", check, true);
//
//     typedef double block [12];
//     block& v2 = *((block*) data);
//     {
//       check = true;
//       for (i = 0; i < 12; i++)
//      if (v1(i) != v2[i])
//        check = false;
//     }
//     TEST ("matrix(i)==block[i]", check, true);
//
//     // convert from a block array to a vnl_vector:
//     block b1;
//     for (i = 0; i < 12; i++)
//       b1[i] = i;
//     data = ((const double*) b1);             // & in ((const double*) &b1)
//     {                                                // is not needed
//       check = true;
//       for (d = 0; d < 12; d++)
//      if (data[d] != d)
//        check = false;
//     }
//     TEST ("(const double*) block", check, true);
//     vnl_vector<double> b2(data, 12);
//     {
//       check = true;
//     for (i = 0; i < 12; i++)
//       if (b1[i] != b2(i))
//      check = false;
//     }
//     TEST ("block[i]==matrix(i)", check, true);
//   }
}


void vnl_vector_test_leak()           // use top4.1 to watch for memory.
{                                     // remember to kill process.
  while (true) {
    vnl_vector_test_int();
    vnl_vector_test_matrix();
    vnl_vector_test_conversion();
  }
}

void test_vector()
{
  vnl_vector_test_int();
  vnl_vector_test_float();
  vnl_vector_test_matrix();
  vnl_vector_test_conversion();
#if LEAK
  vnl_vector_test_leak();
#endif
}


TESTMAIN(test_vector);
