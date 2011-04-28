#ifndef vnl_test_arithmetic_body_h_
#define vnl_test_arithmetic_body_h_
// See test_arithmetic.cxx for the actual test.

// This is a "generic" test body. It expects the macros NewMat and
// NewVec to be defined. The semantics are:
//
//  NewMat( m, r, c, data ) : define a r x c double matrix m
//    initialized from (or referring to) data.
//  NewVec( v, n, data ) : define an n-dimensional double vector v
//    initialized from (or referring to) data.
//
// data will be an array of size >= r*c for NewMat, >= n for NewVec.

#include <testlib/testlib_test.h>

//void
//test_arithmetic()
//{
  double m1_data[] = { 1, 2, 3, 4, 5, 6 };
  double m2_data[] = { 7, 8, 9, 1, 2, 3 };
  double m3_data[] = { 3, 4, 5, 6, 7, 8 };
  double v1_data[] = { 3, 2 };
  double v2_data[] = { 1, 4 };
  double v3_data[] = { 1, 2, 3 };

  double m1_data_copy[] = { 1, 2, 3, 4, 5, 6 };
  double m2_data_copy[] = { 7, 8, 9, 1, 2, 3 };
  //double m3_data_copy[] = { 3, 4, 5, 6, 7, 8 };
  double v1_data_copy[] = { 3, 2 };
  double v2_data_copy[] = { 1, 4 };
  //double v3_data_copy[] = { 1, 2, 3 };

  // Storage for temporary vectors/matrices, when the actual type is a
  // reference.
  double tmp1[100];

  NewMat( m1, 3, 2, m1_data );
  TEST( "3x2 matrix construction" ,
        m1.rows() == 3 && m1.cols() == 2 &&
        m1(0,0)==1 && m1(0,1)==2 &&
        m1(1,0)==3 && m1(1,1)==4 &&
        m1(2,0)==5 && m1(2,1)==6, true);

  NewMat( m3, 2, 3, m3_data );
  TEST( "2x3 matrix construction" ,
        m3.rows() == 2 && m3.cols() == 3 &&
        m3(0,0)==3 && m3(0,1)==4 && m3(0,2)==5 &&
        m3(1,0)==6 && m3(1,1)==7 && m3(1,2)==8, true);

  NewVec( v1, 2, v1_data );
  TEST( "2 vector construction" ,
        v1.size() == 2 &&
        v1[0] == 3 && v1[1] == 2, true);

  NewVec( v3, 3, v3_data );
  TEST( "3 vector construction" ,
        v3.size() == 3 &&
        v3[0] == 1 && v3[1] == 2 && v3[2] == 3, true);

  // Assume these will work
  NewMat( m2, 3, 2, m2_data );
  NewVec( v2, 2, v2_data );
  NewMat( m1_orig, 3, 2, m1_data_copy );
  NewMat( m2_orig, 3, 2, m2_data_copy );
  //NewMat( m3_orig, 3, 2, m3_data_copy );
  NewVec( v1_orig, 2, v1_data_copy );
  NewVec( v2_orig, 2, v2_data_copy );
  //NewVec( v3_orig, 2, v3_data_copy );

  // Equality
  {
    TEST( "Equality operator/1", v1 == v1_orig, true );
    TEST( "Equality operator/2", v1 == v2_orig, false );
    TEST( "Inequality operator/1", v1 != v1_orig, false );
    TEST( "Inequality operator/2", v1 != v2_orig, true );
  }

  // Assignment should do a data copy, even if the types are
  // references.
  {
    double v_result_data[] = { 9, 2 };
    NewVec( v_result, 2, v_result_data );

    NewVec( tv, 2, tmp1 );
    tv = v1;
    tv(0) = 9;
    TEST( "Assignment on vector", tv == v_result && v1 == v1_orig, true );

    double m_result_data[] = { 1, 2, 3, 8, 5, 6 };
    NewMat( m_result, 3, 2, m_result_data );

    NewMat( tm, 3, 2, tmp1 );
    tm = m1;
    tm(1,1) = 8;
    TEST( "Assignment on matrix", tm == m_result && m1 == m1_orig, true );
  }

  // Addition
  {
    // Vector-vector
    double v_add1_data[] = { 4, 6 };
    NewVec( v_add1, 2, v_add1_data );

    NewVec( tv1, 2, tmp1 );
    tv1 = v1;
    tv1 += v2;
    TEST( "v += v", tv1 == v_add1 && v2 == v2_orig, true );
    TEST( "v + v", v1+v2 == v_add1 && v1 == v1_orig && v2 == v2_orig, true );

    // Vector-scalar
    double v_add2_data[] = { 8, 7 };
    NewVec( v_add2, 2, v_add2_data );

    NewVec( tv2, 2, tmp1 );
    tv2 = v1;
    tv2 += 5;
    TEST( "v += s", tv2 == v_add2, true );
    TEST( "v + s", v1+5.0 == v_add2 && v1 == v1_orig, true );
    TEST( "s + v", 5.0+v1 == v_add2 && v1 == v1_orig, true );

    // Matrix-matrix
    double m_add1_data[] = { 8, 10, 12, 5, 7, 9 };
    NewMat( m_add1, 3, 2, m_add1_data );

    NewMat( mv1, 3, 2, tmp1 );
    mv1 = m1;
    mv1 += m2;
    TEST( "m += m", mv1 == m_add1 && m2 == m2_orig, true );
    TEST( "m + m", m1+m2 == m_add1 && m1 == m1_orig && m2 == m2_orig, true );

    // Matrix-scalar
    double m_add2_data[] = { 4, 5, 6, 7, 8, 9 };
    NewMat( m_add2, 3, 2, m_add2_data );

    NewMat( mv2, 3, 2, tmp1 );
    mv2 = m1;
    mv2 += 3;
    TEST( "m += s", mv2 == m_add2, true );
    TEST( "m + s", m1+3.0 == m_add2 && m1 == m1_orig, true );
    TEST( "s + m", 3.0+m1 == m_add2 && m1 == m1_orig, true );
  }

  // Subtraction
  {
    // Vector-vector
    double v_sub1_data[] = { 2, -2 };
    NewVec( v_sub1, 2, v_sub1_data );

    NewVec( tv1, 2, tmp1 );
    tv1 = v1;
    tv1 -= v2;
    TEST( "v -= v", tv1 == v_sub1 && v2 == v2_orig, true );
    TEST( "v - v", v1-v2 == v_sub1 && v1 == v1_orig && v2 == v2_orig, true );

    // Vector-scalar
    double v_sub2_data[] = { 2, 1 };
    NewVec( v_sub2, 2, v_sub2_data );
    double v_sub3_data[] = { -2, -1 };
    NewVec( v_sub3, 2, v_sub3_data );

    NewVec( tv2, 2, tmp1 );
    tv2 = v1;
    tv2 -= 1;
    TEST( "v -= s", tv2 == v_sub2, true );
    TEST( "v - s", v1-1.0 == v_sub2 && v1 == v1_orig, true );
    TEST( "s - v", 1.0-v1 == v_sub3 && v1 == v1_orig, true );

    // Matrix-matrix
    double m_sub1_data[] = { -6, -6, -6, 3, 3, 3 };
    NewMat( m_sub1, 3, 2, m_sub1_data );

    NewMat( mv1, 3, 2, tmp1 );
    mv1 = m1;
    mv1 -= m2;
    TEST( "m -= m", mv1 == m_sub1 && m2 == m2_orig, true );
    TEST( "m - m", m1-m2 == m_sub1 && m1 == m1_orig && m2 == m2_orig, true );

    // Matrix+scalar
    double m_sub2_data[] = { 0, 1, 2, 3, 4, 5 };
    double m_sub3_data[] = { 0, -1, -2, -3, -4, -5 };
    NewMat( m_sub2, 3, 2, m_sub2_data );
    NewMat( m_sub3, 3, 2, m_sub3_data );

    NewMat( mv2, 3, 2, tmp1 );
    mv2 = m1;
    mv2 -= 1;
    TEST( "m -= s", mv2 == m_sub2, true );
    TEST( "m - s", m1-1.0 == m_sub2 && m1 == m1_orig, true );
    TEST( "s - m", 1.0-m1 == m_sub3 && m1 == m1_orig, true );
  }

  // Multiplication
  {
    // Vector-scalar
    double v_mul1_data[] = { 9, 6 };
    NewVec( v_mul1, 2, v_mul1_data );

    NewVec( tv1, 2, tmp1 );
    tv1 = v1;
    tv1 *= 3;
    TEST( "v *= s", tv1 == v_mul1, true );
    TEST( "v * s", v1*3.0 == v_mul1 && v1 == v1_orig, true );
    TEST( "s * v", 3.0*v1 == v_mul1 && v1 == v1_orig, true );

    // Matrix-scalar
    double m_mul1_data[] = { 2, 4, 6, 8, 10, 12 };
    NewMat( m_mul1, 3, 2, m_mul1_data );

    NewMat( mv1, 3, 2, tmp1 );
    mv1 = m1;
    mv1 *= 2;
    TEST( "m *= s", mv1 == m_mul1, true );
    TEST( "m * s", m1*2.0 == m_mul1 && m1 == m1_orig, true );
    TEST( "s * m", 2.0*m1 == m_mul1 && m1 == m1_orig, true );

    // Matrix-vector
    double v_mul2_data[] = { 7, 17, 27 };
    NewVec( v_mul2, 3, v_mul2_data );

    TEST( "m * v", m1*v1 == v_mul2, true );
  }

  // Division
  {
    // Vector-scalar
    double v_div1_data[] = { 1.5, 1 };
    NewVec( v_div1, 2, v_div1_data );

    NewVec( tv1, 2, tmp1 );
    tv1 = v1;
    tv1 /= 2;
    TEST( "v /= s", tv1 == v_div1, true );
    TEST( "v / s", v1/2.0 == v_div1 && v1 == v1_orig, true );

    // Matrix+scalar
    double m_div1_data[] = { 0.5, 1.0, 1.5, 2.0, 2.5, 3.0 };
    NewMat( m_div1, 3, 2, m_div1_data );

    NewMat( tm1, 3, 2, tmp1 );
    tm1 = m1;
    tm1 /= 2;
    TEST( "m /= s", tm1 == m_div1, true );
    TEST( "m / s", m1/2.0 == m_div1 && m1 == m1_orig, true );
  }

  // Element product
  {
    // Vector
    double v_ep1_data[] = { 3, 8 };
    NewVec( v_ep1, 2, v_ep1_data );

    TEST( "element_product(v,v)", element_product(v1,v2) == v_ep1, true );

    // Matrix
    double m_ep1_data[] = { 7, 16, 27, 4, 10, 18 };
    NewMat( m_ep1, 3, 2, m_ep1_data );

    TEST( "element_product(m,m)", element_product(m1,m2) == m_ep1, true );
  }

  // Element quotient
  {
    // Vector
    double v_eq1_data[] = { 3, 0.5 };
    NewVec( v_eq1, 2, v_eq1_data );

    TEST( "element_quotient(v,v)", element_quotient(v1,v2) == v_eq1, true );

    // Matrix
    double m_eq1_data[] = { 1.0/7.0, 2.0/8.0, 3.0/9.0, 4.0/1.0, 5.0/2.0, 6.0/3.0 };
    NewMat( m_eq1, 3, 2, m_eq1_data );

    TEST( "element_quotient(m,m)", element_quotient(m1,m2) == m_eq1, true );
  }

  // Negation
  {
    // Vector
    double v_neg1_data[] = { -3, -2 };
    NewVec( v_neg1, 2, v_neg1_data );

    TEST( "-v", -v1 == v_neg1 && v1 == v1_orig, true );

    // Matrix
    double m_neg1_data[] = { -1, -2, -3, -4, -5, -6 };
    NewMat( m_neg1, 3, 2, m_neg1_data );

    TEST( "-m", -m1 == m_neg1 && m1 == m1_orig, true );
  }
//}

#endif // vnl_test_arithmetic_body_h_
