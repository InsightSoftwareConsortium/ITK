/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCovariantVectorGeometryTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
/**
 *  
 *  This program illustrates the use of Geometric objects
 *
 */

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkCovariantVector.h"
#include <vnl/vnl_vector_ref.h>
#include <iostream>

  // Dimension & Type
  const     unsigned int    N = 3;
  typedef   double          ValueType;

  //  Vector type
  typedef    itk::CovariantVector< ValueType, N >    VectorType;



//-------------------------
//
//   Main code
//
//-------------------------
int itkCovariantVectorGeometryTest(int, char* [] ) 
{

  VectorType va;
  va[0] = 1.0;
  va[1] = 2.0;
  va[2] = 7.0;

  std::cout << "va = { 1.0, 2.0, 7.0 } = ";
  std::cout << va << std::endl;

  VectorType vb;
  
  vb[0] = 1.0;
  vb[1] = 3.0;
  vb[2] = 5.0;

  std::cout << "vb = (1,3,5)   = ";
  std::cout << vb << std::endl;

  VectorType   vc  =  vb - va;
  std::cout << "vc  =  vb - va  = ";
  std::cout << vc << std::endl;

  VectorType   vd  =  va * 5.0;
  std::cout << "vd  =  va * 5.0 = ";
  std::cout << vd << std::endl;

  VectorType   ve  =  vd / 5.0;
  std::cout << "ve  =  vd * 5.0 = ";
  std::cout << ve << std::endl;

  vd += va;
  std::cout << "vd  +=  va      = ";
  std::cout << vd << std::endl;

  ve -= vb;
  std::cout << "ve  -=  vb      = ";
  std::cout << ve << std::endl;

  VectorType   vh  =  vb;
  std::cout << "vh   =  vb      = ";
  std::cout << vh << std::endl;


  VectorType   vg( va );
  std::cout << "vg( va )        = ";
  std::cout << vg << std::endl;


  ValueType norm2 = vg.GetSquaredNorm();
  std::cout << "vg squared norm = ";
  std::cout << norm2 << std::endl;

  ValueType norm  = vg.GetNorm();
  std::cout << "vg norm = ";
  std::cout << norm << std::endl;


  // Test for vnl interface

  // Test the no const version that returns an vnl_vector_ref
  vnl_vector_ref< ValueType > vnlVector = va.Get_vnl_vector();
  {
    std::cout << "vnl_vector_ref = va ";
    for( unsigned int i=0; i<N; i++ )
    {
      std::cout << vnlVector[i] << ", ";
    }
    std::cout << std::endl;

    std::cout << "vnl_vector_ref.begin() = va.Begin()";
    std::cout << std::endl;
    std::cout << vnlVector.begin() << " = ";
    std::cout << va.Begin() << std::endl;
  }

  // Test the const version that returns an vnl_vector
  const VectorType vf(va);
  vnl_vector<ValueType> vnlVector2 = vf.Get_vnl_vector();
  {
    std::cout << "vnl_vector = va ";
    for( unsigned int i=0; i<N; i++ )
    {
      std::cout << vnlVector2[i] << ", ";
    }
    std::cout << std::endl;

    std::cout << "vnl_vector.begin() != vf.Begin()";
    std::cout << std::endl;
    std::cout << vnlVector2.begin() << " = ";
    std::cout << vf.Begin() << std::endl;
  }


  // Test for CastFrom() method
  {
  std::cout << "Test for CastFrom() method... ";

  const float tolerance = 1e-7;

  // Dimension & Type
  const     unsigned int    N = 3;

  //  CovariantVector Classes
  typedef    itk::CovariantVector<  double, N >    DoubleCovariantVectorType;
  typedef    itk::CovariantVector<  float , N >    FloatCovariantVectorType;

  DoubleCovariantVectorType dp;
  dp[0] = 1.0;
  dp[1] = 1.7;
  dp[2] = 1.9;

  FloatCovariantVectorType fp;
  fp[0] = 0.0;
  fp[1] = 0.0;
  fp[2] = 0.0;

  
  fp.CastFrom( dp ); 

  std::cout << std::endl;
  for(unsigned int i=0; i<N; i++)
    {
    FloatCovariantVectorType::ValueType val = 
        static_cast< FloatCovariantVectorType::ValueType >( dp[i] );

//   std::cout << val   << std::endl;
//   std::cout << fp[i] << std::endl;

    const float diff = vnl_math_abs( val - fp[i] ); 
    std::cout << "difference = " << diff << std::endl;
    if( vnl_math_abs ( val - fp[i] ) > tolerance )
      {
        std::cout << "Test failed at component " << i << std::endl;
        return EXIT_FAILURE;
      }
    }


  std::cout << " PASSED ! " << std::endl;

  }

  return EXIT_SUCCESS;
}



