/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointCastTest.cxx
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
 *  This program illustrates the use of the utility class PointCast<>
 *
 */


#include "itkPoint.h"
#include <iostream>



//-------------------------
//
//   Main code
//
//-------------------------
int main() 
{

  // Dimension & Type
  const     unsigned int    N = 3;

  //  Point Classes
  typedef    itk::Point<  double, N >    DoublePointType;
  typedef    itk::Point<  float , N >    FloatPointType;

  DoublePointType dp;
  dp[0] = 1.0;
  dp[1] = 1.7;
  dp[2] = 1.9;

  FloatPointType fp;
  fp[0] = 0.0;
  fp[1] = 0.0;
  fp[2] = 0.0;


  std::cout << "Initial values dp = ";
  std::cout << dp << std::endl;

  std::cout << "Initial values fp = ";
  std::cout << fp << std::endl;

  
  itk::PointCast( dp, fp ); 

  std::cout << "Final values fp = ";
  std::cout << fp << std::endl;

  
  for(unsigned int i=0; i<N; i++)
    {
    FloatPointType::ValueType val = 
        static_cast< FloatPointType::ValueType >( dp[i] );
    if( val != fp[i] )
      {
        std::cout << "Test failed at component " << i << std::endl;
        return EXIT_FAILURE;
      }
    }


  std::cout << "Test PASSED ! " << std::endl;

  return EXIT_SUCCESS;
}



