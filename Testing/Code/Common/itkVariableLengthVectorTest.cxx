/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVariableLengthVectorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include "itkVariableLengthVector.h"

int itkVariableLengthVectorTest(int, char*[])
{
  typedef itk::VariableLengthVector<float>   FloatVariableLengthVectorType;
  typedef itk::VariableLengthVector<double>  DoubleVariableLengthVectorType;
  
  FloatVariableLengthVectorType f( 3 );
  f[0]=1.0; f[1] = 2.0; f[2] = 3.0;
  DoubleVariableLengthVectorType g( 3 );
  g[0]=4.0; g[1] = 5.0; g[2] = 6.0;
  FloatVariableLengthVectorType h;
  h = g + f;
  g = h++;
  h-=1.1;
  h*=2.0;
  h/=2.0;
  h += g;
  h -= g;
  h = g - h;
  h = -h;
  
  std::cout << h << std::endl;  // should be [-1.1 -1.1 -1.1]

  h = ( FloatVariableLengthVectorType )g ;
  if( h!= static_cast< FloatVariableLengthVectorType >( g ) ) 
    { 
    std::cerr << "Casts: [FAILED]" << std::endl;
    }

  {
  double *d = new double[3];
  d[0] = 0.1; d[1] = 0.2; d[2] = 0.3; 
    {
    DoubleVariableLengthVectorType x( d, 3, false );
    }
    {
    DoubleVariableLengthVectorType x( d, 3, false );
    if( (d[0] != 0.1) || (x[0] != 0.1) )
      {
      std::cerr << "Memory management: [FAILED]" << std::endl;
      }    
    std::cout << x << std::endl;
    x.SetSize( 5 , false); 
    x[3] = 3.0;
    x[4] = 4.0;
    std::cout << x << std::endl;
    if( (d[0] != 0.1) || (x[0] != 0.1) ) // increase length but preserve existing data
      {
      std::cerr << "Memory management: [FAILED]" << std::endl;
      }    
    x.SetSize( 2 , false); // reduce length but preserve existing data 
    std::cout << x << std::endl;
    if( (d[0] != 0.1) || (x[0] != 0.1) )
      {
      std::cerr << "Memory management: [FAILED]" << std::endl;
      }    
     x.SetSize( 5 , true); 
     }
  delete []d;
  } 

  std::cout << "[PASSED]" << std::endl;

  return EXIT_SUCCESS;

}
