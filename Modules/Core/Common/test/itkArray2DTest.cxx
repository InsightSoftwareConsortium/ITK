/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>

#include "itkArray2D.h"


int itkArray2DTest(int, char* [] )
{
  typedef itk::Array2D< double > ArrayType;

  typedef vnl_matrix<double> VnlMatrixType;

  const unsigned int rows = 3;
  const unsigned int cols = 4;

  ArrayType a( rows, cols);
  VnlMatrixType vm( rows, cols );

  for( unsigned int r=0; r<rows; r++)
    {
    for( unsigned int c=0; c<cols; c++)
      {
      const double value = static_cast<double>( r + c );
      a.SetElement(r,c,value);
      vm(r,c) = value;
      }
    }

  const double tolerance = 1e-6;

  // test copy constructor
  ArrayType b( a );

  for( unsigned int r=0; r<rows; r++)
    {
    for( unsigned int c=0; c<cols; c++)
      {
      double diff = a.GetElement(r,c) - b(r,c);
      diff = (diff > 0.0 ) ? diff : -diff; // take abs value
      if( diff > tolerance )
        {
        std::cerr << "Error in copy constructor " << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  // test construction from vnl_matrix
  ArrayType d( vm );

  for( unsigned int r=0; r<rows; r++)
    {
    for( unsigned int c=0; c<cols; c++)
      {
      double diff = d(r,c) - vm(r,c);
      diff = (diff > 0.0 ) ? diff : -diff; // take abs value
      if(  diff  > tolerance )
        {
        std::cerr << "Error in construction from vnl_matrix" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  // test for assignment from Array2D
  ArrayType e;

  e = a;

  for( unsigned int r=0; r<rows; r++)
    {
    for( unsigned int c=0; c<cols; c++)
      {
      double diff = a(r,c) - e(r,c);
      diff = (diff > 0.0 ) ? diff : -diff; // take abs value
      if( diff  > tolerance )
        {
        std::cerr << "Error in assignment from Array2D constructor " << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  // test for assignment from vnl_matrix
  ArrayType f;

  f = vm;

  for( unsigned int r=0; r<rows; r++)
    {
    for( unsigned int c=0; c<cols; c++)
      {
      double diff = f(r,c) - vm(r,c);
      diff = (diff > 0.0 ) ? diff : -diff; // take abs value
      if( diff > tolerance )
        {
        std::cerr << "Error in assignment from  vn_matrix" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  std::cout << "Test Passed ! " << std::endl;
  return EXIT_SUCCESS;
}
