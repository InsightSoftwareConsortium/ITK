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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkTransformParameters.h"

using namespace itk;

template< typename TValueType >
bool runTestByType()
{
  bool passed = true;

  TransformParameters<TValueType> params;
  params.SetSize(10);
  std::cout << "GetSize: " << params.GetSize() << std::endl;

  /* Test different ctors */

  //Construct by size
  unsigned int dim = 20;
  TransformParameters<TValueType> paramsSize(dim);
  if( paramsSize.GetSize() != dim )
    {
    std::cout << "Constructor with dimension failed. Expected size of "
              << dim << ", but got " << paramsSize.GetSize() << "." << std::endl;
    passed = false;
    }

  //Copy constructor
  {
  TransformParameters<TValueType> paramsCopy( params );
  for( unsigned int i=0; i < params.GetSize(); i++ )
    {
    if( params[i] != paramsCopy[i] )
      {
      std::cout << "Copy constructor failed. " << std::endl;
      passed = false;
      }
    }
  }

  //Constructor from array
  Array<TValueType> array(dim);
  for( unsigned int i=0; i<dim; i++ )
    { array[i]=i*3.19; }
  {
  TransformParameters<TValueType> paramsCopy( array );
  for( unsigned int i=0; i < params.GetSize(); i++ )
    {
    if( array[i] != paramsCopy[i] )
      {
      std::cout << "Constructor from Array failed. " << std::endl;
      passed = false;
      }
    }
  }

  /* Test assignment operators from different types */

  //Assign from Array
  TransformParameters<TValueType> paramsArray;
  paramsArray = array;
  for( unsigned int i=0; i < array.GetSize(); i++ )
    {
    if( paramsArray[i] != array[i] )
      {
      std::cout << "Copy from Array via assignment failed. " << std::endl;
      passed = false;
      }
    }

  //Assign from VnlVector
  vnl_vector<TValueType> vector(dim);
  for( unsigned int i=0; i<dim; i++ )
    { vector[i]=i*0.123; }
  {
  TransformParameters<TValueType> paramsVnl;
  paramsVnl = vector;
  for( unsigned int i=0; i < paramsVnl.GetSize(); i++ )
    {
    if( vector[i] != paramsVnl[i] )
      {
      std::cout << "Assignment from VnlVector failed. " << std::endl;
      passed = false;
      }
    }
  }

  /* Test MoveDataPointer to point to different memory block */
  TValueType  block[10] = {10,9,8,7,6,5,4,3,2,1};
  params.MoveDataPointer( block );
  for( int i=0; i < 10; i++)
    {
    if( params[i] != 10-i )
      {
      std::cout << "Failed reading memory after MoveDataPointer." << std::endl
                << "Expected: " << 10-i << ", got: " << params[i] << std::endl;
      passed = false;
      }
    }
  return passed;
}

int itkTransformParametersTest(int, char *[])
{
  bool passed = true;

  /* Test double type */
  if( runTestByType<double>() == false )
    {
    passed = false;
    }
  /* Test float type */
  if( runTestByType<float>() == false )
    {
    passed = false;
    }

  return passed ? EXIT_SUCCESS : EXIT_FAILURE;
}
