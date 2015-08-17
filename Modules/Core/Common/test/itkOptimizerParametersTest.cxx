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

#include "itkOptimizerParameters.h"
#include "itkIntTypes.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

template< typename TValue >
bool runTestByType()
{
  bool passed = true;

  itk::OptimizerParameters<TValue> params;
  params.SetSize(10);
  params.Fill(1.23);
  std::cout << "GetSize: " << params.GetSize() << std::endl;

  /* Test different ctors */

  //Construct by size
  itk::SizeValueType dim = 20;
  itk::OptimizerParameters<TValue> paramsSize(dim);
  if( paramsSize.GetSize() != dim )
    {
    std::cerr << "Constructor with dimension failed. Expected size of "
              << dim << ", but got " << paramsSize.GetSize() << "." << std::endl;
    passed = false;
    }

  //Copy constructor
  {
  itk::OptimizerParameters<TValue> paramsCopy( params );
  for( itk::SizeValueType i=0; i < params.GetSize(); i++ )
    {
    if( itk::Math::NotExactlyEquals(params[i], paramsCopy[i]) )
      {
      std::cerr << "Copy constructor failed. " << std::endl;
      passed = false;
      }
    }
  }

  //Constructor from array
  itk::Array<TValue> array(dim);
  for( itk::SizeValueType i=0; i<dim; i++ )
    { array[i]=i*3.19; }
  {
  itk::OptimizerParameters<TValue> paramsCopy( array );
  for( itk::SizeValueType i=0; i < params.GetSize(); i++ )
    {
    if( itk::Math::NotExactlyEquals(array[i], paramsCopy[i]) )
      {
      std::cerr << "Constructor from Array failed. " << std::endl;
      passed = false;
      }
    }
  }

  /* Test assignment operators from different types */

  //Assign from Array
  itk::OptimizerParameters<TValue> paramsArray;
  paramsArray = array;
  for( itk::SizeValueType i=0; i < array.GetSize(); i++ )
    {
    if( itk::Math::NotExactlyEquals(paramsArray[i], array[i]) )
      {
      std::cerr << "Copy from Array via assignment failed. " << std::endl;
      passed = false;
      }
    }

  //Assign from VnlVector
  vnl_vector<TValue> vector(dim);
  for( itk::SizeValueType i=0; i<dim; i++ )
    { vector[i]=i*0.123; }
  {
  itk::OptimizerParameters<TValue> paramsVnl;
  paramsVnl = vector;
  for( itk::SizeValueType i=0; i < paramsVnl.GetSize(); i++ )
    {
    if( itk::Math::NotExactlyEquals(vector[i], paramsVnl[i]) )
      {
      std::cerr << "Assignment from VnlVector failed. " << std::endl;
      passed = false;
      }
    }
  }

  /* Test MoveDataPointer to point to different memory block */
  TValue  block[10] = {10,9,8,7,6,5,4,3,2,1};
  params.MoveDataPointer( block );
  for( int i=0; i < 10; i++)
    {
    if( itk::Math::NotExactlyEquals(params[i], 10-i) )
      {
      std::cerr << "Failed reading memory after MoveDataPointer." << std::endl
                << "Expected: " << 10-i << ", got: " << params[i] << std::endl;
      passed = false;
      }
    }

  /* Test SetParametersObject. Should throw exception with default helper. */
  typename itk::LightObject::Pointer dummyObj = itk::LightObject::New();
  TRY_EXPECT_EXCEPTION( params.SetParametersObject( dummyObj.GetPointer() ) );

  /* Test with null helper and expect exception */
  params.SetHelper( ITK_NULLPTR );
  TRY_EXPECT_EXCEPTION( params.MoveDataPointer( block ) );
  TRY_EXPECT_EXCEPTION( params.SetParametersObject( dummyObj.GetPointer() ) );

  /* Test copy operator */
  itk::OptimizerParameters<TValue> params1(4);
  itk::OptimizerParameters<TValue> params2(4);
  params1.Fill(1.23);
  params2 = params1;
  for( itk::SizeValueType i=0; i < params1.GetSize(); i++ )
    {
    if( itk::Math::NotExactlyEquals(params1[i], params2[i]) )
      {
      std::cerr << "Copy operator failed:" << std::endl
              << "params1 " << params1 << std::endl
              << "params2 " << params2 << std::endl;
      passed = false;
      break;
      }
    }

  /* Exercise set helper */
  typedef typename itk::OptimizerParameters<TValue>::OptimizerParametersHelperType HelperType;
  HelperType * helper = new HelperType;
  params1.SetHelper( helper );

  return passed;
}

int itkOptimizerParametersTest(int, char *[])
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
