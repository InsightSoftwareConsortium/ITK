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

#include "itkHilbertPath.h"
#include "itkNumericTraits.h"
#include "itkTestingMacros.h"


template< typename PathType >
int HilbertPathTestHelper( unsigned int maxHilbertPathOder )
{
  int testStatus = EXIT_SUCCESS;

  typedef typename PathType::IndexType IndexType;

  typename PathType::Pointer path = PathType::New();

  for( unsigned int order = 1; order < maxHilbertPathOder; ++order )
    {
    path->SetHilbertOrder( order );
    TEST_SET_GET_VALUE( order, path->GetHilbertOrder() );

    path->Initialize();

    typename PathType::InputType input = 0;
    TRY_EXPECT_EXCEPTION( path->IncrementInput( input ) );

    typename PathType::InputType endOfInput = path->EndOfInput();
    std::cout << "End of input: "
      << typename itk::NumericTraits< typename PathType::InputType >::PrintType( endOfInput )
      << std::endl;

    for( unsigned int d = 0; d < path->NumberOfSteps(); ++d )
      {
      IndexType index = path->Evaluate( d );

      if( d != path->EvaluateInverse( index ) )
        {
        std::cerr << "Test failed!" << std::endl;
        std::cerr << "Incorrect match-up for path index (" << d
          << ") and multi-dimensional index (" << index << ")" << std::endl;
        testStatus = EXIT_FAILURE;
        }
      }

    //path->EvaluateToIndex( 6 )

    for( unsigned int d = 0; d < 10; ++d )
      {
      IndexType index = path->TransformPathIndexToMultiDimensionalIndex( d );

      if( d != path->EvaluateInverse( index ) )
        {
        std::cerr << "Test failed!" << std::endl;
        std::cerr << "Incorrect match-up for path index (" << d
          << ") and multi-dimensional index (" << index << ")" << std::endl;
        testStatus = EXIT_FAILURE;
        }
      }
    }

  return testStatus;
}


int itkHilbertPathTest( int, char*[] )
{
  typedef unsigned int IndexValueType;

  int testStatus = EXIT_SUCCESS;

  // Set a maximumg Hilbert path order
  const unsigned int maxHilbertPathOder = 5;

  // Test dimension = 2
  typedef itk::HilbertPath< IndexValueType, 2 > HilbertPathType2D;
  HilbertPathType2D::Pointer path2D = HilbertPathType2D::New();

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  EXERCISE_BASIC_OBJECT_METHODS( path2D, HilbertPath, Path );

  testStatus = HilbertPathTestHelper< HilbertPathType2D >( maxHilbertPathOder );

  // Test dimension = 3
  typedef itk::HilbertPath< IndexValueType, 3 > HilbertPathType3D;
  HilbertPathType3D::Pointer path3D = HilbertPathType3D::New();

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  EXERCISE_BASIC_OBJECT_METHODS( path3D, HilbertPath, Path );

  testStatus = HilbertPathTestHelper< HilbertPathType3D >( maxHilbertPathOder );

  // Test dimension = 4
  typedef itk::HilbertPath< IndexValueType, 4 > HilbertPathType4D;
  HilbertPathType4D::Pointer path4D = HilbertPathType4D::New();

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  EXERCISE_BASIC_OBJECT_METHODS( path4D, HilbertPath, Path );

  testStatus = HilbertPathTestHelper< HilbertPathType4D >( maxHilbertPathOder );


  std::cerr << "Test finished " << std::endl;

  return testStatus;
}
