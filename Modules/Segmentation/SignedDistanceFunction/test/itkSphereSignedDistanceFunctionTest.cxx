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

#include "itkSphereSignedDistanceFunction.h"

/**
 * This module tests the functionaliyt of the SphereSignedDistanceFunction
 * class and its superclass ShapeSignedDistanceFunction.
 *
 * In particular, it creates a SphereSignedDistanceFunction object
 * cast it to a generic ShapeSignedDistanceFunction object.
 *
 * The shape parameters are set at radius of 5 and center of (0,0).
 * The signed distance is evaluated at several point and compared
 * to expected values. The test fails if the evaluated results is
 * not within a certain tolerance of the expected results.
 *
 */
int itkSphereSignedDistanceFunctionTest( int, char *[])
{
  typedef double CoordRep;
  const unsigned int Dimension = 2;

  typedef itk::ShapeSignedDistanceFunction<CoordRep,Dimension>
                                       FunctionType;
  typedef itk::SphereSignedDistanceFunction<CoordRep,Dimension>
                                       SphereFunctionType;
  typedef FunctionType::PointType      PointType;
  typedef FunctionType::ParametersType ParametersType;

  SphereFunctionType::Pointer sphere = SphereFunctionType::New();

  // cast it to a generic function
  FunctionType::Pointer function = dynamic_cast<FunctionType *>( sphere.GetPointer() );
  sphere = ITK_NULLPTR;

  // we must initialize the function before use
  function->Initialize();

  ParametersType parameters( function->GetNumberOfParameters() );
  parameters.Fill( 0.0 );
  parameters[0] = 5.0;

  function->SetParameters( parameters );

  std::cout << "Parameters: " << function->GetParameters() << std::endl;

  PointType point;
  function->Print( std::cout );
  std::cout << function->FunctionType::GetNameOfClass() << std::endl;

  for ( double p = 0.0; p < 10.0; p += 1.0 )
    {
    point.Fill( p );
    FunctionType::OutputType output = function->Evaluate( point );

    std::cout << "f( " << point << ") = " << output << std::endl;

    // check results
    CoordRep expected = p * std::sqrt( 2.0 ) - parameters[0];
    if( itk::Math::abs( output - expected ) > 1e-9 )
      {
      std::cout << "But expected value is: " << expected << std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
