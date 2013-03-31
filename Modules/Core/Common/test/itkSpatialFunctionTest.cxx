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

#include <stdio.h>

// Spatial function stuff
#include "itkSphereSpatialFunction.h"

int itkSpatialFunctionTest(int, char* [] )
{
  // Change this parameter (and the positions, below) to work in higher or lower dimensions
  const unsigned int dim = 3;

  //---------Create and initialize a spatial function-----------

  typedef itk::SphereSpatialFunction<dim> TFunctionType;
  typedef TFunctionType::InputType        TFunctionPositionType;

  // Create and initialize a new sphere function

  TFunctionType::Pointer spatialFunc = TFunctionType::New();
  spatialFunc->SetRadius( 5 );

  TFunctionPositionType center;
  center[0]=10;
  center[1]=10;
  center[2]=10;
  spatialFunc->SetCenter(center);

  // Test the Get macros as well
  spatialFunc->GetCenter();
  spatialFunc->GetRadius();

  std::cout << "Sphere spatial function created\n";

  //----------------Test evaluation of funtion------------------

  // We're going to evaluate it at the center of the sphere (10,10,10)
  bool funcVal = spatialFunc->Evaluate(center);
  printf("Sphere function value is %i\n", funcVal);

  // The function should have returned a value of 1, since the center is inside
  // the sphere
  if(funcVal == 1)
    return EXIT_SUCCESS;
  else
    return EXIT_FAILURE;
}
