/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkSpatialFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

#include <stdio.h>

// Spatial function stuff
#include "itkSphereSpatialFunction.h"

int main()
{
  // Change this parameter (and the positions, below) to work in higher or lower dimensions
  const unsigned int dim = 3;

  //---------Create and initialize a spatial function-----------

  typedef itk::SphereSpatialFunction<dim> TFunctionType;
  typedef TFunctionType::InputType TFunctionPositionType;

  // Create and initialize a new sphere function

  TFunctionType::Pointer spatialFunc = TFunctionType::New();
  spatialFunc->SetRadius( 5 );

  TFunctionPositionType center;
  center[0]=10;
  center[1]=10;
  center[2]=10;
  spatialFunc->SetCenter(center);

  std::cout << "Sphere spatial function created\n";

  //----------------Test evaluation of funtion------------------

  // We're going to evaluate it at the center of the sphere (0,0,0)
  TFunctionPositionType evalPosition;
  evalPosition[0] = 0;
  evalPosition[1] = 0;
  evalPosition[2] = 0;

  bool funcVal = spatialFunc->Evaluate(evalPosition);
  printf("Sphere function value is %i\n", funcVal);

  // The function should have returned a value of 1, since the center is inside
  // the sphere
  if(funcVal == 1)
    return EXIT_SUCCESS;
  else
    return EXIT_FAILURE;
}

