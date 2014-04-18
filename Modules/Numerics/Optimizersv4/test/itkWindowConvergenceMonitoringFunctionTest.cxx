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

#include "itkWindowConvergenceMonitoringFunction.h"

int itkWindowConvergenceMonitoringFunctionTest( int itkNotUsed( argc ), char * [] )
{
  typedef float RealType;

  typedef itk::Function::WindowConvergenceMonitoringFunction<RealType> ConvergenceMonitoringType;
  ConvergenceMonitoringType::Pointer convergenceMonitoring = ConvergenceMonitoringType::New();

  convergenceMonitoring->SetWindowSize( 10 );

  for( RealType x = 0.0; x < 20; x += 1.0 )
    {
    convergenceMonitoring->AddEnergyValue( std::pow( static_cast<RealType>(2.0), -x ) );
    try
      {
      std::cout << "convergence value: " << convergenceMonitoring->GetConvergenceValue() << std::endl;
      }
    catch(...)
      {
      std::cout << "GetConvergenceValue() failed." << std::endl;
      return EXIT_FAILURE;
      }
    }

  convergenceMonitoring->GetWindowSize();
  convergenceMonitoring->Print( std::cout, 3 );

  return EXIT_SUCCESS;
}
