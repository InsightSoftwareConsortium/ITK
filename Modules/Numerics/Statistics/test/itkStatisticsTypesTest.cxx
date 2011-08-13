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

#include "itkMeasurementVectorTraits.h"

#define declareType( _x ) \
  typedef itk::Statistics::MeasurementVectorTraits::_x _x; \
  std::cout << #_x << " = " << sizeof( _x ) << " bytes "; \
  if( itk::NumericTraits< _x >::is_integer ) \
    { \
    std::cout << " Integer type " << std::endl; \
    } \
  else \
    {  \
    std::cout << " Real type " << std::endl; \
    }


int itkStatisticsTypesTest(int, char * [])
{

  declareType( InstanceIdentifier );
  declareType( AbsoluteFrequencyType );
  declareType( RelativeFrequencyType );
  declareType( TotalAbsoluteFrequencyType );
  declareType( TotalRelativeFrequencyType );

  return EXIT_SUCCESS;
}
