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
#include "itkRealTimeStamp.h"
#include "itkRealTimeInterval.h"
#include "itkTemporalRegion.h"
#include "itkTemporalDataObject.h"

/**
 * Test the basic functionality of temporal data objects
 */
int itkTemporalDataObjectTest( int argc, char* argv[] )
{

#define CHECK_FOR_VALUE(a,b)                                            \
  {                                                                     \
    if( a != b )                                                        \
      {                                                                 \
      std::cerr << "Error in "#a << " expected " << b << " but got " << a << std::endl; \
      return EXIT_FAILURE;                                              \
      }                                                                 \
  }

  // Create TemporalRegions
  itk::TemporalRegion regionLarge;
  itk::TemporalRegion regionRequested;
  itk::TemporalRegion regionBuffered;

  itk::TemporalDataObject::Pointer tdo = itk::TemporalDataObject::New();

  return EXIT_SUCCESS;

}
