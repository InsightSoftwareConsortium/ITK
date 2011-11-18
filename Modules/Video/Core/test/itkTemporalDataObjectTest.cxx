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
#include "itkTemporalDataObject.h"

/**
 * Test the basic functionality of temporal data objects
 */
int itkTemporalDataObjectTest( int, char* [] )
{

#define CHECK_FOR_VALUE(a,b)                                            \
    {                                                                     \
    if( a != b )                                                        \
      {                                                                 \
      std::cerr << "Error in " #a << " expected " << b << " but got "    \
                << a << std::endl;                                      \
      return EXIT_FAILURE;                                              \
      }                                                                 \
    }

#define ITK_CHECK_FOR_VALUE(a,b)                                        \
    {                                                                     \
    if( a != b )                                                        \
      {                                                                 \
      std::cerr << "Error in " #a << std::endl;                          \
      a.Print(std::cerr);                                               \
      std::cerr << " != " << std::endl;                                 \
      b.Print(std::cerr);                                               \
      return EXIT_FAILURE;                                              \
      }                                                                 \
    }

  // TODO HACK FIXME
  // This should be also verify that the temporal region functions handle
  // RealTime in a proper way.

  // Create TemporalRegions
  itk::TemporalRegion              regionLarge;
  itk::TemporalRegion              regionRequested;
  itk::TemporalRegion              regionBuffered;
  itk::TemporalDataObject::Pointer tdo;
  itk::TemporalDataObject::Pointer tdo2;
  itk::TemporalDataObject::Pointer tdo3;
  itk::TemporalDataObject::Pointer tdo4;
  itk::DataObject::Pointer         notTemporal;

  // Instantiate a TemporalDataObject
  tdo = itk::TemporalDataObject::New();
  tdo2 = itk::TemporalDataObject::New();
  tdo3 = itk::TemporalDataObject::New();
  tdo4 = itk::TemporalDataObject::New();

  // Setup regions
  regionLarge.SetFrameStart(0);
  regionLarge.SetFrameDuration(20);
  regionRequested.SetFrameStart(2);
  regionRequested.SetFrameDuration(10);
  regionBuffered.SetFrameStart(1);
  regionBuffered.SetFrameDuration(15);

  tdo->SetLargestPossibleTemporalRegion(regionLarge);
  tdo->SetRequestedTemporalRegion(regionRequested);
  tdo->SetBufferedTemporalRegion(regionBuffered);

  tdo3->SetLargestPossibleTemporalRegion(regionLarge);
  tdo3->SetRequestedTemporalRegion(regionRequested);
  tdo3->SetBufferedTemporalRegion(regionBuffered);

  ITK_CHECK_FOR_VALUE(tdo->GetLargestPossibleTemporalRegion(),regionLarge);
  ITK_CHECK_FOR_VALUE(tdo->GetRequestedTemporalRegion(),regionRequested);
  ITK_CHECK_FOR_VALUE(tdo->GetBufferedTemporalRegion(),regionBuffered);

  CHECK_FOR_VALUE(tdo->GetTemporalUnit(),itk::TemporalDataObject::Frame);
  CHECK_FOR_VALUE(tdo->VerifyRequestedRegion(),true);
  CHECK_FOR_VALUE(tdo->RequestedRegionIsOutsideOfTheBufferedRegion(),false);

  tdo->SetRequestedRegionToLargestPossibleRegion();
  CHECK_FOR_VALUE(tdo->RequestedRegionIsOutsideOfTheBufferedRegion(),true);
  CHECK_FOR_VALUE(tdo->VerifyRequestedRegion(),true);

  tdo2->SetRequestedRegion(tdo);
  ITK_CHECK_FOR_VALUE(tdo2->GetRequestedTemporalRegion(),regionLarge);
  CHECK_FOR_VALUE(tdo2->VerifyRequestedRegion(),false);
  CHECK_FOR_VALUE(tdo2->RequestedRegionIsOutsideOfTheBufferedRegion(),true);

  tdo4->Graft(tdo3);
  ITK_CHECK_FOR_VALUE(tdo4->GetLargestPossibleTemporalRegion(),regionLarge);
  ITK_CHECK_FOR_VALUE(tdo4->GetBufferedTemporalRegion(),regionBuffered);

  tdo->Print(std::cout);
  tdo2->Print(std::cout);
  tdo3->Print(std::cout);
  tdo4->Print(std::cout);

  return EXIT_SUCCESS;

}
