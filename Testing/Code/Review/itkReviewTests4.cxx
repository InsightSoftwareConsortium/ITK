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
// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests

#include "vnl/vnl_sample.h"
#include "itkTestMain.h"
#include "itkConfigure.h"

void RegisterTests()
{
  vnl_sample_reseed(8775070);
  //
  // These tests currently cause the following linker failure on Borland
  // Fatal: illegal VIRDEF fixup index in module ...
  // Please limit the tests in this file to Borland failing in this
  // manner
  REGISTER_TEST(itkBinaryImageToStatisticsLabelMapFilterTest1);
  REGISTER_TEST(itkLabelImageToStatisticsLabelMapFilterTest1);
  REGISTER_TEST(itkStatisticsOpeningLabelMapFilterTest1);
  REGISTER_TEST(itkStatisticsKeepNObjectsLabelMapFilterTest1);
  REGISTER_TEST(itkStatisticsRelabelLabelMapFilterTest1);
  REGISTER_TEST(itkBinaryStatisticsOpeningImageFilterTest1);
  REGISTER_TEST(itkBinaryStatisticsKeepNObjectsImageFilterTest1);
  REGISTER_TEST(itkLabelStatisticsKeepNObjectsImageFilterTest1);
  REGISTER_TEST(itkLabelStatisticsOpeningImageFilterTest1);
  REGISTER_TEST(itkStatisticsRelabelImageFilterTest1);
  REGISTER_TEST(itkStatisticsUniqueLabelMapFilterTest1);
  REGISTER_TEST(itkShiftLabelObjectTest);
}
