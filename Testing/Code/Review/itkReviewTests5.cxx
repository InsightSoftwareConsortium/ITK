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
  REGISTER_TEST(itkLabelGeometryImageFilterTest);
  REGISTER_TEST(itkMRCImageIOTest);
  REGISTER_TEST(itkVTKImageIO2Test);
  REGISTER_TEST(itkJPEG2000ImageIOFactoryTest01);
  REGISTER_TEST(itkJPEG2000ImageIOTest00);
  REGISTER_TEST(itkJPEG2000ImageIOTest01);
  REGISTER_TEST(itkJPEG2000ImageIOTest02);
  REGISTER_TEST(itkJPEG2000ImageIOTest03);
  REGISTER_TEST(itkJPEG2000ImageIOTest04);
  REGISTER_TEST(itkJPEG2000ImageIOTest05);
  REGISTER_TEST(itkJPEG2000ImageIOTest06);
  REGISTER_TEST(itkJPEG2000ImageIORegionOfInterest);
}
