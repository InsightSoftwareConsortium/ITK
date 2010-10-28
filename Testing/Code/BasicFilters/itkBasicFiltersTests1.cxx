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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h"


void RegisterTests()
{
  REGISTER_TEST(itkNarrowBandTest );
  REGISTER_TEST(itkNarrowBandImageFilterBaseTest );
  REGISTER_TEST(itkNaryAddImageFilterTest );
  REGISTER_TEST(itkNaryMaximumImageFilterTest );
  REGISTER_TEST(itkNeighborhoodConnectedImageFilterTest  );
  REGISTER_TEST(itkNeighborhoodOperatorImageFilterTest );
  REGISTER_TEST(itkNoiseImageFilterTest );
  REGISTER_TEST(itkNonThreadedShrinkImageTest );
  REGISTER_TEST(itkNormalizeImageFilterTest  );
  REGISTER_TEST(itkObjectMorphologyImageFilterTest );
  REGISTER_TEST(itkOrImageFilterTest );
  REGISTER_TEST(itkOrientImageFilterTest);
  REGISTER_TEST(itkOrientImageFilterTest2);
  REGISTER_TEST(itkParallelSparseFieldLevelSetImageFilterTest );
  REGISTER_TEST(itkPasteImageFilterTest );
  REGISTER_TEST(itkPathToChainCodePathFilterTest );
  REGISTER_TEST(itkPathToImageFilterTest );
  REGISTER_TEST(itkPromoteDimensionImageTest );
  REGISTER_TEST(itkPermuteAxesImageFilterTest );
  REGISTER_TEST(itkRGBToVectorAdaptImageFilterTest );
  REGISTER_TEST(itkRGBToLuminanceImageFilterAndAdaptorTest );
  REGISTER_TEST(itkRecursiveGaussianImageFiltersTest );
  REGISTER_TEST(itkRecursiveGaussianImageFiltersOnTensorsTest );
  REGISTER_TEST(itkReflectImageFilterTest );
  REGISTER_TEST(itkReflectiveImageRegionIteratorTest );
  REGISTER_TEST(itkRegionOfInterestImageFilterTest );
  REGISTER_TEST(itkRelabelComponentImageFilterTest );
  REGISTER_TEST(itkRemoveBoundaryObjectsTest );
  REGISTER_TEST(itkRemoveBoundaryObjectsTest2 );
  REGISTER_TEST(itkResampleImageTest );
  REGISTER_TEST(itkResampleImageTest2 );
  REGISTER_TEST(itkResamplePhasedArray3DSpecialCoordinatesImageTest );
  REGISTER_TEST(itkRescaleIntensityImageFilterTest );
}
