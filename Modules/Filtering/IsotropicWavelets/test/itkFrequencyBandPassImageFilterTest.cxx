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
#include "itkFrequencyBandPassImageFilter.h"
#include "itkImage.h"
#include "itkAddImageFilter.h"
#include "itkTestingComparisonImageFilter.h"
using namespace std;
using namespace itk;

// Visualize for dev/debug purposes. Set in cmake file. Require VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

int
itkFrequencyBandPassImageFilterTest(int, char **)
{
  typedef float                    PixelType;
  typedef itk::Image<PixelType, 3> ImageType3D;
  ImageType3D::SizeType            size = { { 20, 40, 80 } };

  // Create an image
  ImageType3D::Pointer    image = ImageType3D::New();
  ImageType3D::RegionType region;
  region.SetSize(size);

  image->SetRegions(region);
  image->SetLargestPossibleRegion(region);
  image->SetBufferedRegion(region);
  image->Allocate();
  image->FillBuffer(1.0);

  typedef itk::FrequencyBandPassImageFilter<ImageType3D> BandPassFilterType;
  BandPassFilterType::Pointer                            passBandFilter = BandPassFilterType::New();
  passBandFilter->SetInput(image);
  passBandFilter->SetLowFrequencyThreshold(0.0);
  passBandFilter->SetHighFrequencyThreshold(0.5);
  passBandFilter->SetPassBand(true, true);
  passBandFilter->Update();
#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(passBandFilter->GetOutput(), "PassBand - default");
#endif

  /** StopBand */
  BandPassFilterType::Pointer stopBandFilter = BandPassFilterType::New();
  stopBandFilter->SetInput(image);
  stopBandFilter->SetLowFrequencyThreshold(0.0);
  stopBandFilter->SetHighFrequencyThreshold(0.5);
  stopBandFilter->SetStopBand(false, false);
  stopBandFilter->Update();
#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(stopBandFilter->GetOutput(), "StopBand");
#endif

  /******* TESTS ********/
  /** Sum of passBand and stopBand images with these settings should be equal to original image */
  typedef itk::AddImageFilter<ImageType3D, ImageType3D> AddFilterType;
  AddFilterType::Pointer                                addFilter = AddFilterType::New();
  addFilter->SetInput1(passBandFilter->GetOutput());
  addFilter->SetInput2(stopBandFilter->GetOutput());

  typedef itk::Testing::ComparisonImageFilter<ImageType3D, ImageType3D> DifferenceFilterType;
  DifferenceFilterType::Pointer                                         differenceFilter = DifferenceFilterType::New();
  differenceFilter->SetToleranceRadius(0);
  differenceFilter->SetDifferenceThreshold(0);
  differenceFilter->SetValidInput(image);
  differenceFilter->SetTestInput(addFilter->GetOutput());
  differenceFilter->Update();
  unsigned int wrong_pixels = differenceFilter->GetNumberOfPixelsWithDifferences();
  if (wrong_pixels > 0)
  {
    std::cout << "The images are not equal, #wrong_pixels= " << wrong_pixels << std::endl;
    return EXIT_FAILURE;
  }

  /**************************************/
  /******* Playing with radians ********/
  /**************************************/
  passBandFilter->SetLowFrequencyThresholdInRadians(Math::pi_over_4);
  passBandFilter->SetHighFrequencyThresholdInRadians(Math::pi_over_2);
  passBandFilter->SetPassBand(true, true);
  passBandFilter->Update();
#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(passBandFilter->GetOutput(), "PassBand - radians");
#endif
  return EXIT_SUCCESS;
}
