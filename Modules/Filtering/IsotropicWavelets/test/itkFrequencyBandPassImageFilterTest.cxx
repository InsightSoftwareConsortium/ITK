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

  // Create a test image
  ImageType3D::Pointer    image = ImageType3D::New();
  ImageType3D::RegionType region;
  region.SetSize(size);

  image->SetRegions(region);
  image->SetLargestPossibleRegion(region);
  image->SetBufferedRegion(region);
  image->Allocate();
  image->FillBuffer(1.0);

  typedef itk::FrequencyBandPassImageFilter<ImageType3D> BandPassFilterType;
  BandPassFilterType::Pointer                            bandFilter = BandPassFilterType::New();
  bandFilter->SetInput(image);
  bandFilter->SetLowFrequencyThreshold(0.1);
  bandFilter->SetHighFrequencyThreshold(0.4);
  bandFilter->Update();
#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(bandFilter->GetOutput(), "PassBand");
#endif
  /** StopBand */
  bandFilter->SetStopBand(false, false);
  bandFilter->Update();
#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(bandFilter->GetOutput(), "StopBand");
#endif
  bandFilter->SetLowFrequencyThreshold(0.2);
  bandFilter->SetHighFrequencyThreshold(0.5);
  bandFilter->SetPassBand(true, true);
  bandFilter->Update();
#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(bandFilter->GetOutput(), "HighPass");
#endif
  return EXIT_SUCCESS;
}
