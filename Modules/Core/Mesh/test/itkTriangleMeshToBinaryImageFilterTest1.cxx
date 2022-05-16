/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkRegularSphereMeshSource.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkImageFileWriter.h"
#include "itkTriangleMeshToBinaryImageFilter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkTestingMacros.h"

// test double update and usage of RoI filter
int
itkTriangleMeshToBinaryImageFilterTest1(int argc, char * argv[])
{
  // Declare the type of the input and output mesh
  using TriangleMeshTraits = itk::DefaultDynamicMeshTraits<double, 3, 3>;
  using TriangleMeshType = itk::Mesh<double, 3, TriangleMeshTraits>;

  // declare triangle mesh source
  using SphereMeshSourceType = itk::RegularSphereMeshSource<TriangleMeshType>;
  using PointType = SphereMeshSourceType::PointType;
  using VectorType = SphereMeshSourceType::VectorType;

  auto      mySphereMeshSource = SphereMeshSourceType::New();
  PointType center;
  center.Fill(50);
  PointType::ValueType scaleInit[3] = { 10, 10, 10 };
  VectorType           scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(3);
  mySphereMeshSource->SetScale(scale);
  mySphereMeshSource->Update();

  using ImageType = itk::Image<unsigned char, 3>;
  auto                im = ImageType::New();
  ImageType::SizeType imSize;
  imSize.Fill(100);
  im->SetRegions(imSize);
  im->Allocate();

  using TriangleMeshToBinaryImageFilterType = itk::TriangleMeshToBinaryImageFilter<TriangleMeshType, ImageType>;
  auto imageFilter = TriangleMeshToBinaryImageFilterType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(imageFilter, TriangleMeshToBinaryImageFilter, ImageSource);

  imageFilter->SetInput(mySphereMeshSource->GetOutput());
  imageFilter->SetInfoImage(im);
  ITK_TRY_EXPECT_NO_EXCEPTION(imageFilter->Update());

  using ROIImageFilter = itk::RegionOfInterestImageFilter<ImageType, ImageType>;
  auto                       roifilter = ROIImageFilter::New();
  ROIImageFilter::RegionType region;
  region.GetModifiableIndex().Fill(35);
  region.GetModifiableSize().Fill(30);
  roifilter->SetInput(im);
  roifilter->SetRegionOfInterest(region);
  ITK_TRY_EXPECT_NO_EXCEPTION(roifilter->Update());
  ImageType::Pointer roiImage = roifilter->GetOutput();
  imageFilter->SetInfoImage(roiImage);
  ITK_TRY_EXPECT_NO_EXCEPTION(imageFilter->Update());

  if (argc > 1)
  {
    ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(imageFilter->GetOutput(), argv[1]));
  }

  std::cout << "TEST PASSED" << std::endl;
  return EXIT_SUCCESS;
}
