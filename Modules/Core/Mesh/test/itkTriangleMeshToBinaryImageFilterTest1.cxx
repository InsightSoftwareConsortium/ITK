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

#include "itkRegularSphereMeshSource.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkImageFileWriter.h"
#include "itkTriangleMeshToBinaryImageFilter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkTestingMacros.h"

//test double update and usage of RoI filter
int itkTriangleMeshToBinaryImageFilterTest1(int argc, char * argv[])
{
  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3> TriangleMeshTraits;
  typedef itk::Mesh<double, 3, TriangleMeshTraits>    TriangleMeshType;

  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType> SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType                PointType;
  typedef SphereMeshSourceType::VectorType               VectorType;

  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();
  PointType center;
  center.Fill(50);
  PointType::ValueType scaleInit[3] = { 10,10,10 };
  VectorType scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(3);
  mySphereMeshSource->SetScale(scale);
  mySphereMeshSource->Update();

  typedef itk::Image<unsigned char, 3> ImageType;
  ImageType::Pointer im = ImageType::New();
  ImageType::SizeType imSize;
  imSize.Fill(100);
  im->SetRegions(imSize);
  im->Allocate();

  typedef itk::TriangleMeshToBinaryImageFilter<TriangleMeshType, ImageType> TriangleMeshToBinaryImageFilterType;
  TriangleMeshToBinaryImageFilterType::Pointer imageFilter = TriangleMeshToBinaryImageFilterType::New();
  EXERCISE_BASIC_OBJECT_METHODS(imageFilter, TriangleMeshToBinaryImageFilter, ImageSource);

  imageFilter->SetInput(mySphereMeshSource->GetOutput());
  imageFilter->SetInfoImage(im);
  TRY_EXPECT_NO_EXCEPTION(imageFilter->Update());

  typedef itk::RegionOfInterestImageFilter< ImageType, ImageType > ROIImageFilter;
  ROIImageFilter::Pointer roifilter = ROIImageFilter::New();
  ROIImageFilter::RegionType region;
  region.GetModifiableIndex().Fill(35);
  region.GetModifiableSize().Fill(30);
  roifilter->SetInput(im);
  roifilter->SetRegionOfInterest(region);
  TRY_EXPECT_NO_EXCEPTION(roifilter->Update());
  ImageType::Pointer roiImage = roifilter->GetOutput();
  imageFilter->SetInfoImage(roiImage);
  TRY_EXPECT_NO_EXCEPTION(imageFilter->Update());

  if (argc > 1)
    {
    typedef itk::ImageFileWriter<ImageType > WriterType;
    WriterType::Pointer ImageWriter = WriterType::New();
    ImageWriter->SetInput(imageFilter->GetOutput());
    ImageWriter->SetFileName(argv[1]);
    TRY_EXPECT_NO_EXCEPTION(ImageWriter->Update());
    }

  std::cout << "TEST PASSED" << std::endl;
  return EXIT_SUCCESS;
}
