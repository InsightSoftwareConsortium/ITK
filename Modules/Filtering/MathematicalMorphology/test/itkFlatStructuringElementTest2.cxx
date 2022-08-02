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

#include "itkFlatStructuringElement.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkConstantPadImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkTestingMacros.h"

using namespace itk;

/**
 * Return bool image from input FlatStructuringElement kernel
 */
template <unsigned int VDimension>
typename itk::Image<unsigned char, VDimension>::Pointer
GetImage(const itk::FlatStructuringElement<VDimension> & flatElement)
{
  using ImageType = typename itk::Image<unsigned char, VDimension>;
  using RadiusType = typename FlatStructuringElement<2U>::RadiusType;
  using ConstIterator = typename FlatStructuringElement<2U>::ConstIterator;
  using PixelType = unsigned char;

  auto                           image = ImageType::New();
  typename ImageType::RegionType region;
  RadiusType                     size = flatElement.GetRadius();
  Index<VDimension>              centerIdx;

  for (unsigned int i = 0; i < VDimension; ++i)
  {
    centerIdx[i] = size[i];
    size[i] = 2 * size[i] + 1;
  }
  region.SetSize(size);
  image->SetRegions(region);
  image->Allocate();

  ImageRegionIterator<ImageType> img_it(image, region);
  ConstIterator                  kernel_it;
  for (img_it.GoToBegin(), kernel_it = flatElement.Begin(); !img_it.IsAtEnd(); ++img_it, ++kernel_it)
  {
    if (*kernel_it)
    {
      img_it.Set(255);
    }
    else
    {
      img_it.Set(NumericTraits<PixelType>::ZeroValue());
    }
  }
  return image;
};

/** Test. Compare the result of GetImage() with the original input image. */
int
itkFlatStructuringElementTest2(int argc, char * argv[])
{
  if (argc < 1)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " InputImg OutputImg" << std::endl;
    std::cerr << "Images must be odd in size in all dimensions" << std::endl;
    return EXIT_FAILURE;
  }

  static constexpr unsigned int Dimension = 2;

  // Read test image as unsigned char
  using ImageUCType = itk::Image<unsigned char, Dimension>;
  using ReaderUCType = itk::ImageFileReader<ImageUCType>;
  auto reader = ReaderUCType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->UpdateLargestPossibleRegion());


  ImageUCType::Pointer testImg = reader->GetOutput();
  using FSEType = itk::FlatStructuringElement<Dimension>;


  // Cast to Bool Image. Required by constructor

  using ImageBoolType = itk::Image<bool, Dimension>;

  using RescaleType = itk::RescaleIntensityImageFilter<ImageUCType, ImageUCType>;
  auto rescale = RescaleType::New();
  rescale->SetInput(testImg);
  rescale->SetOutputMinimum(itk::NumericTraits<bool>::ZeroValue());
  rescale->SetOutputMaximum(itk::NumericTraits<bool>::OneValue());

  using castFilterType = itk::CastImageFilter<ImageUCType, ImageBoolType>;
  auto cast = castFilterType::New();
  cast->SetInput(rescale->GetOutput());
  cast->Update();
  ImageBoolType::Pointer testImgBool = cast->GetOutput();

  FSEType              flatStructure = FSEType::FromImage(testImgBool);
  ImageUCType::Pointer imgFromStructure = GetImage(flatStructure);

  // Write result from GetImage for comparison with input image

  using WriterType = itk::ImageFileWriter<ImageUCType>;
  auto writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(imgFromStructure);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // Even input image sizes generate exception

  // Pad test image to even size
  using ConstPadFilterType = itk::ConstantPadImageFilter<ImageBoolType, ImageBoolType>;
  auto padFilter = ConstPadFilterType::New();
  padFilter->SetInput(testImgBool);
  ImageBoolType::SizeType lowerExtendRegion;
  lowerExtendRegion[0] = 1;
  lowerExtendRegion[1] = 1;
  padFilter->SetPadLowerBound(lowerExtendRegion);
  ImageBoolType::PixelType constPixel = true;
  padFilter->SetConstant(constPixel);

  ITK_TRY_EXPECT_NO_EXCEPTION(padFilter->Update());


  ImageBoolType::Pointer evenBoolImg = padFilter->GetOutput();

  ITK_TRY_EXPECT_EXCEPTION(FSEType::FromImage(evenBoolImg));


  return EXIT_SUCCESS;
}
