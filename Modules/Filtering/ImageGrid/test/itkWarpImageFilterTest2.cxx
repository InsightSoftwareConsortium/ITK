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
#include <iostream>
#include "itkWarpImageFilter.h"
#include "itkStreamingImageFilter.h"

#include "itkPipelineMonitorImageFilter.h"
#include "itkMath.h"

using ImageType = itk::Image<float, 3>;
using DisplacementFieldType = itk::Image<itk::Vector<double, 3>, 3>;
using WarpFilterType = itk::WarpImageFilter<ImageType, ImageType, DisplacementFieldType>;

using MonitorFilter = itk::PipelineMonitorImageFilter<ImageType>;
#define AllocateImageFromRegionAndSpacing(ImageType, rval, region, spacing) \
  {                                                                         \
    rval = ImageType::New();                                                \
    rval->SetSpacing(spacing);                                              \
    rval->SetRegions(region);                                               \
    rval->Allocate();                                                       \
  }                                                                         \
  ITK_MACROEND_NOOP_STATEMENT

namespace
{

ImageType::Pointer
MakeCheckerboard()
{
  using IteratorType = itk::ImageRegionIterator<ImageType>;
  ImageType::SizeType    size = { { 16, 16, 16 } };
  ImageType::SpacingType spacing;
  spacing[0] = spacing[1] = spacing[2] = 1.0;
  ImageType::IndexType  index = { { 0, 0, 0 } };
  ImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);
  ImageType::Pointer image;
  AllocateImageFromRegionAndSpacing(ImageType, image, region, spacing);
  image->FillBuffer(0.0);
  for (IteratorType it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd(); ++it)
  {
    ImageType::IndexType ind(it.GetIndex());
    // initially checkboard 4 pixels wide
    int  x = ind[0] / 4;
    int  y = ind[1] / 4;
    int  z = ind[2] / 4;
    bool black(((x & 1) + (y & 1)) & 1);
    if (z & 1)
    {
      black = !black;
    }
    it.Set(black ? 255.0 : 0.0);
  }
  return image;
}

template <long unsigned int TImageIndexSpaceSize>
typename DisplacementFieldType::Pointer
MakeDisplacementField()
{
  using IteratorType = itk::ImageRegionIterator<DisplacementFieldType>;
  const DisplacementFieldType::SizeType size = { { TImageIndexSpaceSize, TImageIndexSpaceSize, TImageIndexSpaceSize } };
  DisplacementFieldType::SpacingType    spacing;
  spacing[0] = spacing[1] = spacing[2] = 16.0 / static_cast<double>(TImageIndexSpaceSize);
  DisplacementFieldType::IndexType  index = { { 0, 0, 0 } };
  DisplacementFieldType::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);
  DisplacementFieldType::Pointer image;
  AllocateImageFromRegionAndSpacing(DisplacementFieldType, image, region, spacing);
  for (IteratorType it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd(); ++it)
  {
    DisplacementFieldType::PixelType pix;
    for (unsigned int i = 0; i < 3; ++i)
    {
      pix[i] = 1.0;
    }
    it.Set(pix);
  }
  return image;
}

} // namespace

int
itkWarpImageFilterTest2(int, char *[])
{
  //  itk::MultiThreaderBase::SetGlobalDefaultNumberOfThreads(1);
  // make test image
  ImageType::Pointer image = MakeCheckerboard();
  // make full-res displacement field
  DisplacementFieldType::Pointer defField1 = MakeDisplacementField<16u>();
  // make half-res displacement field
  DisplacementFieldType::Pointer defField2 = MakeDisplacementField<8u>();

  auto filter = WarpFilterType::New();
  // test with full res
  filter->SetDisplacementField(defField1);
  filter->SetInput(image);
  filter->SetOutputParametersFromImage(image);
  filter->Update();
  ImageType::Pointer result1 = filter->GetOutput(); // save output for later comparison
  result1->DisconnectPipeline();                    // disconnect to create new output
  // test with half res
  filter->SetDisplacementField(defField2);
  filter->SetInput(image);
  filter->SetOutputParametersFromImage(image);
  filter->Modified(); // enforce re-execution just to be sure
  filter->Update();
  ImageType::Pointer                  result2 = filter->GetOutput();
  itk::ImageRegionIterator<ImageType> it1(result1, result1->GetLargestPossibleRegion()),
    it2(result2, result1->GetLargestPossibleRegion());
  for (it1.GoToBegin(), it2.GoToBegin(); !it1.IsAtEnd() && !it2.IsAtEnd(); ++it1, ++it2)
  {
    if (itk::Math::NotAlmostEquals(it1.Value(), it2.Value()))
    {
      std::cout << "Pixels differ " << it1.Value() << " " << it2.Value() << std::endl;
      return EXIT_FAILURE;
    }
  }
  if (it1.IsAtEnd() != it2.IsAtEnd())
  {
    std::cout << "Iterators don't agree on end of image" << std::endl;
    return EXIT_FAILURE;
  }
  //
  // try streaming
  auto monitor1 = MonitorFilter::New();
  monitor1->SetInput(image);

  auto filter2 = WarpFilterType::New();
  filter2->SetDisplacementField(defField2);
  filter2->SetInput(monitor1->GetOutput());
  filter2->SetOutputParametersFromImage(image);


  auto monitor2 = MonitorFilter::New();
  monitor2->SetInput(filter2->GetOutput());

  using StreamerType = itk::StreamingImageFilter<ImageType, ImageType>;
  auto streamer = StreamerType::New();
  streamer->SetInput(monitor2->GetOutput());
  streamer->SetNumberOfStreamDivisions(4);
  streamer->Update();
  itk::ImageRegionIterator<ImageType> streamIt(streamer->GetOutput(), streamer->GetOutput()->GetBufferedRegion());
  for (streamIt.GoToBegin(), it2.GoToBegin(); !streamIt.IsAtEnd() && !it2.IsAtEnd(); ++streamIt, ++it2)
  {
    if (itk::Math::NotAlmostEquals(streamIt.Value(), it2.Value()))
    {
      std::cout << "Pixels differ " << streamIt.Value() << " " << it2.Value() << std::endl;
      return EXIT_FAILURE;
    }
  }
  if (streamIt.IsAtEnd() != it2.IsAtEnd())
  {
    std::cout << "Iterators don't agree on end of image" << std::endl;
    return EXIT_FAILURE;
  }

  // this verifies that the pipeline was executed as expected along
  // with correct region propagation and output information
  if (!monitor2->VerifyAllInputCanStream(4))
  {
    std::cout << "Filter failed to execute as expected!" << std::endl;
    std::cout << monitor2;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
