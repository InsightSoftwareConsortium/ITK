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
#include <iostream>
#include "itkWarpImageFilter.h"
#include "itkStreamingImageFilter.h"

#include "itkPipelineMonitorImageFilter.h"
#include "itkMath.h"

typedef itk::Image<float,3>                    ImageType;
typedef itk::Image<itk::Vector<double,3> , 3 > DisplacementFieldType;
typedef itk::WarpImageFilter<ImageType,
                             ImageType,
                             DisplacementFieldType> WarpFilterType;

typedef itk::PipelineMonitorImageFilter<ImageType> MonitorFilter;
#define AllocateImageFromRegionAndSpacing(ImageType,rval,region,spacing) \
{ \
  rval = ImageType::New(); \
  rval->SetSpacing(spacing); \
  rval->SetRegions(region); \
  rval->Allocate(); \
}

namespace {

ImageType::Pointer
MakeCheckerboard()
{
  typedef itk::ImageRegionIterator<ImageType> IteratorType;
  ImageType::SizeType size = {{16,16,16}};
  ImageType::SpacingType spacing;
  spacing[0] = spacing[1] = spacing[2] = 1.0;
  ImageType::IndexType index = {{0,0,0}};
  ImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);
  ImageType::Pointer image;
  AllocateImageFromRegionAndSpacing(ImageType,image,region,spacing);
  image->FillBuffer(0.0);
  for( IteratorType it(image,image->GetLargestPossibleRegion()); !it.IsAtEnd(); ++it)
    {
    ImageType::IndexType ind(it.GetIndex());
    // initially checkboard 4 pixels wide
    int x = ind[0] / 4;
    int y = ind[1] / 4;
    int z = ind[2] / 4;
    bool black(((x&1) + (y&1)) & 1);
    if(z & 1)
      {
      black = !black;
      }
    it.Set(black ? 255.0 : 0.0);
    }
  return image;
}

template <long unsigned int TImageIndexSpaceSize>
typename DisplacementFieldType::Pointer MakeDisplacementField(void)
{
  typedef itk::ImageRegionIterator<DisplacementFieldType> IteratorType;
  const DisplacementFieldType::SizeType size = {{TImageIndexSpaceSize,TImageIndexSpaceSize,TImageIndexSpaceSize}};
  DisplacementFieldType::SpacingType spacing;
  spacing[0] = spacing[1] = spacing[2] = 16.0/(double)TImageIndexSpaceSize;
  DisplacementFieldType::IndexType index = {{0,0,0}};
  DisplacementFieldType::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);
  DisplacementFieldType::Pointer image;
  AllocateImageFromRegionAndSpacing(DisplacementFieldType,image,region,spacing);
  for( IteratorType it(image,image->GetLargestPossibleRegion()); ! it.IsAtEnd(); ++it)
    {
    DisplacementFieldType::PixelType pix;
    for(unsigned i = 0; i < 3; i++)
      {
      pix[i] = 1.0;
      }
    it.Set(pix);
    }
  return image;
}

}

int
itkWarpImageFilterTest2(int, char * [])
{
  //  itk::MultiThreader::SetGlobalDefaultNumberOfThreads(1);
  // make test image
  ImageType::Pointer image = MakeCheckerboard();
  // make full-res displacement field
  DisplacementFieldType::Pointer defField1 = MakeDisplacementField<16u>();
  // make half-res displacement field
  DisplacementFieldType::Pointer defField2 = MakeDisplacementField<8u>();

  WarpFilterType::Pointer filter = WarpFilterType::New();
  // test with full res
  filter->SetDisplacementField(defField1);
  filter->SetInput(image);
  filter->SetOutputParametersFromImage(image);
  filter->Update();
  ImageType::Pointer result1 = filter->GetOutput();
  // test with half res
  filter->SetDisplacementField(defField2);
  filter->SetInput(image);
  filter->SetOutputParametersFromImage(image);
  filter->Update();
  ImageType::Pointer result2 = filter->GetOutput();
  itk::ImageRegionIterator<ImageType>
    it1(result1,result1->GetLargestPossibleRegion()),
    it2(result2,result1->GetLargestPossibleRegion());
  for(it1.GoToBegin(),it2.GoToBegin();
      !it1.IsAtEnd() && !it2.IsAtEnd();
      ++it1, ++it2)
    {
    if(itk::Math::NotAlmostEquals( it1.Value(), it2.Value() ))
      {
      std::cout << "Pixels differ " << it1.Value() << " "
                << it2.Value()
                << std::endl;
      return EXIT_FAILURE;
      }
    }
  if(it1.IsAtEnd() != it2.IsAtEnd())
    {
    std::cout << "Iterators don't agree on end of image" << std::endl;
    return EXIT_FAILURE;
    }
  //
  // try streaming
  MonitorFilter::Pointer monitor1 = MonitorFilter::New();
  monitor1->SetInput( image );

  WarpFilterType::Pointer filter2 = WarpFilterType::New();
  filter2->SetDisplacementField(defField2);
  filter2->SetInput( monitor1->GetOutput() );
  filter2->SetOutputParametersFromImage(image);


  MonitorFilter::Pointer monitor2 = MonitorFilter::New();
  monitor2->SetInput(filter2->GetOutput());

  typedef itk::StreamingImageFilter<ImageType,ImageType> StreamerType;
  StreamerType::Pointer streamer = StreamerType::New();
  streamer->SetInput(monitor2->GetOutput());
  streamer->SetNumberOfStreamDivisions(4);
  streamer->Update();
  itk::ImageRegionIterator<ImageType> streamIt(streamer->GetOutput(),
                                               streamer->GetOutput()->GetBufferedRegion());
  for(streamIt.GoToBegin(),it2.GoToBegin();
      !streamIt.IsAtEnd() && !it2.IsAtEnd();
      ++streamIt, ++it2)
    {
    if(itk::Math::NotAlmostEquals( streamIt.Value(), it2.Value() ))
      {
      std::cout << "Pixels differ " << streamIt.Value() << " "
                << it2.Value()
                << std::endl;
      return EXIT_FAILURE;
      }

    }
  if(streamIt.IsAtEnd() != it2.IsAtEnd())
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
