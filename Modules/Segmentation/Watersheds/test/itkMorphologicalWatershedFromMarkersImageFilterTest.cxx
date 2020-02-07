/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkMorphologicalWatershedFromMarkersImageFilter.h"
#include "itkLabelOverlayImageFilter.h"
#include "itkTestingMacros.h"


int
itkMorphologicalWatershedFromMarkersImageFilterTest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Missing parameters" << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImageFile"
              << " markerImageFile"
              << " outputImageFile"
              << " markWatershedLine"
              << " fullyConnected"
              << " [ovelayOutput [alpha]]";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  using FilterType = itk::MorphologicalWatershedFromMarkersImageFilter<ImageType, ImageType>;
  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, MorphologicalWatershedFromMarkersImageFilter, ImageToImageFilter);


  bool markWatershedLine = std::stoi(argv[4]);
  ITK_TEST_SET_GET_BOOLEAN(filter, MarkWatershedLine, markWatershedLine);

  bool fullyConnected = std::stoi(argv[5]);
  ITK_TEST_SET_GET_BOOLEAN(filter, FullyConnected, fullyConnected);


  filter->SetInput(reader->GetOutput());

  // Test the marker and input image size disagreement exception
  //
  // Create a marker image larger than the input image

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  ImageType::RegionType           region = reader->GetOutput()->GetLargestPossibleRegion();
  ImageType::RegionType::SizeType size = region.GetSize();
  for (unsigned int i = 0; i < size.GetSizeDimension(); ++i)
  {
    size[i] = size[i] * 2;
  }

  ImageType::RegionType::IndexType index;
  index.Fill(0);

  region.SetSize(size);
  region.SetIndex(index);

  FilterType::LabelImageType::Pointer largerMarkerImage = FilterType::LabelImageType::New();
  largerMarkerImage->SetBufferedRegion(region);
  largerMarkerImage->SetLargestPossibleRegion(region);
  largerMarkerImage->Allocate();

  filter->SetInput2(largerMarkerImage);

  ITK_TRY_EXPECT_EXCEPTION(filter->Update());


  FilterType::LabelImageType::Pointer markerImage = reader2->GetOutput();
  filter->SetInput2(markerImage);
  ITK_TEST_SET_GET_VALUE(markerImage, filter->GetMarkerImage());

  itk::SimpleFilterWatcher watcher(filter, "MorphologicalWatershedFromMarkersImageFilter");

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Write output image
  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[3]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  if (argc > 6)
  {
    using RGBPixelType = itk::RGBPixel<PixelType>;
    using RGBImageType = itk::Image<RGBPixelType, Dimension>;

    using OverlayType = itk::LabelOverlayImageFilter<ImageType, ImageType, RGBImageType>;
    OverlayType::Pointer overlay = OverlayType::New();
    overlay->SetInput(reader->GetOutput());
    overlay->SetLabelImage(filter->GetOutput());

    using RGBWriterType = itk::ImageFileWriter<RGBImageType>;
    RGBWriterType::Pointer rgbwriter = RGBWriterType::New();
    rgbwriter->SetInput(overlay->GetOutput());
    rgbwriter->SetFileName(argv[6]);

    if (argc > 7)
    {
      overlay->SetOpacity(std::stod(argv[7]));
    }

    ITK_TRY_EXPECT_NO_EXCEPTION(rgbwriter->Update());
  }

  std::cerr << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
