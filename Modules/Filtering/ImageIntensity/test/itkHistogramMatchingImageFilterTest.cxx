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

#include <iomanip>
#include "itkHistogramMatchingImageFilter.h"
#include "itkCommand.h"

/**
 * This file tests the functionality of the HistogramMatchingImageFilter.
 * This test uses artificial data, where we multiply different intensity
 * classes by different factors and test whether we can recover the
 * reference image.
 */

static double
refPattern(unsigned long offset)
{
  if (offset < 40)
  {
    return 5.0;
  }
  if (offset < 160)
  {
    return 10.0;
  }
  if (offset < 200)
  {
    return 15.0;
  }
  if (offset < 320)
  {
    return 20.0;
  }
  return 0.0;
}

static double
srcPattern(unsigned long offset)
{
  if (offset < 40)
  {
    return 5.0 * 1.5;
  }
  if (offset < 160)
  {
    return 10.0 * 0.9;
  }
  if (offset < 200)
  {
    return 15.0 * 1.0;
  }
  if (offset < 320)
  {
    return 20.0 * 0.8;
  }
  return 0.0;
}
namespace
{

// The following classe is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject * o) { m_Process = o; }
  void
  ShowProgress()
  {
    std::cout << "Progress " << m_Process->GetProgress() << std::endl;
  }
  itk::ProcessObject::Pointer m_Process;
};
} // namespace

template <typename ImageType>
static bool
CompareImages(itk::ImageRegionIterator<ImageType> & refIter, itk::ImageRegionIterator<ImageType> & outIter)
{
  bool passed = true;
  refIter.GoToBegin();
  outIter.GoToBegin();
  while (!outIter.IsAtEnd())
  {
    typename ImageType::PixelType diff = refIter.Get() - outIter.Get();
    if (itk::Math::abs(diff) > 1)
    {
      passed = false;
      std::cout << "Test failed at: " << outIter.GetIndex() << " ";
      std::cout << "Output value: " << outIter.Get() << " ";
      std::cout << "Ref value: " << refIter.Get() << std::endl;
    }
    ++outIter;
    ++refIter;
  }
  return passed;
}

/**
 * Write the histogram to the console
 * @tparam HistogramType
 * @param refHistogram
 */
template <typename HistogramConstPointerType>
void
PrintHistogramInfo(HistogramConstPointerType refHistogram)
{
  std::cout << std::endl;
  std::cout << "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^" << std::endl;
  std::cout << refHistogram << std::endl;
  std::cout << "--------------------------------------------------------------------" << std::endl;

  // If the reference histogram is provided, then extract summary statistics
  // directly from the histogram.
  const auto &  allReferenceMinsByDimension = refHistogram->GetMins();              // Array of dimensions
  const auto &  allReferenceMinsFirstDimension = allReferenceMinsByDimension.at(0); // Mins for dimension 0
  const auto &  allReferenceMaxsByDimension = refHistogram->GetMaxs();              // Array of dimensions
  const auto &  allReferenceMaxsFirstDimension = allReferenceMaxsByDimension.at(0); // Maxes for dimension 0
  constexpr int colWidth = 8;
  const std::ios_base::fmtflags initial_cout_state{ std::cout.flags() };
  std::cout << std::left << std::setw(colWidth) << "INDEX" << std::left << std::setw(colWidth) << "FREQ" << std::left
            << std::setw(colWidth) << "MIN" << std::left << std::setw(colWidth) << "MAX" << std::left
            << std::setw(colWidth) << "BINSIZE" << std::endl;
  for (auto histit = refHistogram->Begin(); histit != refHistogram->End(); ++histit)
  {
    const auto histidx = histit.GetIndex()[0];
    const auto binmin = static_cast<double>(allReferenceMinsFirstDimension[histidx]);
    const auto binmax = static_cast<double>(allReferenceMaxsFirstDimension[histidx]);

    std::cout << std::left << std::setw(colWidth) << histidx << std::left << std::setw(colWidth)
              << histit.GetFrequency() << std::left << std::setw(colWidth) << binmin << std::left << std::setw(colWidth)
              << binmax << std::left << std::setw(colWidth) << binmax - binmin << std::endl;
  }
  std::cout << "\n\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^" << std::endl;
  std::cout.flags(initial_cout_state);
}

template <typename TScalar>
int
itkHistogramMatchingImageFilterTest()
{
  using PixelType = TScalar;
  constexpr unsigned int ImageDimension = 3;

  using ImageType = itk::Image<PixelType, ImageDimension>;
  using Iterator = itk::ImageRegionIterator<ImageType>;

  typename ImageType::SizeType size;
  size[0] = 30;
  size[1] = 20;
  size[2] = 2;

  typename ImageType::RegionType region;
  region.SetSize(size);

  typename ImageType::Pointer reference = ImageType::New();
  typename ImageType::Pointer source = ImageType::New();

  reference->SetLargestPossibleRegion(region);
  reference->SetBufferedRegion(region);
  reference->Allocate();

  // Change the origin of the reference image.
  typename ImageType::PointType origin;
  origin[0] = 1.0;
  origin[1] = 10.0;
  origin[2] = 100.0;
  reference->SetOrigin(origin);

  source->SetLargestPossibleRegion(region);
  source->SetBufferedRegion(region);
  source->Allocate();

  Iterator refIter(reference, region);
  Iterator srcIter(source, region);

  unsigned long counter = 0;

  while (!refIter.IsAtEnd())
  {
    refIter.Set(static_cast<PixelType>(refPattern(counter)));
    srcIter.Set(static_cast<PixelType>(srcPattern(counter)));

    ++refIter;
    ++srcIter;
    ++counter;
  }

  bool passed = true;
  using FilterType = itk::HistogramMatchingImageFilter<ImageType, ImageType>;
  typename FilterType::HistogramType::ConstPointer refHistogram = nullptr;

  // Test with historical reference image input, and then capture the histogram as cached
  // value for other tests
  {
    typename FilterType::Pointer filterWithReferenceImage = FilterType::New();

    filterWithReferenceImage->SetReferenceImage(reference);
    filterWithReferenceImage->SetSourceImage(source);
    filterWithReferenceImage->SetNumberOfHistogramLevels(50);
    filterWithReferenceImage->SetNumberOfMatchPoints(8);

    ShowProgressObject                                    progressWatch(filterWithReferenceImage);
    itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
    command = itk::SimpleMemberCommand<ShowProgressObject>::New();
    command->SetCallbackFunction(&progressWatch, &ShowProgressObject::ShowProgress);
    filterWithReferenceImage->AddObserver(itk::ProgressEvent(), command);

    {
      // Exercise and test with ThresholdAtMeanIntensityOff
      filterWithReferenceImage->ThresholdAtMeanIntensityOff();
      filterWithReferenceImage->Update();
      filterWithReferenceImage->Print(std::cout);
    }
    {
      // Exercise auxiliary functions
      std::cout << "Exercise auxiliary functions" << std::endl;
      std::cout << filterWithReferenceImage->GetNumberOfHistogramLevels() << std::endl;
      std::cout << filterWithReferenceImage->GetNumberOfMatchPoints() << std::endl;

      std::cout << "Source Histogram: " << filterWithReferenceImage->GetSourceHistogram() << std::endl;
      std::cout << "Reference Histogram: " << filterWithReferenceImage->GetReferenceHistogram() << std::endl;
      std::cout << "Output Histogram: " << filterWithReferenceImage->GetOutputHistogram() << std::endl;

      std::cout << "Threshold At Mean Intensity? ";
      std::cout << filterWithReferenceImage->GetThresholdAtMeanIntensity() << std::endl;
    }
    {
      // Exercise and test with ThresholdAtMeanIntensityOn
      filterWithReferenceImage->ThresholdAtMeanIntensityOn();
      filterWithReferenceImage->Update();
      filterWithReferenceImage->Print(std::cout);

      // Walk the output and compare with reference
      Iterator outIter(filterWithReferenceImage->GetOutput(), region);
      std::cout << "filterWithReferenceImage - Image Test -- START" << std::endl;
      passed &= CompareImages(refIter, outIter);
      std::cout << "filterWithReferenceImage - Image Test -- FINISHED" << std::endl;
    }
    {
      // Get referenceHistogram for other tests
      refHistogram = filterWithReferenceImage->GetReferenceHistogram();
      PrintHistogramInfo(refHistogram);
    }
  }
  std::cout << "===================================================================================" << std::endl;
  {
    // Test SourceHistogram same size (50) as ReferenceHistogram
    typename FilterType::Pointer filterWithSameSizeHistogram = FilterType::New();

    filterWithSameSizeHistogram->SetReferenceHistogram(refHistogram);
    filterWithSameSizeHistogram->GenerateReferenceHistogramFromImageOff();
    filterWithSameSizeHistogram->SetSourceImage(source);
    filterWithSameSizeHistogram->SetNumberOfHistogramLevels(50);
    filterWithSameSizeHistogram->SetNumberOfMatchPoints(8);
    filterWithSameSizeHistogram->ThresholdAtMeanIntensityOn();

    ShowProgressObject                                    progressWatchHistogramReference(filterWithSameSizeHistogram);
    itk::SimpleMemberCommand<ShowProgressObject>::Pointer commandHistogramReference;
    commandHistogramReference = itk::SimpleMemberCommand<ShowProgressObject>::New();
    commandHistogramReference->SetCallbackFunction(&progressWatchHistogramReference, &ShowProgressObject::ShowProgress);
    filterWithSameSizeHistogram->AddObserver(itk::ProgressEvent(), commandHistogramReference);

    filterWithSameSizeHistogram->ThresholdAtMeanIntensityOn();
    filterWithSameSizeHistogram->Update();
    filterWithSameSizeHistogram->Print(std::cout);

    // Walk the output and compare with reference
    Iterator outIter(filterWithSameSizeHistogram->GetOutput(), region);
    std::cout << "filterWithSameSizeHistogram - Image Test -- START" << std::endl;
    passed &= CompareImages(refIter, outIter);
    std::cout << "filterWithSameSizeHistogram - Image Test -- FINISHED" << std::endl;
  }
  // Test SourceHistogram smaller than (31) ReferenceHistogram
  {
    typename FilterType::Pointer filterWithSmallerHistogram = FilterType::New();

    filterWithSmallerHistogram->SetReferenceHistogram(refHistogram);
    filterWithSmallerHistogram->SetGenerateReferenceHistogramFromImage(false);
    filterWithSmallerHistogram->SetSourceImage(source);
    filterWithSmallerHistogram->SetNumberOfHistogramLevels(31);
    filterWithSmallerHistogram->SetNumberOfMatchPoints(8);
    filterWithSmallerHistogram->ThresholdAtMeanIntensityOn();

    ShowProgressObject                                    progressWatchHistogramReference(filterWithSmallerHistogram);
    itk::SimpleMemberCommand<ShowProgressObject>::Pointer commandHistogramReference;
    commandHistogramReference = itk::SimpleMemberCommand<ShowProgressObject>::New();
    commandHistogramReference->SetCallbackFunction(&progressWatchHistogramReference, &ShowProgressObject::ShowProgress);
    filterWithSmallerHistogram->AddObserver(itk::ProgressEvent(), commandHistogramReference);

    filterWithSmallerHistogram->ThresholdAtMeanIntensityOn();
    filterWithSmallerHistogram->Update();
    filterWithSmallerHistogram->Print(std::cout);

    // Walk the output and compare with reference
    Iterator outIter(filterWithSmallerHistogram->GetOutput(), region);
    std::cout << "filterWithSmallerHistogram - Image Test -- START" << std::endl;
    passed &= CompareImages(refIter, outIter);
    std::cout << "filterWithSmallerHistogram - Image Test -- FINISHED" << std::endl;
  }

  // Test SourceHistogram larger than (93) ReferenceHistogram
  {
    typename FilterType::Pointer filterWithLargerHistogram = FilterType::New();

    filterWithLargerHistogram->SetReferenceHistogram(refHistogram);
    filterWithLargerHistogram->SetGenerateReferenceHistogramFromImage(false);
    filterWithLargerHistogram->SetSourceImage(source);
    filterWithLargerHistogram->SetNumberOfHistogramLevels(93);
    filterWithLargerHistogram->SetNumberOfMatchPoints(8);
    filterWithLargerHistogram->ThresholdAtMeanIntensityOn();

    ShowProgressObject                                    progressWatchHistogramReference(filterWithLargerHistogram);
    itk::SimpleMemberCommand<ShowProgressObject>::Pointer commandHistogramReference;
    commandHistogramReference = itk::SimpleMemberCommand<ShowProgressObject>::New();
    commandHistogramReference->SetCallbackFunction(&progressWatchHistogramReference, &ShowProgressObject::ShowProgress);
    filterWithLargerHistogram->AddObserver(itk::ProgressEvent(), commandHistogramReference);

    filterWithLargerHistogram->ThresholdAtMeanIntensityOn();
    filterWithLargerHistogram->Update();
    filterWithLargerHistogram->Print(std::cout);

    // Walk the output and compare with reference
    Iterator outIter(filterWithLargerHistogram->GetOutput(), region);
    std::cout << "filterWithLargerHistogram - Image Test -- START" << std::endl;
    passed &= CompareImages(refIter, outIter);
    std::cout << "filterWithLargerHistogram - Image Test -- FINISHED" << std::endl;
  }

  // Incorrect input setting failures for ReferenceHistogram
  {
    typename FilterType::Pointer mismatchReferenceChoice = FilterType::New();
    try
    {
      mismatchReferenceChoice->SetReferenceHistogram(refHistogram);
      mismatchReferenceChoice->SetGenerateReferenceHistogramFromImage(true);
      mismatchReferenceChoice->SetSourceImage(source);
      mismatchReferenceChoice->SetNumberOfHistogramLevels(10);
      mismatchReferenceChoice->SetNumberOfMatchPoints(2);
      mismatchReferenceChoice->Update();
      passed = false; // We should never get here, and exception should have been thrown
      std::cout
        << "ERROR: Reached code that should have aborted due to thrown exception of missing ReferenceHistogram\n"
        << __FILE__ << ":" << __LINE__ << std::endl;
    }
    catch (itk::ExceptionObject &)
    {
      std::cout << "Test caught known exception for SetReferenceHistogram correctly, NO FAILURE!" << std::endl;
    }
  }
  // Incorrect input setting failures for ReferenceImage
  {
    typename FilterType::Pointer mismatchReferenceChoice = FilterType::New();
    try
    {
      mismatchReferenceChoice->SetReferenceImage(reference);
      mismatchReferenceChoice->SetGenerateReferenceHistogramFromImage(false);
      mismatchReferenceChoice->SetSourceImage(source);
      mismatchReferenceChoice->SetNumberOfHistogramLevels(10);
      mismatchReferenceChoice->SetNumberOfMatchPoints(2);
      mismatchReferenceChoice->Update();
      passed = false; // We should never get here, and exception should have been thrown
      std::cout << "ERROR: Reached code that should have aborted due to thrown exception of missing ReferenceImage\n"
                << __FILE__ << ":" << __LINE__ << std::endl;
    }
    catch (itk::ExceptionObject &)
    {
      std::cout << "Test caught known exception for SetReferenceImage correctly, NO FAILURE!" << std::endl;
    }
  }

  if (!passed)
  {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
int
itkHistogramMatchingImageFilterTest(int, char *[])
{
  if (itkHistogramMatchingImageFilterTest<float>() != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }
  if (itkHistogramMatchingImageFilterTest<long>() != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }
  if (itkHistogramMatchingImageFilterTest<unsigned long>() != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }
  if (itkHistogramMatchingImageFilterTest<int>() != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }
  if (itkHistogramMatchingImageFilterTest<unsigned int>() != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }
  if (itkHistogramMatchingImageFilterTest<short>() != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }
  if (itkHistogramMatchingImageFilterTest<unsigned short>() != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }
  if (itkHistogramMatchingImageFilterTest<char>() != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }
  if (itkHistogramMatchingImageFilterTest<unsigned char>() != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
