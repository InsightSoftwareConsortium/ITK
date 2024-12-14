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

#include "itkRelabelComponentImageFilter.h"
#include "itkConnectedComponentImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkChangeInformationImageFilter.h"
#include "itkLabelStatisticsImageFilter.h"
#include "itkTestingMacros.h"


using SignedPixelType = signed short;

// Explicit template instantiation to test compile-time support of signed types
template class itk::RelabelComponentImageFilter<itk::Image<SignedPixelType>, itk::Image<SignedPixelType>>;


int
itkRelabelComponentImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing Parameters " << '\n';
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImage  outputImage threshold_low threshold_hi" << '\n';
    return EXIT_FAILURE;
  }

  bool success = true;
  using InternalPixelType = unsigned short;
  using LabelPixelType = unsigned long;
  using WritePixelType = unsigned char;
  constexpr unsigned int Dimension = 2;

  using InternalImageType = itk::Image<InternalPixelType, Dimension>;
  using LabelImageType = itk::Image<LabelPixelType, Dimension>;
  using WriteImageType = itk::Image<WritePixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InternalImageType>;
  using WriterType = itk::ImageFileWriter<WriteImageType>;


  using ChangeFilterType = itk::ChangeInformationImageFilter<InternalImageType>;
  using ThresholdFilterType = itk::BinaryThresholdImageFilter<InternalImageType, InternalImageType>;
  using ConnectedComponentType = itk::ConnectedComponentImageFilter<InternalImageType, LabelImageType>;
  using RelabelComponentType = itk::RelabelComponentImageFilter<LabelImageType, LabelImageType>;
  using FinalThresholdFilterType = itk::BinaryThresholdImageFilter<LabelImageType, WriteImageType>;
  using StatisticsFilterType = itk::LabelStatisticsImageFilter<InternalImageType, LabelImageType>;

  using RealType = itk::NumericTraits<InternalPixelType>::RealType;

  using HistogramType = itk::Statistics::Histogram<RealType>;

  const int      NumBins = 13;
  const RealType LowerBound = 51.0;
  const RealType UpperBound = 252.0;

  auto reader = ReaderType::New();
  auto writer = WriterType::New();
  auto change = ChangeFilterType::New();
  auto threshold = ThresholdFilterType::New();
  auto connected = ConnectedComponentType::New();
  auto relabel = RelabelComponentType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(relabel, RelabelComponentImageFilter, InPlaceImageFilter);


  auto finalThreshold = FinalThresholdFilterType::New();
  auto statistics = StatisticsFilterType::New();

  const itk::SimpleFilterWatcher watcher(relabel);
  const itk::SimpleFilterWatcher statswatcher(statistics);

  reader->SetFileName(argv[1]);

  // try changing the spacing on the output to test the sorting on
  // physical size
  ChangeFilterType::SpacingType changeSpacing;
  changeSpacing[0] = 1;
  changeSpacing[1] = 0.5;
  change->SetInput(reader->GetOutput());
  change->SetOutputSpacing(changeSpacing);
  change->ChangeSpacingOn();

  // Create a binary input image to label
  const InternalPixelType threshold_low = std::stoi(argv[3]);
  const InternalPixelType threshold_hi = std::stoi(argv[4]);

  threshold->SetInput(change->GetOutput());
  threshold->SetInsideValue(itk::NumericTraits<InternalPixelType>::OneValue());
  threshold->SetOutsideValue(InternalPixelType{});
  threshold->SetLowerThreshold(threshold_low);
  threshold->SetUpperThreshold(threshold_hi);
  threshold->Update();

  // Label the components in the image and relabel them so that object
  // numbers increase as the size of the objects decrease.
  connected->SetInput(threshold->GetOutput());
  relabel->SetInput(connected->GetOutput());

  const itk::SizeValueType numberOfObjectsToPrint = 5;
  relabel->SetNumberOfObjectsToPrint(numberOfObjectsToPrint);
  ITK_TEST_SET_GET_VALUE(numberOfObjectsToPrint, relabel->GetNumberOfObjectsToPrint());

  const typename RelabelComponentType::ObjectSizeType minimumObjectSize = 0;
  relabel->SetMinimumObjectSize(minimumObjectSize);
  ITK_TEST_SET_GET_VALUE(minimumObjectSize, relabel->GetMinimumObjectSize());

  const bool sortByObjectSize = true;
  ITK_TEST_SET_GET_BOOLEAN(relabel, SortByObjectSize, sortByObjectSize);

  std::cout << "Modified time of relabel's output = " << relabel->GetOutput()->GetMTime() << '\n';
  relabel->Update();
  std::cout << "NumberOfObjects: " << relabel->GetNumberOfObjects()
            << " OriginalNumberOfObjects: " << relabel->GetOriginalNumberOfObjects() << '\n';

  // pull out the largest object
  finalThreshold->SetInput(relabel->GetOutput());
  finalThreshold->SetLowerThreshold(1); // object #1
  finalThreshold->SetUpperThreshold(1); // object #1
  finalThreshold->SetInsideValue(255);
  finalThreshold->SetOutsideValue(WritePixelType{});

  try
  {
    writer->SetInput(finalThreshold->GetOutput());
    writer->SetFileName(argv[2]);
    writer->Update();
    std::cout << "Modified time of relabel's output = " << relabel->GetOutput()->GetMTime() << '\n';
    writer->Update();
    std::cout << "Modified time of relabel's output = " << relabel->GetOutput()->GetMTime() << '\n';
    relabel->Modified();
    relabel->Update();
    std::cout << "Modified time of relabel's output = " << relabel->GetOutput()->GetMTime() << '\n';
  }
  catch (const itk::ExceptionObject & excep)
  {
    std::cerr << "Exception caught !" << '\n';
    std::cerr << excep << '\n';
    success = false;
  }


  try
  {
    statistics->SetInput(change->GetOutput());
    statistics->SetLabelInput(relabel->GetOutput());
    statistics->SetHistogramParameters(NumBins, LowerBound, UpperBound);
    statistics->UseHistogramsOn();
    statistics->Update();
  }
  catch (const itk::ExceptionObject & excep)
  {
    std::cerr << "Exception caught during statistics calculation!" << '\n';
    std::cerr << excep << '\n';
    success = false;
  }
  try
  {
    HistogramType::Pointer histogram;
    unsigned long          printNum = statistics->GetNumberOfLabels();
    if (printNum > 10)
    {
      printNum = 10;
    }
    for (unsigned int ii = 0; ii < printNum; ++ii)
    {
      std::cout << "Label " << ii << ": " << (statistics->HasLabel(ii) ? "Exists" : "Does not exist") << '\n';
      std::cout << "\tCount = " << statistics->GetCount(ii) << '\n';
      std::cout << "\tMinimum = " << statistics->GetMinimum(ii) << '\n';
      std::cout << "\tMaximum = " << statistics->GetMaximum(ii) << '\n';
      std::cout << "\tMean = " << statistics->GetMean(ii) << '\n';
      std::cout << "\tSigma = " << statistics->GetSigma(ii) << '\n';
      std::cout << "\tVariance = " << statistics->GetVariance(ii) << '\n';
      std::cout << "\tSum = " << statistics->GetSum(ii) << '\n';
      std::cout << "\tMedian = " << statistics->GetMedian(ii) << '\n';
      std::cout << "\tRegion = " << statistics->GetRegion(ii) << '\n';
      const StatisticsFilterType::BoundingBoxType bbox = statistics->GetBoundingBox(ii);

      std::cout << "\tBounding box = ";
      for (auto jj : bbox)
      {
        std::cout << jj << ' ';
      }
      std::cout << '\n';
      if (statistics->HasLabel(ii))
      {
        std::cout << "\tHistogram Frequencies:" << '\n';
        histogram = statistics->GetHistogram(ii);
        for (int jj = 0; jj <= NumBins; ++jj)
        {
          std::cout << histogram->GetFrequency(jj) << ", ";
        }
        std::cout << '\n';
      }
    }

    printNum = 2;
    for (unsigned int ii = statistics->GetNumberOfObjects(); ii < statistics->GetNumberOfObjects() + printNum; ++ii)
    {
      std::cout << "Label " << ii << ": " << (statistics->HasLabel(ii) ? "Exists" : "Does not exist") << '\n';
      std::cout << "\tCount = " << statistics->GetCount(ii) << '\n';
      std::cout << "\tMinimum = " << statistics->GetMinimum(ii) << '\n';
      std::cout << "\tMaximum = " << statistics->GetMaximum(ii) << '\n';
      std::cout << "\tMean = " << statistics->GetMean(ii) << '\n';
      std::cout << "\tSigma = " << statistics->GetSigma(ii) << '\n';
      std::cout << "\tVariance = " << statistics->GetVariance(ii) << '\n';
      std::cout << "\tSum = " << statistics->GetSum(ii) << '\n';
      std::cout << "\tMedian = " << statistics->GetMedian(ii) << '\n';
      if (statistics->HasLabel(ii))
      {
        std::cout << "\tEvery tenth Histogram Frequencies:" << '\n';
        histogram = statistics->GetHistogram(ii);
        for (int jj = 0; jj <= NumBins; ++jj)
        {
          std::cout << histogram->GetFrequency(jj) << ", ";
        }
        std::cout << '\n';
      }
    }
  }
  catch (...)
  {
    std::cerr << "Exception caught while printing statistics" << '\n';
    success = false;
  }

  // Check for the sizes of the 7 first labels which should be sorted by default
  const unsigned long ref1[7] = { 7656, 2009, 1586, 1491, 1454, 921, 906 };
  for (int i = 0; i < 6; ++i)
  {
    if (relabel->GetSizeOfObjectsInPixels()[i] != ref1[i])
    {
      std::cerr << "Comparing label size to reference value." << '\n';
      std::cerr << "Got " << relabel->GetSizeOfObjectsInPixels()[i] << ", expected " << ref1[i] << '\n';
      success = false;
    }
  }

  // Disable size sorting
  relabel->SetSortByObjectSize(false);
  relabel->Update();

  // Check for the sizes of the 7 first labels which are no more sorted
  const unsigned long ref2[7] = { 1491, 2, 1, 906, 3, 40, 1 };
  for (int i = 0; i < 7; ++i)
  {
    if (relabel->GetSizeOfObjectsInPixels()[i] != ref2[i])
    {
      std::cerr << "Comparing label size to reference value." << '\n';
      std::cerr << "Got " << relabel->GetSizeOfObjectsInPixels()[i] << ", expected " << ref2[i] << '\n';
      success = false;
    }
  }

  if (success)
  {
    std::cout << "Test PASSED!" << '\n';
    return EXIT_SUCCESS;
  }
  else
  {
    std::cout << "Test FAILED!" << '\n';
    return EXIT_FAILURE;
  }
}
