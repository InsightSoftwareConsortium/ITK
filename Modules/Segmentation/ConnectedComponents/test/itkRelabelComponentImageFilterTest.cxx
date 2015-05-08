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

#include "itkRelabelComponentImageFilter.h"
#include "itkConnectedComponentImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"
#include "itkChangeInformationImageFilter.h"
#include "itkLabelStatisticsImageFilter.h"


int itkRelabelComponentImageFilterTest(int argc, char* argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage threshold_low threshold_hi" << std::endl;
    return EXIT_FAILURE;
    }

  typedef   unsigned short  InternalPixelType;
  typedef   unsigned long   LabelPixelType;
  typedef   unsigned char   WritePixelType;
  const     unsigned int    Dimension = 2;

  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;
  typedef itk::Image< LabelPixelType, Dimension>     LabelImageType;
  typedef itk::Image<WritePixelType, Dimension>      WriteImageType;

  typedef itk::ImageFileReader< InternalImageType > ReaderType;
  typedef itk::ImageFileWriter<  WriteImageType  >  WriterType;


  typedef itk::ChangeInformationImageFilter<InternalImageType> ChangeFilterType;
  typedef itk::BinaryThresholdImageFilter< InternalImageType, InternalImageType > ThresholdFilterType;
  typedef itk::ConnectedComponentImageFilter< InternalImageType, LabelImageType > ConnectedComponentType;
  typedef itk::RelabelComponentImageFilter< LabelImageType, LabelImageType > RelabelComponentType;
  typedef itk::BinaryThresholdImageFilter<LabelImageType, WriteImageType> FinalThresholdFilterType;
  typedef itk::LabelStatisticsImageFilter< InternalImageType, LabelImageType> StatisticsFilterType;

  typedef itk::NumericTraits<InternalPixelType>::RealType RealType;

  typedef itk::Statistics::Histogram<RealType> HistogramType;

  int NumBins = 13;
  RealType LowerBound = 51.0;
  RealType UpperBound = 252.0;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  ChangeFilterType::Pointer change = ChangeFilterType::New();
  ThresholdFilterType::Pointer threshold = ThresholdFilterType::New();
  ConnectedComponentType::Pointer connected = ConnectedComponentType::New();
  RelabelComponentType::Pointer relabel = RelabelComponentType::New();
  FinalThresholdFilterType::Pointer finalThreshold = FinalThresholdFilterType::New();
  StatisticsFilterType::Pointer statistics = StatisticsFilterType::New();

  FilterWatcher watcher(relabel);
  FilterWatcher statswatcher(statistics);

  reader->SetFileName( argv[1] );

  // try changing the spacing on the output to test the sorting on
  // physical size
  ChangeFilterType::SpacingType changeSpacing;
  changeSpacing[0] = 1;
  changeSpacing[1] = 0.5;
  change->SetInput( reader->GetOutput() );
  change->SetOutputSpacing(changeSpacing);
  change->ChangeSpacingOn();

  // Create a binary input image to label
  InternalPixelType threshold_low, threshold_hi;
  threshold_low = atoi( argv[3]);
  threshold_hi = atoi( argv[4]);

  threshold->SetInput (change->GetOutput());
  threshold->SetInsideValue(itk::NumericTraits<InternalPixelType>::OneValue());
  threshold->SetOutsideValue(itk::NumericTraits<InternalPixelType>::ZeroValue());
  threshold->SetLowerThreshold(threshold_low);
  threshold->SetUpperThreshold(threshold_hi);
  threshold->Update();

  // Label the components in the image and relabel them so that object
  // numbers increase as the size of the objects decrease.
  connected->SetInput (threshold->GetOutput());
  relabel->SetInput( connected->GetOutput() );
  relabel->SetNumberOfObjectsToPrint( 5 );
  std::cout << "Modified time of relabel's output = " << relabel->GetOutput()->GetMTime() << std::endl;
  relabel->Update();
  std::cout << "NumberOfObjects: " << relabel->GetNumberOfObjects() << " OriginalNumberOfObjects: " <<
    relabel->GetOriginalNumberOfObjects() << " MinimumObjectSize: " << relabel->GetMinimumObjectSize() << std::endl;

  // pull out the largest object
  finalThreshold->SetInput( relabel->GetOutput() );
  finalThreshold->SetLowerThreshold( 1 ); // object #1
  finalThreshold->SetUpperThreshold( 1 ); // object #1
  finalThreshold->SetInsideValue(255);
  finalThreshold->SetOutsideValue(itk::NumericTraits<WritePixelType>::ZeroValue());

  try
    {
    writer->SetInput (finalThreshold->GetOutput());
    writer->SetFileName( argv[2] );
    writer->Update();
    std::cout << "Modified time of relabel's output = " << relabel->GetOutput()->GetMTime() << std::endl;
    writer->Update();
    std::cout << "Modified time of relabel's output = " << relabel->GetOutput()->GetMTime() << std::endl;
    relabel->Modified();
    relabel->Update();
    std::cout << "Modified time of relabel's output = " << relabel->GetOutput()->GetMTime() << std::endl;
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }


  try
    {
    statistics->SetInput( change->GetOutput() );
    statistics->SetLabelInput( relabel->GetOutput() );
    statistics->SetHistogramParameters(NumBins, LowerBound, UpperBound);
    statistics->UseHistogramsOn();
    statistics->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught during statistics calculation!"
              << std::endl;
    std::cerr << excep << std::endl;
    }
  try
    {
    HistogramType::Pointer histogram;
    unsigned long printNum = statistics->GetNumberOfLabels();
    if (printNum > 10)
      {
      printNum = 10;
      }
    for (unsigned int ii=0; ii < printNum; ++ii)
      {
      std::cout << "Label " << ii << ": " << (statistics->HasLabel(ii) ? "Exists" : "Does not exist") << std::endl;
      std::cout << "\tCount = " << statistics->GetCount(ii) << std::endl;
      std::cout << "\tMinimum = " << statistics->GetMinimum(ii) << std::endl;
      std::cout << "\tMaximum = " << statistics->GetMaximum(ii) << std::endl;
      std::cout << "\tMean = " << statistics->GetMean(ii) << std::endl;
      std::cout << "\tSigma = " << statistics->GetSigma(ii) << std::endl;
      std::cout << "\tVariance = " << statistics->GetVariance(ii) << std::endl;
      std::cout << "\tSum = " << statistics->GetSum(ii) << std::endl;
      std::cout << "\tMedian = " << statistics->GetMedian(ii) << std::endl;
      std::cout << "\tRegion = " << statistics->GetRegion(ii) << std::endl;
      const StatisticsFilterType::BoundingBoxType bbox =
        statistics->GetBoundingBox(ii);

      std::cout << "\tBounding box = ";
      for ( unsigned int jj = 0; jj <  bbox.size(); jj++)
        {
        std::cout << bbox[jj] << " ";
        }
      std::cout << std::endl;
      if (statistics->HasLabel(ii))
        {
        std::cout << "\tHistogram Frequencies:" << std::endl;
        histogram = statistics->GetHistogram(ii);
        for (int jj=0;jj<=NumBins;jj++)
          {
          std::cout << histogram->GetFrequency(jj) << ", ";
          }
        std::cout <<  std::endl;
        }
      }

    printNum = 2;
    for (unsigned int ii=statistics->GetNumberOfObjects();
           ii < statistics->GetNumberOfObjects()+printNum; ++ii)
      {
      std::cout << "Label " << ii << ": " << (statistics->HasLabel(ii) ? "Exists" : "Does not exist") << std::endl;
      std::cout << "\tCount = " << statistics->GetCount(ii) << std::endl;
      std::cout << "\tMinimum = " << statistics->GetMinimum(ii) << std::endl;
      std::cout << "\tMaximum = " << statistics->GetMaximum(ii) << std::endl;
      std::cout << "\tMean = " << statistics->GetMean(ii) << std::endl;
      std::cout << "\tSigma = " << statistics->GetSigma(ii) << std::endl;
      std::cout << "\tVariance = " << statistics->GetVariance(ii) << std::endl;
      std::cout << "\tSum = " << statistics->GetSum(ii) << std::endl;
      std::cout << "\tMedian = " << statistics->GetMedian(ii) << std::endl;
      if (statistics->HasLabel(ii))
        {
        std::cout << "\tEvery tenth Histogram Frequencies:" << std::endl;
        histogram = statistics->GetHistogram(ii);
        for (int jj=0;jj<=NumBins;jj++)
          {
          std::cout << histogram->GetFrequency(jj) << ", ";
          }
        std::cout <<  std::endl;
        }
      }

    }
  catch (...)
    {
    std::cerr << "Exception caught while printing statistics" << std::endl;
    }

  // Check for the sizes of the 7 first labels which should be sorted by default
  unsigned long ref1 [7] = { 7656, 2009, 1586, 1491, 1454, 921, 906 };
  for ( int i=0; i<6; ++i )
  {
  if ( relabel->GetSizeOfObjectsInPixels()[i] != ref1[i] )
    {
    std::cerr << "Comparing label size to reference value." << std::endl;
    std::cerr << "Got " << relabel->GetSizeOfObjectsInPixels()[i] << ", expected " << ref1[i] << std::endl;
    return EXIT_FAILURE;
    }
  }

  // Disable size sorting
  relabel->SetSortByObjectSize(false);
  relabel->Update();

  // Check for the sizes of the 7 first labels which are no more sorted
  unsigned long ref2 [7] = { 1491, 2, 1, 906, 3, 40, 1 };
  for ( int i=0; i<6; ++i )
  {
  if ( relabel->GetSizeOfObjectsInPixels()[i] != ref2[i] )
    {
    std::cerr << "Comparing label size to reference value." << std::endl;
    std::cerr << "Got " << relabel->GetSizeOfObjectsInPixels()[i] << ", expected " << ref2[i] << std::endl;
    return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
