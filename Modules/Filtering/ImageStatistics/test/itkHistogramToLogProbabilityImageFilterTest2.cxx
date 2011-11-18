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

#include "itkImageToHistogramFilter.h"
#include "itkMinimumMaximumImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkJoinImageFilter.h"
#include "itkHistogramToLogProbabilityImageFilter.h"

int itkHistogramToLogProbabilityImageFilterTest2( int argc, char * argv [] )
{

  if( argc < 3 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  " << argv[0] << " inputScalarImageFileName outputImage" << std::endl;
    return EXIT_FAILURE;
    }


  const unsigned int                Dimension = 2;
  typedef unsigned char             PixelComponentType;

  typedef itk::Image< PixelComponentType, Dimension > ScalarImageType;
  typedef itk::ImageFileReader< ScalarImageType >     ReaderType;

  ReaderType::Pointer reader1 = ReaderType::New();
  ReaderType::Pointer reader2 = ReaderType::New();

  reader1->SetFileName( argv[1] );
  reader2->SetFileName( argv[2] );

  try
    {
    reader1->Update();
    reader2->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem encoutered while reading image file : " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::JoinImageFilter< ScalarImageType, ScalarImageType > JoinFilterType;

  JoinFilterType::Pointer joinFilter = JoinFilterType::New();

  typedef JoinFilterType::OutputImageType     ArrayImageType;

  joinFilter->SetInput1( reader1->GetOutput() );
  joinFilter->SetInput2( reader2->GetOutput() );

  typedef itk::Statistics::ImageToHistogramFilter< ArrayImageType >
    HistogramFilterType;

  typedef HistogramFilterType::HistogramMeasurementVectorType  HistogramMeasurementVectorType;

  const unsigned int NumberOfComponents = 2;

  itk::MinimumMaximumImageFilter<ScalarImageType>::Pointer minmaxFilter
    = itk::MinimumMaximumImageFilter<ScalarImageType>::New();

  HistogramMeasurementVectorType imageMin( NumberOfComponents );
  HistogramMeasurementVectorType imageMax( NumberOfComponents );

  minmaxFilter->SetInput( reader1->GetOutput() );
  minmaxFilter->Update();

  imageMin[0] = minmaxFilter->GetMinimum();
  imageMax[0] = minmaxFilter->GetMaximum();

  minmaxFilter->SetInput( reader2->GetOutput() );
  minmaxFilter->Update();

  imageMin[1] = minmaxFilter->GetMinimum();
  imageMax[1] = minmaxFilter->GetMaximum();


  HistogramFilterType::Pointer histogramFilter = HistogramFilterType::New();

  histogramFilter->SetInput( joinFilter->GetOutput() );

  HistogramFilterType::HistogramSizeType numberOfBins( NumberOfComponents );

  numberOfBins[0] = static_cast<unsigned int>( imageMax[0] - imageMin[0] + 1 );
  numberOfBins[1] = static_cast<unsigned int>( imageMax[1] - imageMin[1] + 1 );

  histogramFilter->SetHistogramSize( numberOfBins );

  histogramFilter->SetMarginalScale(1.0);

  histogramFilter->SetHistogramBinMinimum( imageMin );
  histogramFilter->SetHistogramBinMaximum( imageMax );

  histogramFilter->Update();


  typedef HistogramFilterType::HistogramType  HistogramType;
  const HistogramType * histogram = histogramFilter->GetOutput();

  typedef itk::HistogramToLogProbabilityImageFilter< HistogramType > HistogramToImageFilterType;
  HistogramToImageFilterType::Pointer histogramToImageFilter = HistogramToImageFilterType::New();

  histogramToImageFilter->SetInput( histogram );

  typedef HistogramToImageFilterType::OutputImageType OutputImageType;

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[3] );

  writer->SetInput( histogramToImageFilter->GetOutput() );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
