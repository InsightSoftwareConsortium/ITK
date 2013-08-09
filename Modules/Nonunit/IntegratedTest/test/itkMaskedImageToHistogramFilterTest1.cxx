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

#include "itkMaskedImageToHistogramFilter.h"
#include "itkHistogramToLogProbabilityImageFilter.h"
#include "itkImage.h"
#include "itkRGBPixel.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkComposeImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSimpleFilterWatcher.h"

int itkMaskedImageToHistogramFilterTest1( int argc, char * argv [] )
{

  if( argc < 6 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  " << argv[0] << " inputImageFileName inputImageFileName maskImage maskValue outputHistogramFile" << std::endl;
    return EXIT_FAILURE;
    }


  typedef unsigned char                                   PixelComponentType;
  const unsigned int                                      Dimension = 3;
  typedef itk::Vector< PixelComponentType, 2 >            VectorPixelType;

  typedef itk::Image< unsigned char, Dimension >   ImageType;
  typedef itk::Image< VectorPixelType, Dimension > VectorImageType;

  typedef itk::ImageFileReader< ImageType >  ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  ReaderType::Pointer reader3 = ReaderType::New();
  reader3->SetFileName( argv[3] );

  typedef itk::ComposeImageFilter< ImageType, VectorImageType > ComposeType;
  ComposeType::Pointer compose = ComposeType::New();
  compose->SetInput1(reader->GetOutput());
  compose->SetInput2(reader2->GetOutput());

  typedef itk::Statistics::MaskedImageToHistogramFilter< VectorImageType, ImageType >   HistogramFilterType;
  HistogramFilterType::Pointer histogramFilter = HistogramFilterType::New();
  histogramFilter->SetInput( compose->GetOutput() );
  histogramFilter->SetMaskImage( reader3->GetOutput() );
  histogramFilter->SetMaskValue( atoi(argv[4]) );
  itk::SimpleFilterWatcher watcher(histogramFilter, "filter");

  typedef HistogramFilterType::HistogramType       HistogramType;
  typedef HistogramFilterType::HistogramSizeType   SizeType;

//   // Setting bin mins and max
//   typedef HistogramFilterType::HistogramMeasurementVectorType  HistogramMeasurementVectorType;
//   HistogramMeasurementVectorType histogramBinMinimum( 2 );
//   histogramBinMinimum[0] = 0;
//   histogramBinMinimum[1] = 0;
//   HistogramMeasurementVectorType histogramBinMaximum( 2 );
//   histogramBinMaximum[0] = 256;
//   histogramBinMaximum[1] = 256;
//   histogramFilter->SetHistogramBinMinimum( histogramBinMinimum );
//   histogramFilter->SetHistogramBinMaximum( histogramBinMaximum );
//   histogramFilter->SetAutoMinimumMaximum( false );

//   SizeType size( 2 );
//   size.Fill(256);
//   histogramFilter->SetHistogramSize( size );

  // TODO: this Update() shouldn't be needed - remove it.
  try
    {
    histogramFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // use a 3D image to check the behavior of HistogramToImageFilter when the image
  // is of greater dimension than the histogram
  typedef itk::Image< float, 3 > FloatImageType;
  typedef itk::HistogramToLogProbabilityImageFilter< HistogramType, FloatImageType >   ImageFilterType;
  ImageFilterType::Pointer imageFilter = ImageFilterType::New();
  imageFilter->SetInput( histogramFilter->GetOutput() );

  typedef itk::RescaleIntensityImageFilter< FloatImageType, ImageType > RescaleType;
  RescaleType::Pointer rescale = RescaleType::New();
  rescale->SetInput( imageFilter->GetOutput() );

  typedef itk::ImageFileWriter< ImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( rescale->GetOutput() );
  writer->SetFileName( argv[5] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // print the image produced by HistogramToLogProbabilityImageFilter for visual inspection
  imageFilter->GetOutput()->Print(std::cout);

  return EXIT_SUCCESS;
}
