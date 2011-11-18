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
#include "itkImageFileReader.h"
#include "itkSimpleFilterWatcher.h"

int itkImageToHistogramFilterTest2( int argc, char * argv [] )
{

  if( argc < 3 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  " << argv[0] << " inputRGBImageFileName outputHistogramFile.txt" << std::endl;
    return EXIT_FAILURE;
    }


  typedef unsigned char                         PixelComponentType;

  typedef itk::RGBPixel< PixelComponentType >   RGBPixelType;

  const unsigned int                            Dimension = 2;

  typedef itk::Image< RGBPixelType, Dimension > RGBImageType;

  const unsigned int                            MeasurementVectorSize = 3; // RGB

  typedef itk::ImageFileReader< RGBImageType >  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem encoutered while reading image file : " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  typedef itk::Statistics::ImageToHistogramFilter< RGBImageType >   HistogramFilterType;

  HistogramFilterType::Pointer histogramFilter = HistogramFilterType::New();
  itk::SimpleFilterWatcher watcher(histogramFilter, "filter");

  typedef HistogramFilterType::HistogramMeasurementVectorType  HistogramMeasurementVectorType;

  // Setting bin mins and max
  HistogramMeasurementVectorType histogramBinMinimum( MeasurementVectorSize );
  histogramBinMinimum[0] = -0.5;
  histogramBinMinimum[1] = -0.5;
  histogramBinMinimum[2] = -0.5;

  HistogramMeasurementVectorType histogramBinMaximum( MeasurementVectorSize );
  histogramBinMaximum[0] = 255.5;
  histogramBinMaximum[1] = 255.5;
  histogramBinMaximum[2] = 255.5;

  histogramFilter->SetHistogramBinMinimum( histogramBinMinimum );
  histogramFilter->SetHistogramBinMaximum( histogramBinMaximum );


  typedef HistogramFilterType::HistogramSizeType   SizeType;

  SizeType size( MeasurementVectorSize );

  size[0] = 255;        // number of bins for the Red   channel
  size[1] =   1;        // number of bins for the Green channel
  size[2] =   1;        // number of bins for the Blue  channel

  histogramFilter->SetHistogramSize( size );

  histogramFilter->SetMarginalScale( 10.0 );

  histogramFilter->SetInput(  reader->GetOutput()  );

  try
    {
    histogramFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  typedef HistogramFilterType::HistogramType  HistogramType;
  const HistogramType * histogram = histogramFilter->GetOutput();

  std::ofstream outputFile;
  outputFile.open( argv[2] );

  const unsigned int histogramSize = histogram->Size();
  outputFile << "Histogram size " << histogramSize << std::endl;


  unsigned int channel = 0;  // red channel

  outputFile << "Histogram of the red component" << std::endl;

  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    outputFile << "bin = " << bin << " frequency = ";
    outputFile << histogram->GetFrequency( bin, channel ) << std::endl;
    }


  size[0] =   1;  // number of bins for the Red   channel
  size[1] = 255;  // number of bins for the Green channel
  size[2] =   1;  // number of bins for the Blue  channel

  histogramFilter->SetHistogramSize( size );

  try
    {
    histogramFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  channel = 1;  // green channel

  outputFile << "Histogram of the green component" << std::endl;

  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    outputFile << "bin = " << bin << " frequency = ";
    outputFile << histogram->GetFrequency( bin, channel ) << std::endl;
    }


  size[0] =   1;  // number of bins for the Red   channel
  size[1] =   1;  // number of bins for the Green channel
  size[2] = 255;  // number of bins for the Blue  channel

  histogramFilter->SetHistogramSize( size );

  try
    {
    histogramFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  channel = 2;  // blue channel

  outputFile << "Histogram of the blue component" << std::endl;

  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    outputFile << "bin = " << bin << " frequency = ";
    outputFile << histogram->GetFrequency( bin, channel ) << std::endl;
    }

  outputFile.close();

  return EXIT_SUCCESS;
}
