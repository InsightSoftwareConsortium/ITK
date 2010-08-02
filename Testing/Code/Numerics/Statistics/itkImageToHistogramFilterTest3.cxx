/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToHistogramFilterTest3.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


#include "itkImage.h"
#include "itkScalarImageToHistogramGenerator.h"
#include "itkMinimumMaximumImageFilter.h"
#include "itkImageFileReader.h"

int itkImageToHistogramFilterTest3( int argc, char * argv [] )
{

  if( argc < 3 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  " << argv[0] << " inputScalarImageFileName outputHistogramFile.txt" << std::endl;
    return EXIT_FAILURE;
    }


  const unsigned int                                  Dimension = 2;
  typedef unsigned char                               PixelComponentType;
  typedef itk::Image< PixelComponentType, Dimension > ScalarImageType;
  typedef itk::ImageFileReader< ScalarImageType >     ReaderType;

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

  itk::MinimumMaximumImageFilter<ScalarImageType>::Pointer minmaxFilter
    = itk::MinimumMaximumImageFilter<ScalarImageType>::New();
  minmaxFilter->SetInput(reader->GetOutput());
  minmaxFilter->Update();
  const ScalarImageType::PixelType imageMin = minmaxFilter->GetMinimum();
  const ScalarImageType::PixelType imageMax = minmaxFilter->GetMaximum();


  typedef itk::Statistics::ScalarImageToHistogramGenerator<ScalarImageType>
    HistogramGeneratorType;
  HistogramGeneratorType::Pointer histogramGenerator
    = HistogramGeneratorType::New();
  histogramGenerator->SetInput(reader->GetOutput());

  const int NumberOfBins = static_cast<unsigned int>( imageMax - imageMin + 1 );
  histogramGenerator->SetNumberOfBins( NumberOfBins );
  histogramGenerator->SetMarginalScale(1.0);
// BUG revealed.  Solution is located at:
// http://public.kitware.com/Bug/view.php?id=10025
  histogramGenerator->SetHistogramMin( imageMin );
  histogramGenerator->SetHistogramMax( imageMax );
  histogramGenerator->Compute();


  typedef HistogramGeneratorType::HistogramType  HistogramType;
  const HistogramType * histogram = histogramGenerator->GetOutput();

  std::ofstream outputFile;
  outputFile.open( argv[2] );

  const unsigned int histogramSize = histogram->Size();
  outputFile << "Histogram size " << histogramSize << std::endl;

  unsigned int channel = 0;  // red channel
  outputFile << "Histogram of the scalar component" << std::endl;
  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    outputFile << "bin = " << bin << " frequency = ";
    outputFile << histogram->GetFrequency( bin, channel ) << std::endl;
    }
  outputFile.close();
  return EXIT_SUCCESS;
}
