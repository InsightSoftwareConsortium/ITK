/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageToHistogramGeneratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#include "itkScalarImageToHistogramGenerator.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"

int itkScalarImageToHistogramGeneratorTest( int , char * [] )
{


  typedef unsigned char                         PixelType;
  const unsigned int                            Dimension = 2;

  typedef itk::Image< PixelType, Dimension >    ImageType;


  ImageType::Pointer image = ImageType::New();

  ImageType::RegionType region;
  ImageType::SizeType   size;
  ImageType::IndexType  start;

  size[0] = 255;
  size[1] = 255;
  
  start[0] = 0;
  start[1] = 0;

  region.SetIndex( start );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate();
  
  
  //  Now fill up the image will all the combinations of  
  //  values from 0-255.
  itk::ImageRegionIteratorWithIndex< ImageType >  it( image, region );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    it.Set( it.GetIndex()[0] );
    ++it;
    }


  typedef itk::Statistics::ScalarImageToHistogramGenerator< 
                                                    ImageType 
                                                          >   HistogramGeneratorType;


  HistogramGeneratorType::Pointer histogramGenerator = HistogramGeneratorType::New();

  histogramGenerator->SetInput(  image  );

  histogramGenerator->SetNumberOfBins( 255 );
  histogramGenerator->SetMarginalScale( 10.0 );
  histogramGenerator->Compute();

  typedef HistogramGeneratorType::HistogramType  HistogramType;

  const HistogramType * histogram = histogramGenerator->GetOutput();

  const unsigned int histogramSize = histogram->Size();

  std::cout << "Histogram size " << histogramSize << std::endl;

  unsigned int channel = 0;  // the only channel

  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    if( histogram->GetFrequency( bin, channel ) != 255 ) 
       {
       std::cerr << "Error in bin= " << bin << " channel = " << channel << std::endl;
       std::cerr << "Frequency was= " <<  histogram->GetFrequency( bin, channel ) << " Instead of the expected " << 255 << std::endl;
       return EXIT_FAILURE;
       }
    }


  return EXIT_SUCCESS;
  
}


