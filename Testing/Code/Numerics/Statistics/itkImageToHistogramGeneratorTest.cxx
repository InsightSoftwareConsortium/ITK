/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToHistogramGeneratorTest.cxx
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


#include "itkImageToHistogramGenerator.h"
#include "itkImage.h"
#include "itkRGBPixel.h"
#include "itkImageRegionIteratorWithIndex.h"

int itkImageToHistogramGeneratorTest( int , char * [] )
{


  typedef unsigned char                         PixelComponentType;

  typedef itk::RGBPixel< PixelComponentType >   RGBPixelType;

  const unsigned int                            Dimension = 3;

  typedef itk::Image< RGBPixelType, Dimension > RGBImageType;


  RGBImageType::Pointer image = RGBImageType::New();

  RGBImageType::RegionType region;
  RGBImageType::SizeType   size;
  RGBImageType::IndexType  start;

  size[0] = 127;
  size[1] = 127;
  size[2] = 127;
  
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  region.SetIndex( start );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate();
  
  

  //  Now fill up the image will all the combinations of RGB 
  //  values from 0-255 on each channel.
  itk::ImageRegionIteratorWithIndex< RGBImageType > it( image, region );
  it.GoToBegin();

  RGBPixelType pixel;
  RGBImageType::IndexType index;

  while( !it.IsAtEnd() )
    {
    index = it.GetIndex();
    pixel.SetRed(   index[0] );
    pixel.SetGreen( index[1] );
    pixel.SetBlue(  index[2] );
    it.Set( pixel );
    ++it;
    }


  typedef itk::Statistics::ImageToHistogramGenerator< 
                                              RGBImageType 
                                                          >   HistogramGeneratorType;
  typedef HistogramGeneratorType::SizeType   HistogramSizeType;

  HistogramSizeType hsize;

  hsize[0] = 127;  // number of bins for the Red   channel
  hsize[1] =   1;  // number of bins for the Green channel
  hsize[2] =   1;  // number of bins for the Blue  channel


  HistogramGeneratorType::Pointer histogramGenerator = HistogramGeneratorType::New();

  histogramGenerator->SetInput(  image  );



  histogramGenerator->SetNumberOfBins( hsize );
  histogramGenerator->SetMarginalScale( 10.0 );
  histogramGenerator->Compute();

  typedef HistogramGeneratorType::HistogramType  HistogramType;

  const HistogramType * histogram = histogramGenerator->GetOutput();

  const unsigned int histogramSize = histogram->Size();

  std::cout << "Histogram size " << histogramSize << std::endl;

  unsigned int channel = 0;  // red channel

  std::cout << "Histogram of the red component" << std::endl;

  const unsigned int expectedFrequency = 127 * 127;

  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    if( histogram->GetFrequency( bin, channel ) != expectedFrequency ) 
       {
       std::cerr << "Error in bin= " << bin << " channel = " << channel << std::endl;
       std::cerr << "Frequency was= " <<  histogram->GetFrequency( bin, channel ) << " Instead of the expected " << expectedFrequency << std::endl;
       return EXIT_FAILURE;
       }
    }


  // Now compute the histogram for the Green component
  hsize[0] =   1;  // number of bins for the Red   channel
  hsize[1] = 127;  // number of bins for the Green channel
  hsize[2] =   1;  // number of bins for the Blue  channel

  histogramGenerator->SetNumberOfBins( hsize );
  histogramGenerator->SetMarginalScale( 10.0 );
  histogramGenerator->Compute();

  channel = 1;  // green channel

  std::cout << "Histogram of the green component" << std::endl;

  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    std::cout << "bin = " << bin << " frequency = ";
    std::cout << histogram->GetFrequency( bin, channel ) << std::endl;
    }


  // Now compute the histogram for the Blue component
  size[0] =   1;  // number of bins for the Red   channel
  size[1] =   1;  // number of bins for the Green channel
  size[2] = 127;  // number of bins for the Blue  channel

  histogramGenerator->SetNumberOfBins( size );
  histogramGenerator->SetMarginalScale( 10.0 );
  histogramGenerator->Compute();

  channel = 2;  // blue channel

  std::cout << "Histogram of the blue component" << std::endl;

  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    std::cout << "bin = " << bin << " frequency = ";
    std::cout << histogram->GetFrequency( bin, channel ) << std::endl;
    }


  // Now compute the joint histogram for the three components
  hsize[0] = 127;  // number of bins for the Red   channel
  hsize[1] = 127;  // number of bins for the Green channel
  hsize[2] = 127;  // number of bins for the Blue  channel

  histogramGenerator->SetNumberOfBins( hsize );
  histogramGenerator->SetMarginalScale( 10.0 );
  histogramGenerator->Compute();

  return EXIT_SUCCESS;
  
}


