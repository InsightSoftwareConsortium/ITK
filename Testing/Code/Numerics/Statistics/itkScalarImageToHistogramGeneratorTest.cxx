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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


#include "itkScalarImageToHistogramGenerator.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"

int itkScalarImageToHistogramGeneratorTest( int , char * [] )
{


    {
    typedef unsigned long                         PixelType;
    const unsigned int imageSize=256;
    const unsigned int                            Dimension = 2;

    typedef itk::Image< PixelType, Dimension >    ImageType;


    ImageType::Pointer image = ImageType::New();

    ImageType::RegionType region;
    ImageType::SizeType   size;
    ImageType::IndexType  start;

    size[0] = imageSize;
    size[1] = imageSize;

    start[0] = 0;
    start[1] = 0;

    region.SetIndex( start );
    region.SetSize( size );

    image->SetRegions( region );
    image->Allocate();

    //  Now fill up the image will all the combinations of  
    //  values from 0-imageSize.
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

    histogramGenerator->SetNumberOfBins( imageSize );
    histogramGenerator->SetMarginalScale( 10.0 );
    histogramGenerator->Compute();

    typedef HistogramGeneratorType::HistogramType  HistogramType;

    const HistogramType * histogram = histogramGenerator->GetOutput();

    const unsigned int histogramSize = histogram->Size();

    std::cout << "Histogram size " << histogramSize << std::endl;

    const unsigned int channel = 0;  // the only channel

    for( unsigned int bin=0; bin < histogramSize; bin++ )
      {
      if( histogram->GetFrequency( bin, channel ) != imageSize ) 
        {
        std::cerr << "Error in bin= " << bin << " channel = " << channel << std::endl;
        std::cerr << "Frequency was= " <<  histogram->GetFrequency( bin, channel ) << " Instead of the expected " << imageSize << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

    {
    const unsigned int NumberOfBins = 4;
    typedef unsigned long PixelType;

    // NOTE :  it is not uncommon to have a 3D image with more than 4096 * 4096
    // voxels in the background (i.e. 0'th histogram bin), and this causes an
    // overflow event
    const unsigned int imageSize=4096+NumberOfBins;  

    // Note 4096^2 = 16777216, which is greater than the number that can be
    // accurately incremented by a floating point number.
    //const unsigned int imageSize=406+NumberOfBins;  // Note 4096^2 =
    //16777216, which is greater than the number that can be accurately
    //incremented by a floating point number.
    const unsigned int                            Dimension = 2;

    typedef itk::Image< PixelType, Dimension >    ImageType;


    ImageType::Pointer image = ImageType::New();

    ImageType::RegionType region;
    ImageType::SizeType   size;
    ImageType::IndexType  start;

    size[0] = imageSize;
    size[1] = imageSize;

    start[0] = 0;
    start[1] = 0;

    region.SetIndex( start );
    region.SetSize( size );

    image->SetRegions( region );
    image->Allocate();

    //  Now fill up the image will all the combinations of  
    //  values from 0-imageSize.
    itk::ImageRegionIteratorWithIndex< ImageType >  it( image, region );

    it.GoToBegin();
    for(unsigned int temp=0;temp<NumberOfBins;temp++)
      {
      it.Set((temp+1)*1000); //Make it have a range
      ++it;
      }

    while( !it.IsAtEnd() )
      {
      it.Set( 0 );
      ++it;
      }


    typedef itk::Statistics::ScalarImageToHistogramGenerator< 
      ImageType 
      >   HistogramGeneratorType;


    HistogramGeneratorType::Pointer histogramGenerator = HistogramGeneratorType::New();

    histogramGenerator->SetInput(  image  );

    histogramGenerator->SetNumberOfBins( 4 );
    histogramGenerator->SetMarginalScale( 10.0 );

    try
      {
      histogramGenerator->Compute();
      std::cout << "Failed to catch expected exception about saturation of counters" << std::endl;
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cout << "Get Expected Exception" << std::endl;
      std::cout << excp << std::endl;
      }

    typedef HistogramGeneratorType::HistogramType  HistogramType;

    const HistogramType * histogram = histogramGenerator->GetOutput();

    const unsigned int histogramSize = histogram->Size();

    std::cout << "Histogram size " << histogramSize << std::endl;

    unsigned int channel = 0;

    int status = EXIT_SUCCESS;

    for( unsigned int bin=1; bin < histogramSize; bin++ )
      {
      if( histogram->GetFrequency( bin, channel ) != 1 )
        {
        std::cerr << "Error in bin= " << bin << " channel = " << channel << std::endl;
        std::cerr << "Frequency was= " <<  (unsigned int)histogram->GetFrequency( bin, channel ) << " Instead of the expected " << 1 << std::endl;
        status = EXIT_FAILURE;
        }
      }

    return status;
    }

  return EXIT_SUCCESS;

}
