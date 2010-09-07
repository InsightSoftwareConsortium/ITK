/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: ImageReadRegionOfInterestWrite.cxx,v $
  Language:  C++
  Date:      $Date: 2005/08/27 01:46:11 $
  Version:   $Revision: 1.12 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


#include "itkRGBPixel.h"
#include "itkJPEG2000ImageIOFactory.h"
#include "itkJPEG2000ImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"


int itkJPEG2000ImageIOTest04( int argc, char * argv[] )
{
  // Verify the number of parameters in the command line
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile J2KOutputImageFile tileSizeX tileSizeY" << std::endl;
    return EXIT_FAILURE;
    }

  //  Register the factory
  itk::JPEG2000ImageIOFactory::RegisterOneFactory();


  //  Image types are defined below.
  typedef itk::RGBPixel<unsigned char>  PixelType;
  const   unsigned int        Dimension = 2;

  typedef itk::Image< PixelType,  Dimension >    InputImageType;
  typedef itk::Image< PixelType, Dimension >    OutputImageType;

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  typedef itk::JPEG2000ImageIO                     IOBaseType;

  IOBaseType::Pointer base = IOBaseType::New();
  base->SetTileSize( atoi( argv[3] ), atoi( argv[4] ) );

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  const std::string inputFilename  = argv[1];
  const std::string outputFilename = argv[2];

  reader->SetFileName( inputFilename  );
  writer->SetFileName( outputFilename );

  writer->SetInput( reader->GetOutput() );
  writer->SetImageIO( base );
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
