/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMathematicalMorphologyImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/




#include "itkBinaryDilateImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkGrayscaleErodeImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImage.h"
#include "itkBinaryBallStructuringElement.h"


int itkMathematicalMorphologyImageFilterTest(int, char* [] ) 
{

  // Define the dimension of the images
  const unsigned int Dimension = 3;

  // Declare the types of the images
  typedef itk::Image<unsigned char, Dimension>  ImageType;

  // Declare the type of the Index, Size and Region
  typedef itk::Index<Dimension>             IndexType;
  typedef itk::Size<Dimension>              SizeType;
  typedef itk::ImageRegion<Dimension>       RegionType;

  // Create the image
  ImageType::Pointer inputImage  = ImageType::New();
  
  // Define their size, and start index
  SizeType size;
  size[0] = 20;
  size[1] = 20;
  size[2] = 20;

  IndexType start;
  start.Fill(0);

  RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetRegions( region );
  inputImage->Allocate();

  // Declare Iterator type for the input image
  typedef itk::ImageRegionIterator<ImageType>  IteratorType;

  // Create one iterator for the Input Image A (this is a light object)
  IteratorType it( inputImage, inputImage->GetRequestedRegion() );

  // Initialize the content of Image A
  while( !it.IsAtEnd() ) 
    {
    it.Set( 0 );
    ++it;
    }

  size[0]  = 10;
  size[1]  = 10;
  size[2]  = 10;

  start[0] =  5;
  start[1] =  5;
  start[2] =  5;

  // Create one iterator for an internal region
  region.SetSize( size );
  region.SetIndex( start );
  IteratorType itb( inputImage, region );

  // Initialize the content the internal region
  while( !itb.IsAtEnd() ) 
  {
    itb.Set( 100 );
    ++itb;
  }

  // Declare the type of the Structuring element to be used
  typedef itk::BinaryBallStructuringElement< 
                            ImageType::PixelType,
                            Dimension>                  StructuringElementType;

  // Declare the type for the Morphology Filters to be Tested
  typedef itk::GrayscaleDilateImageFilter< 
                                ImageType, 
                                ImageType, 
                                StructuringElementType >  GrayDilateFilterType;

  typedef itk::GrayscaleErodeImageFilter< 
                                ImageType,
                                ImageType,
                                StructuringElementType >   GrayErodeFilterType;

  typedef itk::BinaryDilateImageFilter< 
                                ImageType,
                                ImageType,
                                StructuringElementType >    BinaryDilateFilterType;

  typedef itk::BinaryErodeImageFilter< 
                                ImageType,
                                ImageType,
                                StructuringElementType >    BinaryErodeFilterType;
            
  GrayErodeFilterType::Pointer     grayErode     = GrayErodeFilterType::New();
  GrayDilateFilterType::Pointer    grayDilate    = GrayDilateFilterType::New();
  BinaryErodeFilterType::Pointer   binaryErode   = BinaryErodeFilterType::New();
  BinaryDilateFilterType::Pointer  binaryDilate  = BinaryDilateFilterType::New();

  grayErode->SetInput( inputImage );
  grayDilate->SetInput( inputImage );
  binaryDilate->SetInput( inputImage );
  binaryErode->SetInput( inputImage );

  StructuringElementType structuringElement;

  structuringElement.SetRadius( 2 );  // 5x5x5 structuring element

  grayErode->SetKernel( structuringElement );
  grayDilate->SetKernel( structuringElement );
  binaryErode->SetKernel( structuringElement );
  binaryDilate->SetKernel( structuringElement );

  try
    {
    std::cout << "Running grayscale erode " << std::endl;
    grayErode->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown during grayErode filter Update" << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    std::cout << "Running grayscale Dilate " << std::endl;
    grayDilate->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown during grayDilate filter Update" << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    std::cout << "Running binary Erode " << std::endl;
    binaryErode->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown during binary Erode Update" << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    std::cout << "Running binary Dilate " << std::endl;
    binaryDilate->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown during binaryDilate filter Update" << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "PASSED ! " << std::endl;

  return EXIT_SUCCESS;

}




