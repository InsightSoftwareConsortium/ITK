/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGridForwardWarpImageFilterTest.cxx
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
#include "itkVector.h"
#include "itkGridForwardWarpImageFilter.h"


int itkGridForwardWarpImageFilterTest(int, char* [] ) 
{

  // Define the dimension of the images
  const unsigned int ImageDimension = 3;

  typedef itk::Vector< double, ImageDimension >   DeformationPixelType;
  typedef unsigned char                           OutputPixelType;

  // Declare the types of the images
  typedef itk::Image<DeformationPixelType, ImageDimension>  DeformationFieldType;
  typedef itk::Image<OutputPixelType, ImageDimension>       OutputImageType;
  
  // Declare Iterator types apropriated for each image
  typedef itk::ImageRegionIteratorWithIndex< DeformationFieldType >  DeformationIteratorType;
  typedef itk::ImageRegionIteratorWithIndex< OutputImageType >  OutputIteratorType;


  // Declare the type of the index to access images
  typedef itk::Index<ImageDimension>         IndexType;

  // Declare the type of the size
  typedef itk::Size<ImageDimension>          SizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<ImageDimension>   RegionType;

  // Create two images
  DeformationFieldType ::Pointer inputDeformationField  = DeformationFieldType ::New();
  
  // Define their size, and start index
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputDeformationField->SetLargestPossibleRegion( region );
  inputDeformationField->SetBufferedRegion( region );
  inputDeformationField->SetRequestedRegion( region );
  inputDeformationField->Allocate();

  // Create one iterator for the Input Image (this is a light object)
  DeformationIteratorType it( inputDeformationField,
                              inputDeformationField->GetBufferedRegion() );

  // Initialize the content of Image A
  DeformationPixelType vectorValue;
  vectorValue.Fill( 5.0 ); // FIXME: replace with something more interesting...
  
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( vectorValue );
    std::cout << it.Get() << std::endl;
    ++it;
    }

  // Declare the type for the Log filter
  typedef itk::GridForwardWarpImageFilter<
    DeformationFieldType, OutputImageType  >   FilterType;


  // Create one Filter
  FilterType::Pointer filter = FilterType::New();


  // Connect the input images
  filter->SetInput( inputDeformationField );

  // Execute the filter
  filter->Update();

  // Get the Smart Pointer to the Filter Output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Create an iterator for going through the image output
  OutputIteratorType ot(outputImage, outputImage->GetRequestedRegion());
  
  //  Check the content of the result image
  std::cout << "Verification of the output " << std::endl;

  ot.GoToBegin();
  it.GoToBegin();
  while( !ot.IsAtEnd() )
    {
    DeformationPixelType input  = it.Get();
    OutputPixelType output = ot.Get();
    std::cout << input << " => ";
    std::cout << output  << std::endl;
    ++ot;
    ++it;
    }

  return EXIT_SUCCESS;
}
