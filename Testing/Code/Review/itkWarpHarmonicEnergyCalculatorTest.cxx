/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWarpHarmonicEnergyCalculatorTest.cxx
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
#include "itkWarpHarmonicEnergyCalculator.h"
#include "itkImageRegionIteratorWithIndex.h"


int itkWarpHarmonicEnergyCalculatorTest(int, char* [] )
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
  typedef itk::ImageRegionIteratorWithIndex< OutputImageType >       OutputIteratorType;


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

  // Declare the type for the calculator
  typedef itk::WarpHarmonicEnergyCalculator< DeformationFieldType > CalculatorType;


  // Create one Filter
  CalculatorType::Pointer calculator = CalculatorType::New();


  // Connect the input images
  calculator->SetImage( inputDeformationField );

  // Execute the calculator
  calculator->Compute();

  const double energy = calculator->GetHarmonicEnergy();
  std::cout << energy << std::endl;

  if ( vcl_abs(energy) > 1e-8 )  return EXIT_FAILURE;

  return EXIT_SUCCESS;
}
