/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanCalculatorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageToListAdaptor.h"
#include "itkMeanCalculator.h"
#include "itkRandomImageSource.h"
#include "itkImageRegionIterator.h"

int itkMeanCalculatorTest(int, char**) 
{
  std::cout << "MeanCalculator Test \n \n"; 
  bool pass = true;
  std::string whereFail = "" ;

  // Now generate an image
  typedef itk::Image< float, 3 > FloatImage ;
  FloatImage::Pointer image = FloatImage::New() ;
  FloatImage::RegionType region ;
  FloatImage::SizeType size ;
  FloatImage::IndexType index ;
  index.Fill(0) ;
  size.Fill(5) ;
  region.SetIndex(index) ;
  region.SetSize(size) ;
  
  image->SetLargestPossibleRegion(region) ;
  image->SetBufferedRegion(region) ;
  image->Allocate() ;

  typedef itk::ImageRegionIterator< FloatImage > ImageIterator ;
  ImageIterator iter(image, region) ;

  unsigned int count = 0 ;
  double sum = 0.0 ;
  // fill the image
  while (!iter.IsAtEnd())
    {
      iter.Set(count) ;
      sum += iter.Get() ;
      ++iter ;
      ++count ;
    }
  double mean = sum / static_cast< double>(count) ;

  // creates an ImageToListAdaptor object
  typedef  itk::Statistics::ImageToListAdaptor< FloatImage,
    itk::Statistics::ScalarImageAccessor< FloatImage > >
    ImageToListAdaptorType ;

  ImageToListAdaptorType::Pointer sample = ImageToListAdaptorType::New() ;
  sample->SetImage(image) ;

  typedef itk::Statistics::MeanCalculator< ImageToListAdaptorType > 
    CalculatorType;

  CalculatorType::Pointer calculator = CalculatorType::New() ;
  
  calculator->SetSample(sample) ;
  calculator->Update() ;

  if (calculator->GetOutput()[0] != mean)
    {
      pass = false ;
    }
 
  if( !pass )
    {
      std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}



