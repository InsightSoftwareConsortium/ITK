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

#include "itkFixedArray.h"
#include "itkVector.h"

int itkMeanCalculatorTest(int, char**) 
{
  std::cout << "MeanCalculator Test \n \n"; 
  bool pass = true;
  std::string whereFail = "" ;

  // Now generate an image
  enum { MeasurementVectorSize = 2 } ;
  typedef float MeasurementType ;
  typedef itk::FixedArray< MeasurementType, MeasurementVectorSize > 
    MeasurementVectorType ;
  typedef itk::Image< MeasurementVectorType, 3 > ImageType ;
  ImageType::Pointer image = ImageType::New() ;
  ImageType::RegionType region ;
  ImageType::SizeType size ;
  ImageType::IndexType index ;
  index.Fill(0) ;
  size.Fill(5) ;
  region.SetIndex(index) ;
  region.SetSize(size) ;
  
  image->SetLargestPossibleRegion(region) ;
  image->SetBufferedRegion(region) ;
  image->Allocate() ;

  typedef itk::ImageRegionIterator< ImageType > ImageIterator ;
  ImageIterator iter(image, region) ;

  unsigned int count = 0 ;
  itk::Vector< double, 2 > sum ;
  sum[0] = 0.0 ;
  sum[1] = 0.0 ;
  MeasurementVectorType temp ;
  // fill the image
  while (!iter.IsAtEnd())
    {
      temp[0] = count ;
      temp[1] = count ;
      iter.Set(temp) ;
      sum[0] += iter.Get()[0] ;
      sum[1] += iter.Get()[1] ;
      ++iter ;
      ++count ;
    }
  itk::Vector< double, 2 > mean = sum / static_cast< double >(count) ;

  // creates an ImageToListAdaptor object
  typedef  itk::Statistics::ImageToListAdaptor< ImageType >
    ImageToListAdaptorType ;

  ImageToListAdaptorType::Pointer sample = ImageToListAdaptorType::New() ;
  sample->SetImage(image) ;

  typedef itk::Statistics::MeanCalculator< ImageToListAdaptorType > 
    CalculatorType;

  CalculatorType::Pointer calculator = CalculatorType::New() ;
  
  calculator->SetInputSample(sample.GetPointer()) ;
  calculator->Update() ;

  CalculatorType::OutputType* meanOutput = calculator->GetOutput() ;
  if ((*meanOutput)[0] != mean[0] || 
      (*meanOutput)[1] != mean[1])
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



