/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCovarianceCalculatorTest.cxx
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
#include "itkCovarianceCalculator.h"
#include "itkRandomImageSource.h"
#include "itkImageRegionIterator.h"
#include "itkFixedArray.h"
#include "itkVector.h"
#include "itkMeanCalculator.h"

int itkCovarianceCalculatorTest(int, char**) 
{
  std::cout << "CovarianceCalculator Test \n \n"; 
  bool pass = true;
  std::string whereFail = "" ;

  // Now generate an image
  enum { MeasurementVectorSize = 1 } ;
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

  // creates an ImageToListAdaptor object
  typedef  itk::Statistics::ImageToListAdaptor< ImageType >
    ImageToListAdaptorType ;

  ImageToListAdaptorType::Pointer sample = ImageToListAdaptorType::New() ;
  sample->SetImage(image) ;

  typedef itk::Statistics::MeanCalculator< ImageToListAdaptorType > 
    MeanCalculatorType ;
  MeanCalculatorType::Pointer meanCalculator = MeanCalculatorType::New() ;
  meanCalculator->SetInputSample(sample) ;
  meanCalculator->Update() ;
  MeanCalculatorType::OutputType* mean = meanCalculator->GetOutput() ;
  // calculates variance
  double count = 0.0 ;
  double variance = 0.0 ;
  double diff ;
  typedef itk::ImageRegionIterator< ImageType > ImageIterator ;
  ImageIterator iter(image, region) ;
  iter.GoToBegin() ;
  while (!iter.IsAtEnd())
    {
      diff = iter.Get()[0] - float((*mean)[0]) ;
      variance += diff * diff ; 
      count++ ; 
      ++iter ;
    }
  variance /= static_cast< double>(count) ;


  typedef itk::Statistics::CovarianceCalculator< ImageToListAdaptorType > 
    CalculatorType;

  CalculatorType::Pointer calculator = CalculatorType::New() ;
  
  calculator->SetInputSample(sample) ;
  calculator->SetMean(meanCalculator->GetOutput()) ;
  calculator->Update() ;

  if (calculator->GetOutput()->GetVnlMatrix().get(0,0) != variance)
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



