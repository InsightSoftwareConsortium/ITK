/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWeightedMeanCalculatorTest.cxx
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
#include "itkWeightedMeanCalculator.h"
#include "itkRandomImageSource.h"
#include "itkImageRegionIterator.h"

#include "itkVector.h"
#include "itkNumericTraits.h"
#include "itkFunctionBase.h"

enum { MeasurementVectorSize = 2 } ;
typedef float MeasurementType ;
typedef itk::Vector< MeasurementType, MeasurementVectorSize > 
MeasurementVectorType ;

class WeightedMeanCalculatorTestWeightFunction :
  public itk::FunctionBase< MeasurementVectorType, double >
{
public:
  /** Standard class typedefs. */
  typedef WeightedMeanCalculatorTestWeightFunction Self;
  typedef itk::FunctionBase< MeasurementVectorType, double > Superclass;
  typedef itk::SmartPointer<Self> Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;
  
  /** Standard macros. */
  itkTypeMacro(WeightedMeanCalculatorTestWeightFunction, FunctionBase);
  itkNewMacro(Self) ;

  /** Input type */
  typedef MeasurementVectorType InputType;

  /** Output type */
  typedef double OutputType;

  /**Evaluate at the specified input position */
  OutputType Evaluate( const InputType& input ) const 
  {
    MeasurementVectorType measurements ;
    // there is only one measurement vector that has
    // values [12, 12] in the sample and it corresponds to
    // the pixel at index [2,2]
    measurements.Fill(12.0f) ;
    if ( input != measurements )
      {
        return 1.0 ;
      }
    else
      {
        return 0.0 ;
      }
  }

protected:
  WeightedMeanCalculatorTestWeightFunction() {}
  ~WeightedMeanCalculatorTestWeightFunction() {}
} ; // end of class

int itkWeightedMeanCalculatorTest(int, char* [] ) 
{
  std::cout << "WeightedMeanCalculator Test \n \n"; 
  bool pass = true;
  std::string whereFail = "" ;

  // Now generate an image
  typedef itk::Image< MeasurementVectorType, 2 > ImageType ;
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
  MeasurementVectorType sum ;
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

  // creates an ImageToListAdaptor object
  typedef  itk::Statistics::ImageToListAdaptor< ImageType >
    ImageToListAdaptorType ;

  ImageToListAdaptorType::Pointer sample = ImageToListAdaptorType::New() ;
  sample->SetImage(image) ;

  // set the pixel value at index [2, 2] to zero
  // to compare the values from the calculator and the 
  // internal calcuation of this test program
  index[0] = 2 ;
  index[1] = 2 ;

  ImageType::PixelType aPixel = image->GetPixel(index) ;
  sum = sum - aPixel ;
  MeasurementVectorType mean = sum / static_cast< MeasurementType >(count - 1) ;

  typedef itk::Statistics::WeightedMeanCalculator< ImageToListAdaptorType > 
    CalculatorType;

  CalculatorType::WeightArrayType weightArray(sample->Size()) ;
  weightArray.Fill(1.0) ;
  // array index 12 corresponds to the image index (2,2) 
  weightArray[12] = 0.0 ;

  CalculatorType::Pointer calculator = CalculatorType::New() ;
  calculator->SetInputSample(sample.GetPointer()) ;
  calculator->SetWeights(&weightArray) ;
  calculator->Update() ;

  CalculatorType::OutputType* meanOutput = calculator->GetOutput() ;
  if ((*meanOutput)[0] != mean[0] || 
      (*meanOutput)[1] != mean[1])
    {
      pass = false ;
    }

  WeightedMeanCalculatorTestWeightFunction::Pointer weightFunction =
    WeightedMeanCalculatorTestWeightFunction::New() ;
  calculator->SetWeightFunction(weightFunction.GetPointer()) ;
  calculator->Update() ;

  meanOutput = calculator->GetOutput() ;
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



