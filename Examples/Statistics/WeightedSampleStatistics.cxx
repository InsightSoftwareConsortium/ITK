/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    WeightedSampleStatistics.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// \index{itk::Statistics::WeightedMeanCalculator|textbf}
// \index{itk::Statistics::WeightedCovarianceCalculator|textbf}
// \index{Statistics!Weighted mean|textbf}
// \index{Statistics!Weighted covariance|textbf}
//
// We include the header file for the \doxygen{Vector} class that will
// be our measurement vector template in this example. 
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// We will use the \subdoxygen{Statistics}{ListSample} as our sample
// template. We include the header for the class too.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// The following headers are for the weighted covariance algorithms.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkWeightedMeanCalculator.h"
#include "itkWeightedCovarianceCalculator.h"
// Software Guide : EndCodeSnippet

typedef itk::Vector< float, 3 > MeasurementVectorType ;

class ExampleWeightFunction :
  public itk::FunctionBase< MeasurementVectorType, double >
{
public:
  /** Standard class typedefs. */
  typedef ExampleWeightFunction Self;
  typedef itk::FunctionBase< MeasurementVectorType, double > Superclass;
  typedef itk::SmartPointer<Self> Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;
  
  /** Standard macros. */
  itkTypeMacro(ExampleWeightFunction, FunctionBase);
  itkNewMacro(Self) ;

  /** Input type */
  typedef MeasurementVectorType InputType;

  /** Output type */
  typedef double OutputType;

  /**Evaluate at the specified input position */
  OutputType Evaluate( const InputType& input ) const 
  {
    if ( input[0] < 3.0 )
      {
      return 1.0 ;
      }
    else
      {
      return 0.0 ;
      }
  }

protected:
  ExampleWeightFunction() {}
  ~ExampleWeightFunction() {}
} ; // end of class

int main()
{
  // Software Guide : BeginLatex
  // The following code snippet will create a \code{ListSample} object
  // with three-component float measurement vectors and put three
  // measurement vectors in tht \code{ListSample} object.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;
  SampleType::Pointer sample = SampleType::New() ;
  MeasurementVectorType mv ;
  mv[0] = 1.0 ;
  mv[1] = 2.0 ;
  mv[2] = 4.0 ;
  
  sample->PushBack( mv ) ;

  mv[0] = 2.0 ;
  mv[1] = 4.0 ;
  mv[2] = 5.0 ;
  sample->PushBack( mv ) ;
  
  mv[0] = 3.0 ;
  mv[1] = 8.0 ;
  mv[2] = 6.0 ;
  sample->PushBack( mv ) ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The robust version of covariance algorithms require
  // weight values for measurement vectors. We have two ways of
  // providing weight values for the weighted mean and weighted
  // covariance algorithms.
  //
  // The first method is to plug in an array of weight values. The
  // size of the weight value array should be equal to that of the
  // measurement vectors. In both algorithms, we use the
  // \code{SetWeights(weight value array*)}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::WeightedMeanCalculator< SampleType >
    WeightedMeanAlgorithmType ;

  WeightedMeanAlgorithmType::WeightArrayType weightArray( sample->Size() ) ;
  weightArray.Fill( 1.0 ) ;
  weightArray[2] = 0.0 ;

  WeightedMeanAlgorithmType::Pointer weightedMeanAlgorithm = 
    WeightedMeanAlgorithmType::New() ;

  weightedMeanAlgorithm->SetInputSample( sample ) ;
  weightedMeanAlgorithm->SetWeights( &weightArray ) ;
  weightedMeanAlgorithm->Update() ;

  std::cout << "Sample weighted mean = " 
            << *(weightedMeanAlgorithm->GetOutput()) << std::endl ;

  typedef itk::Statistics::WeightedCovarianceCalculator< SampleType >
    WeightedCovarianceAlgorithmType ;
  
  WeightedCovarianceAlgorithmType::Pointer weightedCovarianceAlgorithm = 
    WeightedCovarianceAlgorithmType::New() ;

  weightedCovarianceAlgorithm->SetInputSample( sample ) ;
  weightedCovarianceAlgorithm->SetMean( weightedMeanAlgorithm->GetOutput() ) ;
  weightedCovarianceAlgorithm->SetWeights( &weightArray ) ;
  weightedCovarianceAlgorithm->Update() ;

  std::cout << "Sample weighted covariance = " << std::endl ; 
  std::cout << *(weightedCovarianceAlgorithm->GetOutput()) << std::endl ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The second method is to plug-in a function that returns a weight
  // value that is usually a function of each measurement vector. Since
  // the \code{weightedMeanAlgorithm} and
  // \code{weightedCovarianceAlgorithm} already have the input sample
  // plugged in, we need call only the \code{SetWeightFunction(weight
  // function*)} method. For the \code{weightedCovarianceAlgorithm}, we
  // replace the mean vector input with the output from the
  // \code{weightedMeanAlgorithm}. 
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ExampleWeightFunction::Pointer weightFunction =
    ExampleWeightFunction::New() ;

  weightedMeanAlgorithm->SetWeightFunction( weightFunction ) ;
  weightedMeanAlgorithm->Update() ;

  std::cout << "Sample weighted mean = " 
            << *(weightedMeanAlgorithm->GetOutput()) << std::endl ;

  weightedCovarianceAlgorithm->SetMean( weightedMeanAlgorithm->GetOutput() ) ;
  weightedCovarianceAlgorithm->SetWeightFunction( weightFunction ) ;
  weightedCovarianceAlgorithm->Update() ;

  std::cout << "Sample weighted covariance = " << std::endl ; 
  std::cout << *(weightedCovarianceAlgorithm->GetOutput()) << std::endl ;
  // Software Guide : EndCodeSnippet
  return 0 ;
}
