/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SampleStatistics.cxx
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
// \index{itk::Statistics::MeanCalculator|textbf}
// \index{itk::Statistics::CovarianceCalculator!|textbf}
// \index{Statistics!Mean|textbf}
// \index{Statistics!Covariance|textbf}
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
// The following headers are for sample statistics algorithms.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkMeanCalculator.h"
#include "itkCovarianceCalculator.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  // The following code snippet will create a \code{ListSample} object
  // with three-component float measurement vectors and put three
  // measurement vectors in the \code{ListSample} object.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Vector< float, 3 > MeasurementVectorType ;
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
  // To calculate the mean (vector) of a sample, we instantiate the mean
  // algorithm and plug in the sample using the
  // \code{SetInputSample(sample*)}. By calling the \code{Update()}
  // method, we run the algorithm. We get the mean vector using the
  // \code{GetOutput()} method. The output from the \code{GetOutput()}
  // is the pointer to the mean vector.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::MeanCalculator< SampleType >
    MeanAlgorithmType ;
  
  MeanAlgorithmType::Pointer meanAlgorithm = MeanAlgorithmType::New() ;

  meanAlgorithm->SetInputSample( sample ) ;
  meanAlgorithm->Update() ;

  std::cout << "Sample mean = " << *(meanAlgorithm->GetOutput()) << std::endl ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // To use the covariance calculation algorithm, we have to follow a
  // similar procedure except for the additional \code{SetMean(mean
  // vector*)}. We must plug-in the mean vector before calling the
  // \code{Update()} method.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::CovarianceCalculator< SampleType >
    CovarianceAlgorithmType ;
  
  CovarianceAlgorithmType::Pointer covarianceAlgorithm = 
    CovarianceAlgorithmType::New() ;

  covarianceAlgorithm->SetInputSample( sample ) ;
  covarianceAlgorithm->SetMean( meanAlgorithm->GetOutput() ) ;
  covarianceAlgorithm->Update() ;

  std::cout << "Sample covariance = " << std::endl ; 
  std::cout << *(covarianceAlgorithm->GetOutput()) << std::endl ;
  // Software Guide : EndCodeSnippet

  return 0 ;
}
