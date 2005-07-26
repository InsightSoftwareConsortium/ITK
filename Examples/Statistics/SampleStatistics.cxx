/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SampleStatistics.cxx
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

// Software Guide : BeginLatex
//
// \index{itk::Statistics::MeanCalculator}
// \index{itk::Statistics::CovarianceCalculator}
// \index{Statistics!Mean}
// \index{Statistics!Covariance}
//
// We include the header file for the \doxygen{Vector} class that will
// be our measurement vector template in this example. 
//
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
  //
  // The following code snippet will create a ListSample object
  // with three-component float measurement vectors and put five
  // measurement vectors in the ListSample object.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int MeasurementVectorLength = 3;
  typedef itk::Vector< float, MeasurementVectorLength > MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;
  SampleType::Pointer sample = SampleType::New();
  sample->SetMeasurementVectorSize( MeasurementVectorLength );
  MeasurementVectorType mv;
  mv[0] = 1.0;
  mv[1] = 2.0;
  mv[2] = 4.0;
  
  sample->PushBack( mv );

  mv[0] = 2.0;
  mv[1] = 4.0;
  mv[2] = 5.0;
  sample->PushBack( mv );
  
  mv[0] = 3.0;
  mv[1] = 8.0;
  mv[2] = 6.0;
  sample->PushBack( mv );

  mv[0] = 2.0;
  mv[1] = 7.0;
  mv[2] = 4.0;
  sample->PushBack( mv );

  mv[0] = 3.0;
  mv[1] = 2.0;
  mv[2] = 7.0;
  sample->PushBack( mv );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // To calculate the mean (vector) of a sample, we instantiate the
  // \subdoxygen{Statistics}{MeanCalculator} class that implements the mean
  // algorithm and plug in the sample using the
  // \code{SetInputSample(sample*)} method.  By calling the \code{Update()}
  // method, we run the algorithm. We get the mean vector using the
  // \code{GetOutput()} method. The output from the \code{GetOutput()} method
  // is the pointer to the mean vector.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::MeanCalculator< SampleType > MeanAlgorithmType;
  
  MeanAlgorithmType::Pointer meanAlgorithm = MeanAlgorithmType::New();

  meanAlgorithm->SetInputSample( sample );
  meanAlgorithm->Update();

  std::cout << "Sample mean = " << *(meanAlgorithm->GetOutput()) << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // To use the covariance calculation algorithm, we have two options.  Since
  // we already have the mean calculated by the MeanCalculator, we can
  // plug-in its output to an instance of
  // \subdoxygen{Statistics}{CovarianceCalculator} using the \code{SetMean()}
  // method. The other option is not to set the mean at all and just call the
  // \code{Update()} method. The covariance calculation algorithm will
  // compute the mean and covariance together in one pass. If you have
  // already set the mean as in this example and you want to run one pass
  // algorithm, simply pass a null pointer as the mean vector.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::CovarianceCalculator< SampleType > 
    CovarianceAlgorithmType;
  CovarianceAlgorithmType::Pointer covarianceAlgorithm = 
    CovarianceAlgorithmType::New();

  covarianceAlgorithm->SetInputSample( sample );
  covarianceAlgorithm->SetMean( meanAlgorithm->GetOutput() );
  covarianceAlgorithm->Update();

  std::cout << "Sample covariance = " << std::endl ; 
  std::cout << *(covarianceAlgorithm->GetOutput()) << std::endl;

  covarianceAlgorithm->SetMean( 0 );
  covarianceAlgorithm->Update();

  std::cout << "Using the one pass algorithm:" << std::endl;
  std::cout << "Mean = " << std::endl ; 
  std::cout << *(covarianceAlgorithm->GetMean()) << std::endl;

  std::cout << "Covariance = " << std::endl ; 
  std::cout << *(covarianceAlgorithm->GetOutput()) << std::endl;
  // Software Guide : EndCodeSnippet
  return 0;
}
