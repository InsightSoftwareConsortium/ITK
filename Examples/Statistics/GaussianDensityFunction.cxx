/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    GaussianDensityFunction.cxx
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
// \index{Statistics!Guassian (normal) probability density function|textbf}
//
// \index{itk::Statistics::GaussianDensityFunction|textbf}
//
// The Gaussian probability density function
// (\subdoxygen{Statistics}{GaussianDensityFunction} requires two
// distribution parameters --- the mean vector and the covariance matrix.
//
// We include the header files for the class and the \doxygen{Vector}.
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
#include "itkGaussianDensityFunction.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// We define the type of the measurement vector that will be input of
// the Gaussian density function.
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
typedef itk::Vector< float, 2 > MeasurementVectorType ;
// Software Guide : EndCodeSnippet

int main()
{

  // Software Guide : BeginLatex
  // The instantiation of the function is done through the usual
  // \code{New()} method and a smart pointer.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::GaussianDensityFunction< MeasurementVectorType > 
    DensityFunctionType ;
  DensityFunctionType::Pointer densityFunction = DensityFunctionType::New() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We create the two distribution parameters and set them. The mean is
  // [0, 0], and the covariance matrix is:
  // \[
  // \begin{pmatrix}
  // 4 & 0 \\
  // 0 & 4
  // \end{pmatrix}
  // \]
  // We get the probability density for the measurement vector: [0, 0]
  // using the \code{Evaluate(measurement vector)} method and print it out.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  DensityFunctionType::MeanType mean ;
  mean.Fill( 0.0 ) ;

  DensityFunctionType::CovarianceType cov ;
  cov.SetIdentity() ;
  cov *= 4 ;

  densityFunction->SetMean( &mean ) ;
  densityFunction->SetCovariance( &cov ) ;
  
  MeasurementVectorType mv ;
  mv.Fill( 0 ) ;

  std::cout << densityFunction->Evaluate( mv ) << std::endl ;
  // Software Guide : EndCodeSnippet

  return 0 ;
}
