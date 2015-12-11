/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

// Software Guide : BeginLatex
//
// \index{Statistics!Gaussian (normal) probability density function}
//
// \index{itk::Statistics::GaussianMembershipFunction}
//
// The Gaussian probability density function
// \subdoxygen{Statistics}{GaussianMembershipFunction} requires two
// distribution parameters---the mean vector and the covariance matrix.
//
// We include the header files for the class and the \doxygen{Vector}.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
#include "itkGaussianMembershipFunction.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We define the type of the measurement vector that will be input to
// the Gaussian membership function.
//
// Software Guide : EndLatex

int main(int, char*[])
{
  // Software Guide : BeginCodeSnippet
  typedef itk::Vector< float, 2 > MeasurementVectorType;
  // Software Guide : EndCodeSnippet
  // Software Guide : BeginLatex
  //
  // The instantiation of the function is done through the usual
  // \code{New()} method and a smart pointer.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::GaussianMembershipFunction< MeasurementVectorType >
    DensityFunctionType;
  DensityFunctionType::Pointer densityFunction = DensityFunctionType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The length of the measurement vectors in the membership function, in this
  // case a vector of length 2, is specified using the
  // \code{SetMeasurementVectorSize()} method.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  densityFunction->SetMeasurementVectorSize( 2 );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We create the two distribution parameters and set them. The mean is
  // [0, 0], and the covariance matrix is a 2 x 2 matrix:
  // \[
  // \begin{pmatrix}
  // 4 & 0 \cr
  // 0 & 4
  // \end{pmatrix}
  // \]
  // We obtain the probability density for the measurement vector: [0, 0]
  // using the \code{Evaluate(measurement vector)} method and print it out.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  DensityFunctionType::MeanVectorType mean( 2 );
  mean.Fill( 0.0 );

  DensityFunctionType::CovarianceMatrixType cov;
  cov.SetSize( 2, 2 );
  cov.SetIdentity();
  cov *= 4;

  densityFunction->SetMean( mean );
  densityFunction->SetCovariance( cov );

  MeasurementVectorType mv;
  mv.Fill( 0 );

  std::cout << densityFunction->Evaluate( mv ) << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
