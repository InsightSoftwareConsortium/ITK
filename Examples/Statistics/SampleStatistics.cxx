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
// \index{itk::Statistics::MeanCalculator}
// \index{itk::Statistics::CovarianceSampleFilter}
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
//
// We will use the \subdoxygen{Statistics}{ListSample} as our sample
// template. We include the header for the class too.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The following headers are for sample statistics algorithms.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkMeanSampleFilter.h"
#include "itkCovarianceSampleFilter.h"
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
  // \subdoxygen{Statistics}{MeanSampleFilter} class that implements the mean
  // algorithm and plug in the sample using the
  // \code{SetInputSample(sample*)} method.  By calling the \code{Update()}
  // method, we run the algorithm. We get the mean vector using the
  // \code{GetMean()} method. The output from the \code{GetOutput()} method
  // is the pointer to the mean vector.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::MeanSampleFilter< SampleType > MeanAlgorithmType;

  MeanAlgorithmType::Pointer meanAlgorithm = MeanAlgorithmType::New();

  meanAlgorithm->SetInput( sample );
  meanAlgorithm->Update();

  std::cout << "Sample mean = " << meanAlgorithm->GetMean() << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The covariance calculation algorithm will also calculate the mean while
  // performing the covariance matrix calculation. The mean can be accessed
  // using the \code{GetMean()} method while the covariance can be accessed
  // using the \code{GetCovarianceMatrix()} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::CovarianceSampleFilter< SampleType >
    CovarianceAlgorithmType;
  CovarianceAlgorithmType::Pointer covarianceAlgorithm =
    CovarianceAlgorithmType::New();

  covarianceAlgorithm->SetInput( sample );
  covarianceAlgorithm->Update();

  std::cout << "Mean = " << std::endl;
  std::cout << covarianceAlgorithm->GetMean() << std::endl;

  std::cout << "Covariance = " << std::endl;
  std::cout << covarianceAlgorithm->GetCovarianceMatrix() << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
