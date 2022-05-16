/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
// \index{itk::Statistics::Weighted\-Mean\-Calculator}
// \index{itk::Statistics::Weighted\-Covariance\-Calculator}
// \index{Statistics!Weighted mean}
// \index{Statistics!Weighted covariance}
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
// The following headers are for the weighted covariance algorithms.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkWeightedMeanSampleFilter.h"
#include "itkWeightedCovarianceSampleFilter.h"
// Software Guide : EndCodeSnippet

using MeasurementVectorType = itk::Vector<float, 3>;

class ExampleWeightFunction
  : public itk::FunctionBase<MeasurementVectorType, double>
{
public:
  /** Standard class type aliases. */
  using Self = ExampleWeightFunction;
  using Superclass = itk::FunctionBase<MeasurementVectorType, double>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Standard macros. */
  itkTypeMacro(ExampleWeightFunction, FunctionBase);
  itkNewMacro(Self);

  /** Input type */
  using InputType = MeasurementVectorType;

  /** Output type */
  using OutputType = double;

  /**Evaluate at the specified input position */
  OutputType
  Evaluate(const InputType & input) const override
  {
    if (input[0] < 3.0)
    {
      return 0.5;
    }
    else
    {
      return 0.01;
    }
  }

protected:
  ExampleWeightFunction() = default;
  ~ExampleWeightFunction() override = default;
}; // end of class

int
main()
{
  // Software Guide : BeginLatex
  //
  // The following code snippet will create a ListSample instance
  // with three-component float measurement vectors and put five
  // measurement vectors in the ListSample object.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using SampleType = itk::Statistics::ListSample<MeasurementVectorType>;
  auto sample = SampleType::New();
  sample->SetMeasurementVectorSize(3);
  MeasurementVectorType mv;
  mv[0] = 1.0;
  mv[1] = 2.0;
  mv[2] = 4.0;

  sample->PushBack(mv);

  mv[0] = 2.0;
  mv[1] = 4.0;
  mv[2] = 5.0;
  sample->PushBack(mv);

  mv[0] = 3.0;
  mv[1] = 8.0;
  mv[2] = 6.0;
  sample->PushBack(mv);

  mv[0] = 2.0;
  mv[1] = 7.0;
  mv[2] = 4.0;
  sample->PushBack(mv);

  mv[0] = 3.0;
  mv[1] = 2.0;
  mv[2] = 7.0;
  sample->PushBack(mv);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Robust versions of covariance algorithms require
  // weight values for measurement vectors. We have two ways of
  // providing weight values for the weighted mean and weighted
  // covariance algorithms.
  //
  // The first method is to plug in an array of weight values. The
  // size of the weight value array should be equal to that of the
  // measurement vectors. In both algorithms, we use the
  // \code{SetWeights(weights)}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using WeightedMeanAlgorithmType =
    itk::Statistics::WeightedMeanSampleFilter<SampleType>;

  WeightedMeanAlgorithmType::WeightArrayType weightArray(sample->Size());
  weightArray.Fill(0.5);
  weightArray[2] = 0.01;
  weightArray[4] = 0.01;

  auto weightedMeanAlgorithm = WeightedMeanAlgorithmType::New();

  weightedMeanAlgorithm->SetInput(sample);
  weightedMeanAlgorithm->SetWeights(weightArray);
  weightedMeanAlgorithm->Update();

  std::cout << "Sample weighted mean = " << weightedMeanAlgorithm->GetMean()
            << std::endl;

  using WeightedCovarianceAlgorithmType =
    itk::Statistics::WeightedCovarianceSampleFilter<SampleType>;

  auto weightedCovarianceAlgorithm = WeightedCovarianceAlgorithmType::New();

  weightedCovarianceAlgorithm->SetInput(sample);
  weightedCovarianceAlgorithm->SetWeights(weightArray);
  weightedCovarianceAlgorithm->Update();

  std::cout << "Sample weighted covariance = " << std::endl;
  std::cout << weightedCovarianceAlgorithm->GetCovarianceMatrix()
            << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The second method for computing weighted statistics is to plug-in a
  // function that returns a weight value that is usually a function of each
  // measurement vector. Since the \code{weightedMeanAlgorithm} and
  // \code{weightedCovarianceAlgorithm} already have the input sample plugged
  // in, we only need to call the \code{SetWeightingFunction(weights)}
  // method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  auto weightFunction = ExampleWeightFunction::New();

  weightedMeanAlgorithm->SetWeightingFunction(weightFunction);
  weightedMeanAlgorithm->Update();

  std::cout << "Sample weighted mean = " << weightedMeanAlgorithm->GetMean()
            << std::endl;

  weightedCovarianceAlgorithm->SetWeightingFunction(weightFunction);
  weightedCovarianceAlgorithm->Update();

  std::cout << "Sample weighted covariance = " << std::endl;
  std::cout << weightedCovarianceAlgorithm->GetCovarianceMatrix();

  std::cout << "Sample weighted mean (from WeightedCovarainceSampleFilter) = "
            << std::endl
            << weightedCovarianceAlgorithm->GetMean() << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
