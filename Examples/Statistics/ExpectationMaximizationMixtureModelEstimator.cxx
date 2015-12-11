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
// \index{Statistics!Mixture model estimation}
// \index{Statistics!Expectation maximization}
//
// \index{itk::Statistics::Gaussian\-Mixture\-Model\-Component}
// \index{itk::Statistics::Expectation\-Maximization\-Mixture\-Model\-Estimator}
//
// In this example, we present an implementation of the expectation
// maximization (EM) process to generates parameter estimates for a two
// Gaussian component mixture model.
//
// The Bayesian plug-in classifier example (see Section
// \ref{sec:BayesianPluginClassifier}) used two Gaussian probability
// density functions (PDF) to model two Gaussian distribution classes (two
// models for two class). However, in some cases, we want to model a
// distribution as a mixture of several different
// distributions. Therefore, the probability density function ($p(x)$)
// of a mixture model can be stated as follows :
//
// \begin{equation}
//   p(x) = \sum^{c}_{i=0}\alpha_{i}f_{i}(x)
// \end{equation}
// where $i$ is the index of the component,
//       $c$ is the number of components,
//       $\alpha_{i}$ is the proportion of the component,
//       and $f_{i}$ is the probability density function of the component.
//
// Now the task is to find the parameters(the component PDF's
// parameters and the proportion values) to maximize the likelihood of
// the parameters. If we know which component a measurement vector
// belongs to, the solutions to this problem is easy to solve.
// However, we don't know the membership of each measurement
// vector. Therefore, we use the expectation of membership instead of
// the exact membership. The EM process splits into two steps:
// \begin{enumerate}
//    \item{ E step: calculate the expected membership values for each
// measurement vector to each classes.}
//    \item{ M step: find the next parameter sets that maximize the
// likelihood with the expected membership values and the current set of
// parameters.}
// \end{enumerate}
//
// The E step is basically a step that calculates the \emph{a posteriori}
// probability for each measurement vector.
//
// The M step is dependent on the type of each PDF. Most of
// distributions belonging to exponential family such as Poisson,
// Binomial, Exponential, and Normal distributions have analytical
// solutions for updating the parameter set. The
// \subdoxygen{Statistics}{ExpectationMaximizationMixtureModelEstimator}
// class assumes that such type of components.
//
// In the following example we use the \subdoxygen{Statistics}{ListSample} as
// the sample (test and training). The \subdoxygen{Vector} is our measurement
// vector class. To store measurement vectors into two separate sample
// container, we use the \subdoxygen{Statistics}{Subsample} objects.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
#include "itkListSample.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The following two files provides us the parameter estimation algorithms.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkGaussianMixtureModelComponent.h"
#include "itkExpectationMaximizationMixtureModelEstimator.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We will fill the sample with random variables from two normal
// distribution using the \subdoxygen{Statistics}{NormalVariateGenerator}.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkNormalVariateGenerator.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // Since the NormalVariateGenerator class only supports 1-D, we
  // define our measurement vector type as a one component vector. We
  // then, create a ListSample object for data inputs.
  //
  // We also create two Subsample objects that will store the
  // measurement vectors in the \code{sample} into two separate sample
  // containers. Each Subsample object stores only the
  // measurement vectors belonging to a single class. This
  // \textit{class sample} will be used by the parameter estimation
  // algorithms.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  unsigned int numberOfClasses = 2;
  typedef itk::Vector< double, 1 > MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;
  SampleType::Pointer sample = SampleType::New();
  sample->SetMeasurementVectorSize( 1 ); // length of measurement vectors
                                         // in the sample.
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The following code snippet creates a NormalVariateGenerator object.
  // Since the random variable generator returns values according to the
  // standard normal distribution (the mean is zero, and the standard
  // deviation is one) before pushing random values into the \code{sample},
  // we change the mean and standard deviation. We want two normal (Gaussian)
  // distribution data. We have two for loops. Each for loop uses different
  // mean and standard deviation. Before we fill the \code{sample} with the
  // second distribution data, we call \code{Initialize()} method to recreate
  // the pool of random variables in the \code{normalGenerator}. In the
  // second for loop, we fill the two class samples with measurement vectors
  // using the \code{AddInstance()} method.
  //
  // To see the probability density plots from the two distribution,
  // refer to Figure~\ref{fig:TwoNormalDensityFunctionPlot}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::NormalVariateGenerator NormalGeneratorType;
  NormalGeneratorType::Pointer normalGenerator = NormalGeneratorType::New();

  normalGenerator->Initialize( 101 );

  MeasurementVectorType mv;
  double mean = 100;
  double standardDeviation = 30;
  for ( unsigned int i = 0; i < 100; ++i )
    {
    mv[0] = ( normalGenerator->GetVariate() * standardDeviation ) + mean;
    sample->PushBack( mv );
    }

  normalGenerator->Initialize( 3024 );
  mean = 200;
  standardDeviation = 30;
  for ( unsigned int i = 0; i < 100; ++i )
    {
    mv[0] = ( normalGenerator->GetVariate() * standardDeviation ) + mean;
    sample->PushBack( mv );
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // In the following code snippet notice that the template argument
  // for the MeanCalculator and CovarianceCalculator is
  // \code{ClassSampleType} (i.e., type of Subsample) instead of
  // \code{SampleType} (i.e., type of ListSample). This is because the
  // parameter estimation algorithms are applied to the class sample.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Array< double > ParametersType;
  ParametersType params( 2 );

  std::vector< ParametersType > initialParameters( numberOfClasses );
  params[0] = 110.0;
  params[1] = 800.0;
  initialParameters[0] = params;

  params[0] = 210.0;
  params[1] = 850.0;
  initialParameters[1] = params;

  typedef itk::Statistics::GaussianMixtureModelComponent< SampleType >
    ComponentType;

  std::vector< ComponentType::Pointer > components;
  for ( unsigned int i = 0; i < numberOfClasses; i++ )
    {
    components.push_back( ComponentType::New() );
    (components[i])->SetSample( sample );
    (components[i])->SetParameters( initialParameters[i] );
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We run the estimator.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::ExpectationMaximizationMixtureModelEstimator<
                           SampleType > EstimatorType;
  EstimatorType::Pointer estimator = EstimatorType::New();

  estimator->SetSample( sample );
  estimator->SetMaximumIteration( 200 );

  itk::Array< double > initialProportions(numberOfClasses);
  initialProportions[0] = 0.5;
  initialProportions[1] = 0.5;

  estimator->SetInitialProportions( initialProportions );

  for (unsigned int i = 0; i < numberOfClasses; ++i)
    {
    estimator->AddComponent( (ComponentType::Superclass*)
                             (components[i]).GetPointer() );
    }

  estimator->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We then print out the estimated parameters.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  for (unsigned int i = 0; i < numberOfClasses; ++i)
    {
    std::cout << "Cluster[" << i << "]" << std::endl;
    std::cout << "    Parameters:" << std::endl;
    std::cout << "         " << (components[i])->GetFullParameters()
              << std::endl;
    std::cout << "    Proportion: ";
    std::cout << "         " << estimator->GetProportions()[i] << std::endl;
    }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
