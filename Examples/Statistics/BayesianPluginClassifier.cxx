/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    BayesianPluginClassifier.cxx
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
// \index{Statistics!Bayesian plugin classifier|textbf}
// \index{itk::Statistics::SampleClassifier|textbf}
// \index{itk::Statistics::GaussianDensityFunction|textbf}
// \index{itk::Statistics::NormalVariateGenerator|textbf}
//
// In this example, we present a classifier system that classifies
// measurement vectors into the two Gaussian classes. The figure
// \ref{fig:BayesianPluginClassifier} shows all the components of the
// classifier system and the data flow. This system contrasts with the
// previous k-means clustering in several points. The biggest difference
// is that this classifier uses the
// \subdoxygen{Statistics}{GaussianDensityFunction}s as memberhip
// functions instead of the
// \subdoxygen{Statistics}{EuclideanDistance}. Since the membership
// function is different, the membership function requires a different
// set of parameters, mean vector and covariance matrix. We choose the
// \subdoxygen{Statistics}{MeanCalculator} (sample mean) and the
// \subdoxygen{Statistics}{CovarianceCalculator} (sample covariance) for
// the estimation algorithms of the two parameters. If we want more
// robust estimation algorithm, we can replace these estimation
// algorithms with more robust ones without change other components in
// the classifier system. 
// 
// It is a bad idea to use the same sample for test and training
// (parameter estimation) of the parameters. However, for simplicity, in
// this example, we use a sample for test and training.
//
// \begin{figure}
//   \centering
//   \includegraphics[width=0.7\textwidth]{BayesianPluginClassifier.eps}
//   \itkcaption[Bayesian plug-in classifier for two Gaussian classes]{Bayesian
//   plug-in classifier for two Gaussian classes}
//  \protect\label{fig:BayesianPluginClassifier}
// \end{figure}
//
// We use the \subdoxygen{Statistics}{ListSample} as the sample (test
// and training). The \doxygen{Vector} is our measurement vector
// class. To store measurement vectors into two separate sample
// containers, we use the \subdoxygen{Statistics}{Subsample} objects. 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
#include "itkListSample.h"
#include "itkSubsample.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// The following two files provides us the parameter estimation algorithms.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkMeanCalculator.h"
#include "itkCovarianceCalculator.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// The following files define the components required by ITK statistical
// classification framework: the decision rule, the membership
// function, and the classifier. 
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkMaximumRatioDecisionRule.h"
#include "itkGaussianDensityFunction.h"
#include "itkSampleClassifier.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// We will fill the sample with random variables from two normal 
// distribution using the \subdoxygen{Statistics}{NormalVariateGenerator}.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkNormalVariateGenerator.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  // Since the \code{NormalVariateGenerator} class only supports 1-D, we
  // define our measurement vector type as a one component vector. We
  // then, create a \code{ListSample} object for data inputs.
  // 
  // We also create two \code{Subsample} objects that will store
  // the measurement vectors in the \code{sample} into two separate
  // sample containers. Each \code{Subsample} object stores only the
  // measurement vectors belonging to a single class. This class sample
  // will be used by the parameter estimation algorithms.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Vector< double, 1 > MeasurementVectorType ;
  
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;
  SampleType::Pointer sample = SampleType::New() ;

  typedef itk::Statistics::Subsample< SampleType > ClassSampleType ;
  std::vector< ClassSampleType::Pointer > classSamples ;
  for ( unsigned int i = 0 ; i < 2 ; ++i )
    {
    classSamples.push_back( ClassSampleType::New() ) ;
    classSamples[i]->SetSample( sample ) ;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The following code snippet creates a \code{NormalVaraiteGenerator}
  // object. Since the random variable generator returns values
  // according to the standard normal distribution (The mean is zero,
  // and the standard deviation is one), before pushing random values
  // into the \code{sample}, we change the mean and standard
  // deviation. We want two normal (Gaussian) distribution data. We have
  // two for loops. Each for loop uses different mean and standard
  // deviation. Before we fill the \code{sample} with the second
  // distribution data, we call \code{Initialize( random seed )} method,
  // to recreates the pool of random variables in the
  // \code{normalGenerator}. In the two for loop, we fill the two class
  // samples with measurement vectors using the \code{AddInstance} method.
  //
  // To see the probability density plots from the two distribution,
  // refer to the figure \ref{fig:TwoNormalDensityFunctionPlot}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::NormalVariateGenerator NormalGeneratorType ;
  NormalGeneratorType::Pointer normalGenerator = NormalGeneratorType::New() ;

  normalGenerator->Initialize( 101 ) ;

  MeasurementVectorType mv ;
  double mean = 100 ;
  double standardDeviation = 30 ;
  SampleType::InstanceIdentifier id = 0UL ;
  for ( unsigned int i = 0 ; i < 100 ; ++i )
    {
    mv = ( normalGenerator->GetVariate() * standardDeviation ) + mean ;
    sample->PushBack( mv ) ;
    classSamples[0]->AddInstance( id ) ;
    ++id ;
    }

  normalGenerator->Initialize( 3024 ) ;
  mean = 200 ;
  standardDeviation = 30 ;
  for ( unsigned int i = 0 ; i < 100 ; ++i )
    {
    mv = ( normalGenerator->GetVariate() * standardDeviation ) + mean ;
    sample->PushBack( mv ) ;
    classSamples[1]->AddInstance( id ) ;
    ++id ;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // In the following code snippet, notice that the template argument
  // for the \code{MeanCalculator} and \code{CovarianceCalculator} is
  // \code{ClassSampleType} that is \code{Subsample} instead of
  // \code{SampleType} that is \code{ListSample}. It is because the
  // parameter estimation algorithms are applied to the class sample. 
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::MeanCalculator< ClassSampleType > 
    MeanEstimatorType ;

  typedef itk::Statistics::CovarianceCalculator< ClassSampleType >
    CovarianceEstimatorType ;
  
  std::vector< MeanEstimatorType::Pointer > meanEstimators ;
  std::vector< CovarianceEstimatorType::Pointer > covarianceEstimators ;

  for ( unsigned int i = 0 ; i < 2 ; ++i )
    {
    meanEstimators.push_back( MeanEstimatorType::New() ) ;
    meanEstimators[i]->SetInputSample( classSamples[i] ) ;
    meanEstimators[i]->Update() ;
    
    covarianceEstimators.push_back( CovarianceEstimatorType::New() ) ;
    covarianceEstimators[i]->SetInputSample( classSamples[i] ) ;
    covarianceEstimators[i]->SetMean( meanEstimators[i]->GetOutput() ) ;
    covarianceEstimators[i]->Update() ;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We print out the estimated parameters.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  for ( unsigned int i = 0 ; i < 2 ; ++i )
    {
    std::cout << "class[" << i << "] " << std::endl ;
    std::cout << "    estimated mean : " 
              << *(meanEstimators[i]->GetOutput()) 
              << "    covariance matrix : " 
              << *(covarianceEstimators[i]->GetOutput()) << std::endl ;
    }
  // Software Guide : EndCodeSnippet
  
  // Software Guide : BeginLatex
  // After creating a \code{SampleClassifier} object and a
  // \code{MaximumRatioDecisionRule} object, we plug-in the
  // \code{decisionRule} and the \code{sample} to the classifier. Then,
  // we must specify the number of classes that will be considered using
  // the \code{SetNumberOfClasses(unsigned int)} method. 
  //
  // The \code{MaximumRatioDecisionRule} requires a vector of \textit{a
  // priori} probability values. Such \textit{a priori} probability will
  // be the $P(\omega_{i})$ of the following variation of the Bayes
  // decision rule: 
  // \begin{equation}
  //   \textrm{Decide } \omega_{i} \textrm{ if }
  //   \frac{p(\overrightarrow{x}|\omega_{i})}
  //        {p(\overrightarrow{x}|\omega_{j})}
  // > \frac{P(\omega_{j})}{P(\omega_{i})} \textrm{ for all } j \not= i
  //   \label{eq:bayes2}
  // \end{equation}
  // 
  // The rest of the following code snippet shows how to use
  // user-specififed class labels. The classification result will be
  // stored in a \code{MembershipSample} object, and for each
  // measurement vector, its class label will be one of the two class
  // labels, 100 and 200 (\code{unsigned int}).
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::GaussianDensityFunction< MeasurementVectorType >
    MembershipFunctionType ;
  
  typedef itk::MaximumRatioDecisionRule DecisionRuleType ;
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New() ;

  DecisionRuleType::APrioriVectorType aPrioris ;
  aPrioris.push_back( classSamples[0]->GetTotalFrequency() 
                      / sample->GetTotalFrequency() ) ; 
  aPrioris.push_back( classSamples[1]->GetTotalFrequency() 
                      / sample->GetTotalFrequency() ) ; 
  decisionRule->SetAPriori( aPrioris ) ;

  typedef itk::Statistics::SampleClassifier< SampleType > ClassifierType ;
  ClassifierType::Pointer classifier = ClassifierType::New() ;

  classifier->SetDecisionRule( (itk::DecisionRuleBase::Pointer) decisionRule) ;
  classifier->SetSample( sample ) ;
  classifier->SetNumberOfClasses( 2 ) ;

  std::vector< unsigned int > classLabels ;
  classLabels.resize( 2 ) ;
  classLabels[0] = 100 ;
  classLabels[1] = 200 ;
  classifier->SetMembershipFunctionClassLabels(classLabels) ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The \code{classifier} is almost ready to do the classification
  // process except that it needs two membership functions that
  // represents the two clusters.
  //
  // In this example, we can think the two clusters are modeled by two
  // Euclidean distance funtions. The distance function (model) has only
  // one parameter, its mean (centroid) set by the \code{SetOrigin}
  // method. To plug-in two distance functions, we call the
  // \code{AddMembershipFunction} method. Then the \code{update()}
  // method call will do the classification.
  // 
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::vector< MembershipFunctionType::Pointer > membershipFunctions ;
  for ( unsigned int i = 0 ; i < 2 ; i++ ) 
    {
    membershipFunctions.push_back(MembershipFunctionType::New()) ;
    membershipFunctions[i]->SetMean( meanEstimators[i]->GetOutput() ) ;
    membershipFunctions[i]->
      SetCovariance( covarianceEstimators[i]->GetOutput() ) ;
    classifier->AddMembershipFunction(membershipFunctions[i].GetPointer()) ;
    }

  classifier->Update() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The following code snippet prints out pairs of a measurement vector and
  // its class label in the \code{sample}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ClassifierType::OutputType* membershipSample = classifier->GetOutput() ;
  ClassifierType::OutputType::Iterator iter = membershipSample->Begin() ;

  while ( iter != membershipSample->End() )
    {
    std::cout << "measurement vector = " << iter.GetMeasurementVector()
              << "class label = " << iter.GetClassLabel()
              << std::endl ;
    ++iter ;
    }
  // Software Guide : EndCodeSnippet
  return 0 ;
}







