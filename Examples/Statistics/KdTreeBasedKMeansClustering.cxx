/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    KdTreeBasedKMeansClustering.cxx
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
// \index{Statistics!k-means clustering (using k-d tree)|textbf}
//
// \index{itk::Statistics::KdTreeBasedKmeansEstimator|textbf}
//
// The k-means clustering is popular among clustering algorithms
// because it is simple and usually converges to a reasonable
// solution. The k-means algorithm works as follows:
//
// \begin{enumerate}
//   \item{gets the initial k means input from the user.} 
//   \item{assigns each measurement vector in a sample container to its
// closest mean among the k number of means. (i.e. update the membership of
// each measurement vectors to the nearest of the k clusters)}  
//   \item{calculate each cluster's mean from the newly assigned
// measurement vectors. (updates the centeroid (mean) of k clusters)}
//   \item{repeats the step 2 and step 3 until it meets the termination
// criteria.}
// \end{enumerate}
//
// The most common termination criteria is that if there is no
// measurement vector that changes its cluster membership from the
// previous iteration, then stop.
//
// The \subdoxygen{Statistics}{KdTreeBasedKmeansEstimator} is a
// variation of this logic. The k-means clustering algorithm is
// computationally very expensive because it has to recalculate the
// mean at each iteration. To update the mean values, we have to
// calculate the distance between k means and each and every measurement
// vectors. To reduce such distance calculation, the
// \code{KdTreeBasedKmeansEstimator} uses a special data structure:
// the k-d tree (\subdoxygen{Statistics}{KdTree}) with additional
// information. The additional information includes the number and the
// vector sum of measurement vectors under each node under the tree
// architecture.
//
// With such additional information and the k-d tree data structure,
// we can reduce the computational cost of the distance calculation
// and means. Instead of calculating each measurement vectors and k
// means, we can simply compare each node of the k-d tree and the k
// means. This idea of utilizing a k-d tree can be found in multiple
// articles \cite{Alsabti1998} \cite{Pelleg1999}
// \cite{Kanungo2000}. Our implementation of this scheme follows the
// article by the Kanungo et al \cite{Kanungo2000}.
//
// We use the \subdoxygen{Statistics}{ListSample} as the input sample, the
// \doxygen{Vector} as the measurement vector. The following code
// snippet includes them.
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
#include "itkListSample.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// Since our k-means algorithm requires a \code{KdTree} object as an
// input, we include the \code{KdTree} class header file. As mentioned
// above, we need a k-d tree with the vector sum and the number of
// measurement vectors. Therefore we uses the
// \subdoxygen{Statistics}{WeightedCenteroidKdTreeGenerator} instead of
// the \subdoxygen{Statistics}{KdTreeGenerator} that generate a k-d tree
// without such additional information. 
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkKdTree.h"
#include "itkWeightedCenteroidKdTreeGenerator.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// The \code{KdTreeBasedKmeansEstimator} class is the implementation of the
// k-means algorithm. It does not create k clusters. Instead, it
// returns the mean estimates for the k clusters.
// the k mean values. // Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkKdTreeBasedKmeansEstimator.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// To generate the clusters, we must create k instances of
// \subdoxygen{Statistaics}{EuclideanDistance} function as the
// membership functions for each cluster and plug that with a sample
// into an \subdoxygen{Statistics}{SampleClassifier} object to get a
// \subdoxygen{Statistics}{MembershipSample} that stores pairs of
// measurement vectors and their associated class labels (k labels).
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkMinimumDecisionRule.h"
#include "itkEuclideanDistance.h"
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
  // define our measurement vector type as one component vector. We
  // then, create a \code{ListSample} object for data inputs.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Vector< double, 1 > MeasurementVectorType ;
  
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;
  SampleType::Pointer sample = SampleType::New() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The following code snippet creates a \code{NormalVariateGenerator}
  // object. Since the random variable generator returns values
  // according to the standard normal distribution (The mean is zero,
  // and the standard deviation is one), before pushing random values
  // into the \code{sample}, we change the mean and standard
  // deviation. We want two normal (Gaussian) distribution data. We have
  // two for loops. Each for loop uses different mean and standard
  // deviation. Before we fill the \code{sample} with the second
  // distribution data, we call \code{Initialize( random seed )} method,
  // to recreate the pool of random variables in the
  // \code{normalGenerator}.
  //
  // To see the probability density plots from the two distribution,
  // refer to the figure \ref{fig:TwoNormalDensityFunctionPlot}.
  //
  // \begin{figure} 
  //   \center
  //   \includegraphics[width=0.8\textwidth]{TwoNormalDensityFunctionPlot.eps}
  //   \itkcaption[Two normal distributions plot]{Two normal distributions' probability density plot
  // (The means are 100 and 200, and the standard deviation is 30 )}
  //  \protect\label{fig:TwoNormalDensityFunctionPlot}
  // \end{figure}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::NormalVariateGenerator NormalGeneratorType ;
  NormalGeneratorType::Pointer normalGenerator = NormalGeneratorType::New() ;

  normalGenerator->Initialize( 101 ) ;

  MeasurementVectorType mv ;
  double mean = 100 ;
  double standardDeviation = 30 ;
  for ( unsigned int i = 0 ; i < 100 ; ++i )
    {
    mv = ( normalGenerator->GetVariate() * standardDeviation ) + mean ;
    sample->PushBack( mv ) ;
    }

  normalGenerator->Initialize( 3024 ) ;
  mean = 200 ;
  standardDeviation = 30 ;
  for ( unsigned int i = 0 ; i < 100 ; ++i )
    {
    mv = ( normalGenerator->GetVariate() * standardDeviation ) + mean ;
    sample->PushBack( mv ) ;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We create a k-d tree. To see the details on the k-d tree
  // generation, see the section \ref{sec:KdTree}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::WeightedCenteroidKdTreeGenerator< SampleType > 
    TreeGeneratorType ;
  TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New() ;
  
  treeGenerator->SetSample( sample ) ;
  treeGenerator->SetBucketSize( 16 ) ;
  treeGenerator->Update() ;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  // Once we have the k-d tree, it is a simple procedure to produce k
  // mean estimates. 
  //
  // We create the \code{KdTreeBasedKemansEstimator}. Then, we
  // provide the initial mean values using the \code{SetParameters(
  // array of mean values )}. Since we are dealing with two normal
  // distribution in a 1-D space, the size of the mean value array is
  // two. The first element is the first mean value, and the second is
  // the second mean value. If we used two normal in a 2-D space, the
  // size of array should be four, and the first two elements are the
  // two components of the first normal distribution's mean vector. We
  // plug-in the k-d tree using the \code{SetKdTree( k-d tree * )}.
  //
  // The rest two methods specify the termination condition. The
  // estimation process stops when the nubmer of iteration reaches the
  // maximum iteration value set by the \code{SetMaximumIteration(
  // unsigned int)}, or the distances between the newly calculated mean
  // (centeroid) values and previous ones are within the threshold set
  // by the \code{SetCenteroidPositionChangesThreshold( double )}. The
  // final step is to call \code{StartOptimization()} method.
  //
  // The for loop will print out the mean estimates from the estimation
  // process. 
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef TreeGeneratorType::KdTreeType TreeType ;
  typedef itk::Statistics::KdTreeBasedKmeansEstimator< TreeType > 
    EstimatorType ;
  EstimatorType::Pointer estimator = EstimatorType::New() ;

  EstimatorType::ParametersType initialMeans(2) ;
  initialMeans[0] = 0.0 ;
  initialMeans[1] = 0.0 ;

  estimator->SetParameters( initialMeans ) ;
  estimator->SetKdTree( treeGenerator->GetOutput() ) ;
  estimator->SetMaximumIteration( 200 ) ;
  estimator->SetCenteroidPositionChangesThreshold(0.0) ;
  estimator->StartOptimization() ;

  EstimatorType::ParametersType estimatedMeans = estimator->GetParameters() ;

  for ( unsigned int i = 0 ; i < 2 ; ++i )
    {
    std::cout << "cluster[" << i << "] " << std::endl ;
    std::cout << "    estimated mean : " 
              << estimatedMeans[i] << std::endl ;
    }
  // Software Guide : EndCodeSnippet
  
  // Software Guide : BeginLatex
  // If we are only interested in finding the mean estimates, we might
  // stop. However, to illustrate how a classifier can be formed using
  // the statistical classification framework. We go a little bit
  // further in this example.
  //
  // Since the k-means algorithm is an minimum distance classifier using
  // the estimated k means and the measurement vectors. We use the
  // \code{EuclideanDistance} class as membership functions. Our choide
  // for the decision rule is the
  // \subdoxygen{Statistics}{MinimumDecisionRule} which returns the
  // index of the membership functions that have the smallest value for
  // a measurement vector.
  //
  // After creating a \code{SampleClassifier} object and a
  // \code{MinimumDecisionRule} object, we plug-in the
  // \code{decisionRule} and the \code{sample} to the classifier. Then,
  // we must specify the number of classes that will be considered using
  // the \code{SetNumberOfClasses(unsigned int)} method. 
  //
  // The rest of the following code snippet shows how to use
  // user-specififed class labels. The classification result will be
  // stored in a \code{MembershipSample} object, and for each
  // measurement vector, its class label will be one of the two class
  // labels, 100 and 200 (\code{unsigned int}).
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::EuclideanDistance< MeasurementVectorType >
    MembershipFunctionType ;
  
  typedef itk::MinimumDecisionRule DecisionRuleType ;
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New() ;
  
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
  // represents two clusters resprectively.
  //
  // In this example, the two clusters are modeled by two Euclidean
  // distance funtions. The distance function (model) has only one
  // parameter, its mean (centeroid) set by the \code{SetOrigin}
  // method. To plug-in two distance functions, we call the
  // \code{AddMembershipFunction} method. Then the \code{update()}
  // method call will do the classification.
  // 
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::vector< MembershipFunctionType::Pointer > membershipFunctions ;
  MembershipFunctionType::OriginType origin ;
  int index = 0 ;
  for ( unsigned int i = 0 ; i < 2 ; i++ ) 
    {
    membershipFunctions.push_back(MembershipFunctionType::New()) ;
    for ( unsigned int j = 0 ; j < SampleType::MeasurementVectorSize ; j++ )
      {
      origin[j] = estimatedMeans[index++] ;
      }
    membershipFunctions[i]->SetOrigin(origin) ;
    classifier->AddMembershipFunction(membershipFunctions[i].GetPointer()) ;
    }

  classifier->Update() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The following code snippet prints out the measurement vectors and
  // thier class labels in the \code{sample}.
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







