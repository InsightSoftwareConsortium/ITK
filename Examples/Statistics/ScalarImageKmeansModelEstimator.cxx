/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ScalarImageKmeansModelEstimator.cxx
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

//  Software Guide : BeginCommandLineArgs
//    INPUTS: {BrainT1Slice.png}
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// This example shows how to compute the KMeans model of an Scalar Image.
//
// The  \subdoxygen{Statistics}{KdTreeBasedKmeansEstimator} is used for taking
// a scalar image and applying the K-Means algorithm in order to define classes
// that represents statistical distributions of intensity values in the pixels.
// In the context of Medical Imaging, each class is typically associated to a
// particular type of tissue and can therefore be used as a form of image
// segmentation. One of the drawbacks fo this technique is that the spatial
// distribution of the pixels is not considered at all. It is commen therefore
// to combine the classification resulting from K-Means with other segmentation
// techniques that will use the classification as a prior and add spatial
// information to it in order to produce a better segmentation.
//
// Software Guide : EndLatex 


#include "itkKdTree.h"
#include "itkKdTreeBasedKmeansEstimator.h"
#include "itkWeightedCentroidKdTreeGenerator.h"

#include "itkMinimumDecisionRule.h"
#include "itkEuclideanDistance.h"
#include "itkSampleClassifier.h"

#include "itkScalarImageToListAdaptor.h"

#include "itkImage.h"
#include "itkImageFileReader.h"

int main( int argc, char * argv [] )
{

  if( argc < 2 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  " << argv[0] << "  inputImageFileName " << std::endl;
    return -1;
    }


  typedef unsigned char       PixelType;
  const unsigned int          Dimension = 2;

  typedef itk::Image<PixelType, Dimension > ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem encoutered while reading image file : " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return -1;
    }



  // Software Guide : BeginCodeSnippet
  
  // Create a List from the scalar image
  typedef itk::Statistics::ScalarImageToListAdaptor< ImageType >   AdaptorType;

  AdaptorType::Pointer adaptor = AdaptorType::New();

  adaptor->SetImage(  reader->GetOutput() );



  // Define the Measurement vector type from the AdaptorType
  typedef AdaptorType::MeasurementVectorType  MeasurementVectorType;


  // Create the K-d tree structure
  typedef itk::Statistics::WeightedCentroidKdTreeGenerator< 
                                                      AdaptorType > 
                                                              TreeGeneratorType;

  TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New();

  treeGenerator->SetSample( adaptor );
  treeGenerator->SetBucketSize( 16 );
  treeGenerator->Update();



  typedef TreeGeneratorType::KdTreeType TreeType;
  typedef itk::Statistics::KdTreeBasedKmeansEstimator<TreeType> EstimatorType;

  EstimatorType::Pointer estimator = EstimatorType::New();

  const unsigned int numberOfClasses = 3;

  EstimatorType::ParametersType initialMeans( numberOfClasses );
  initialMeans[0] = 25.0;   
  initialMeans[1] = 125.0;  
  initialMeans[2] = 250.0;  

  estimator->SetParameters( initialMeans );
  
  estimator->SetKdTree( treeGenerator->GetOutput() );
  estimator->SetMaximumIteration( 200 );
  estimator->SetCentroidPositionChangesThreshold(0.0);
  estimator->StartOptimization();

  EstimatorType::ParametersType estimatedMeans = estimator->GetParameters();

  for ( unsigned int i = 0 ; i < numberOfClasses ; ++i )
    {
    std::cout << "cluster[" << i << "] " << std::endl;
    std::cout << "    estimated mean : " << estimatedMeans[i] << std::endl;
    }

// Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  // \begin{figure} \center
  // \includegraphics[width=0.44\textwidth]{BrainT1Slice.eps}
  // \itkcaption[Output of the ScalarImageKmeansModelEstimator]{Test image for the
  // KMeans model estimator.}
  // \label{fig:ScalarImageKmeansModelEstimatorTestImage}
  // \end{figure}
  //
  //  The example produces means of 14.8, 91.6, 134.9 on 
  // Figure \ref{fig:ScalarImageKmeansModelEstimatorTestImage}
  //
  //  Software Guide : EndLatex 


  return EXIT_SUCCESS;
  
}


