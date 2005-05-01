/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ScalarImageMarkovRandomField1.cxx
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
//    INPUTS: {BrainT1Slice.png}, {BrainT1Slice_labelled.png}
//    OUTPUTS: {ScalarImageMarkovRandomField1Output.png}
//    50 3 3 14.8 91.6 134.9
//    NORMALIZE_EPS_OUTPUT_OF: {ScalarImageMarkovRandomField1Output.png}
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// This example shows how to use the Markov Random Field approach for
// classifying the pixel of a scalar image.
//
// The  \subdoxygen{Statistics}{MRFImageFilter} is used for refining an initial
// classification by introducing the spatial coherence of the labels. The user
// should provide two images as input. The first image is the one to be
// classified while the second image is an image of labels representing an
// initial classification.
//
// Software Guide : EndLatex 


// Software Guide : BeginLatex
//
// The following headers are related to reading input images, writing the
// output image, and making the necessary conversions between scalar and vector
// images.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkFixedArray.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkScalarToArrayCastImageFilter.h"
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// The following headers are related to the statistical classification classes.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkMRFImageFilter.h"
#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkMinimumDecisionRule.h"
#include "itkImageClassifierBase.h"
// Software Guide : EndCodeSnippet

int main( int argc, char * argv [] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0];
    std::cerr << " inputScalarImage inputLabeledImage";
    std::cerr << " outputLabeledImage numberOfIterations";
    std::cerr << " smoothingFactor numberOfClasses";
    std::cerr << " mean1 mean2 ... meanN " << std::endl;
    return EXIT_FAILURE;
    }

  const char * inputImageFileName      = argv[1];
  const char * inputLabelImageFileName = argv[2];
  const char * outputImageFileName     = argv[3];

  const unsigned int numberOfIterations = atoi( argv[4] );
  const double       smoothingFactor    = atof( argv[5] );
  const unsigned int numberOfClasses    = atoi( argv[6] );

  const unsigned int numberOfArgumentsBeforeMeans = 7;

  if( static_cast<unsigned int>(argc) <
      numberOfClasses + numberOfArgumentsBeforeMeans )
    {
    std::cerr << "Error: " << std::endl;
    std::cerr << numberOfClasses << " classes has been specified ";
    std::cerr << "but no enough means have been provided in the command ";
    std::cerr << "line arguments " << std::endl;
    return EXIT_FAILURE;
    }



// Software Guide : BeginLatex
//
// First we define the pixel type and dimension of the image that we intend to
// classify. With this image type we can also declare the
// \doxygen{ImageFileReader} needed for reading the input image, create one and
// set its input filename. In this particular case we choose to use
// \code{signed short} as pixel type, which is typical for MicroMRI and CT data
// sets.
//
// Software Guide : EndLatex 
  
// Software Guide : BeginCodeSnippet
  typedef signed short        PixelType;
  const unsigned int          Dimension = 2;

  typedef itk::Image<PixelType, Dimension > ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputImageFileName );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// As a second step we define the pixel type and dimension of the image of
// labels that provides the initial classification of the pixels from the first
// image. This initial labeled image can be the output of a K-Means method like
// the one illustrated in section \ref{sec:KMeansClassifier}.
//
// Software Guide : EndLatex 
  
// Software Guide : BeginCodeSnippet
  typedef unsigned char       LabelPixelType;

  typedef itk::Image<LabelPixelType, Dimension > LabelImageType;

  typedef itk::ImageFileReader< LabelImageType > LabelReaderType;
  LabelReaderType::Pointer labelReader = LabelReaderType::New();
  labelReader->SetFileName( inputLabelImageFileName );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// Since the Markov Random Field algorithm is defined in general for images
// whose pixels have multiple components, that is, images of vector type, we
// must adapt our scalar image in order to satisfy the interface expected by
// the \code{MRFImageFilter}. We do this by using the
// \doxygen{ScalarToArrayCastImageFilter}. With this filter we will present our
// scalar image as a vector image whose vector pixels contain a single
// component. 
// 
// Software Guide : EndLatex 
  
// Software Guide : BeginCodeSnippet
  typedef itk::FixedArray<LabelPixelType,1>  ArrayPixelType;

  typedef itk::Image< ArrayPixelType, Dimension > ArrayImageType;

  typedef itk::ScalarToArrayCastImageFilter< 
                     ImageType, ArrayImageType > ScalarToArrayFilterType;

  ScalarToArrayFilterType::Pointer 
    scalarToArrayFilter = ScalarToArrayFilterType::New();
  scalarToArrayFilter->SetInput( reader->GetOutput() );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// With the input image type \code{ImageType} and labeled image type
// \code{LabelImageType} we instantiate the type of the
// \doxygen{MRFImageFilter} that will apply the Markov Random Field algorithm
// in order to refine the pixel classification.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::MRFImageFilter< ArrayImageType, LabelImageType > MRFFilterType;

  MRFFilterType::Pointer mrfFilter = MRFFilterType::New();

  mrfFilter->SetInput( scalarToArrayFilter->GetOutput() );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// We set now some of the parameters for the MRF filter. In particular, the
// number of classes to be used during the classification, the maximum number
// of iterations to be run in this filter and the error tolerance that will be
// used as a criterion for convergence.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  mrfFilter->SetNumberOfClasses( numberOfClasses );
  mrfFilter->SetMaximumNumberOfIterations( numberOfIterations );
  mrfFilter->SetErrorTolerance( 1e-7 );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// The smoothing factor represents the tradeoff between fidelity to the 
// observed image and the smoothness of the segmented image. Typical smoothing
// factors have values between 1~5. This factor will multiply the weights that
// define the influecnce of neighbors on the classification of a given pixel.
// The higher the value, the more uniform will be the regions resulting from
// the classification refinement.
// 
// Software Guide : EndLatex 
 
// Software Guide : BeginCodeSnippet
  mrfFilter->SetSmoothingFactor( smoothingFactor );
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// Given that the MRF filter need to continually relabel the pixels, it needs
// access to a set of membership functions that will measure to what degree
// every pixel belongs to a particular class.  The classification is performed
// by the \doxygen{ImageClassifierBase} class, that is instantiated using the
// type of the input vector image and the type of the labeled image.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::ImageClassifierBase< 
                              ArrayImageType,
                              LabelImageType >   SupervisedClassifierType;

  SupervisedClassifierType::Pointer classifier = 
                                         SupervisedClassifierType::New();
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// The classifier need a decision rule to be set by the user. Note that we must
// use \code{GetPointer()} in the call of the \code{SetDecisionRule()} method
// because we are passing a SmartPointer, and smart pointer cannot perform
// polymorphism, we must then extract the raw pointer that is associated to the
// smart pointer. This extraction is done with the GetPointer() method.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::MinimumDecisionRule DecisionRuleType;

  DecisionRuleType::Pointer  classifierDecisionRule = DecisionRuleType::New();

  classifier->SetDecisionRule( classifierDecisionRule.GetPointer() );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// We now instantiate the membership functions. In this case we use the
// \subdoxygen{Statistics}{DistanceToCentroidMembershipFunction} class
// templated over the pixel type of the vector image, that in our example
// happens to be a vector of dimension 1.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::Statistics::DistanceToCentroidMembershipFunction< 
                                                    ArrayPixelType > 
                                                       MembershipFunctionType;

  typedef MembershipFunctionType::Pointer MembershipFunctionPointer;


  double meanDistance = 0;
  vnl_vector<double> centroid(1); 
  for( unsigned int i=0; i < numberOfClasses; i++ )
    {
    MembershipFunctionPointer membershipFunction = 
                                         MembershipFunctionType::New();

    centroid[0] = atof( argv[i+numberOfArgumentsBeforeMeans] ); 

    membershipFunction->SetCentroid( centroid );

    classifier->AddMembershipFunction( membershipFunction );
    meanDistance += static_cast< double > (centroid[0]);
    }
  meanDistance /= numberOfClasses;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We set the Smoothing factor. This factor will multiply the weights that
// define the influecnce of neighbors on the classification of a given pixel.
// The higher the value, the more uniform will be the regions resulting from
// the classification refinement.
// 
// Software Guide : EndLatex 
 
// Software Guide : BeginCodeSnippet
  mrfFilter->SetSmoothingFactor( smoothingFactor );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// and we set the neighborhood radius that will define the size of the clique
// to be used in the computation of the neighbors' influence in the
// classification of any given pixel. Note that despite the fact that we call
// this a radius, it is actually the half size of an hypercube. That is, the
// actual region of influence will not be circular but rather an N-Dimensional
// box. For example, a neighborhood radius of 2 in a 3D image will result in a
// clique of size 5x5x5 pixels, and a radius of 1 will result in a clique of
// size 3x3x3 pixels.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  mrfFilter->SetNeighborhoodRadius( 1 );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// We should now set the weights used for the neighbors. This is done by
// passing an array of values that contains the linear sequence of weights for
// the neighbors. For example, in a neighborhood of size 3x3x3, we should
// provide a linear array of 9 weight values. The values are packaged in a
// \code{std::vector} and are supposed to be \code{double}. The following lines
// illustrate a typical set of values for a 3x3x3 neighborhood. The array is
// arranged and then passed to the filter by using the method
// \code{SetMRFNeighborhoodWeight()}.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  std::vector< double > weights;
  weights.push_back(1.5);
  weights.push_back(2.0);
  weights.push_back(1.5);
  weights.push_back(2.0);
  weights.push_back(0.0); // This is the central pixel
  weights.push_back(2.0);
  weights.push_back(1.5);
  weights.push_back(2.0);
  weights.push_back(1.5);
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
// We now scale weights so that the smoothing function and the image fidelity
// functions have comparable value. This is necessary since the label
// image and the input image can have different dynamic ranges. The fidelity
// function is usually computed using a distance function, such as the
// \doxygen{DistanceToCentroidMembershipFunction} or one of the other 
// membership functions. They tend to have values in the order of the means
// specified. 
// Software Guide : EndLatex 
    
// Software Guide : BeginCodeSnippet
  double totalWeight = 0;
  for(std::vector< double >::const_iterator wcIt = weights.begin(); 
      wcIt != weights.end(); ++wcIt )
    {
    totalWeight += *wcIt;
    }
  for(std::vector< double >::iterator wIt = weights.begin(); 
      wIt != weights.end(); wIt++ )
    {
    *wIt = static_cast< double > ( (*wIt) * meanDistance / (2 * totalWeight));
    }

  mrfFilter->SetMRFNeighborhoodWeight( weights );
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// Finally, the classifier class is connected to the Markof Random Fields filter.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  mrfFilter->SetClassifier( classifier );
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// The output image produced by the \doxygen{MRFImageFilter} has the same pixel
// type as the labeled input image. In the following lines we use the
// \code{OutputImageType} in order to instantiate the type of a
// \doxygen{ImageFileWriter}. Then create one, and connect it to the output of
// the classification filter.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef MRFFilterType::OutputImageType  OutputImageType;

  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  
  writer->SetInput( mrfFilter->GetOutput() );

  writer->SetFileName( outputImageFileName );
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// We are now ready for triggering the execution of the pipeline. This is done
// by simply invoking the \code{Update()} method in the writer. This call will
// propagate the update request to the reader and then to the MRF filter.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem encoutered while writing image file : " << argv[2] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
// Software Guide : EndCodeSnippet

  std::cout << "Number of Iterations : " << mrfFilter->GetNumberOfIterations() 
    << std::endl;
  std::cout << "Stop condition: (1) Maximum number of iterations (2) Error tolerance:  " 
    << mrfFilter->GetStopCondition() << std::endl;

  //  Software Guide : BeginLatex
  //  
  // \begin{figure} \center
  // \includegraphics[width=0.44\textwidth]{ScalarImageMarkovRandomField1Output.eps}
  // \itkcaption[Output of the ScalarImageMarkovRandomField]{Effect of the
  // MRF filter on a T1 slice of the brain.}
  // \label{fig:ScalarImageMarkovRandomFieldInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:ScalarImageMarkovRandomFieldInputOutput}
  //  illustrates the effect of this filter with three classes.
  //  In this example the filter was run with a smoothing factor of 3.
  //  The labelled image was produced by ScalarImageKmeansClassifier.cxx
  //  and the means were estimated by ScalarImageKmeansModelEstimator.cxx.
  //
  //  Software Guide : EndLatex 

  return EXIT_SUCCESS;
  
}


