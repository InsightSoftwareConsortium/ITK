/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    FuzzyConnectednessImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

// Software Guide : BeginLatex
//
// This example illustrates the use of the
// \doxygen{SimpleFuzzyConnectednessScalarImageFilter}. This filter computes an
// affinity map from a seed point provided by the user. This affinity map
// indicates for every pixels how homogeneous is the path that will link it to
// the seed point.
//
// Please note that the Fuzzy Connectedness algorithm is covered by a Patent
// \cite{Udupa1998}. For this reason the current example is located in the
// \texttt{Examples/Patented} subdirectory.
//
// In order to use this algorithm we should first include the header files of
// the filter and the image class.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkSimpleFuzzyConnectednessScalarImageFilter.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Since the FuzzyConnectednessImageFilter requires an estimation of the
// gray level mean and variance for the region to be segmented, we use here the
// \doxygen{ConfidenceConnectedImageFilter} as a preprocessor that produces a
// rough segmentation and estimates from it the values of the mean and the
// variance.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkConfidenceConnectedImageFilter.h"
// Software Guide : EndCodeSnippet



#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


int main( int argc, char *argv[] )
{
  if( argc < 7 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage outputImage outputAffinityMap " << std::endl;
    std::cerr << " seedX seedY multiplier " << std::endl;
    return 1;
    }


  //  Software Guide : BeginLatex
  //  
  //  Next, we declare the pixel type and image dimension and 
  //  specify the image type to be used as input.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  float            InputPixelType;
  const    unsigned int     Dimension = 2;
  typedef itk::Image< InputPixelType, Dimension >  InputImageType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  Fuzzy connectedness computes first the affinity map and then thresholds
  //  its values in order to get a binary image as output. The type of the
  //  binary image is provided as the second template parameter of the filter.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   unsigned char   BinaryPixelType;
  typedef itk::Image< BinaryPixelType, Dimension >      BinaryImageType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The Confidence connected filter type is instantiated using the input
  //  image type and a binary image type for output.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ConfidenceConnectedImageFilter< 
                                                  InputImageType, 
                                                  BinaryImageType 
                                                    >  ConfidenceConnectedFilterType;

  ConfidenceConnectedFilterType::Pointer confidenceConnectedFilter = 
                                                 ConfidenceConnectedFilterType::New();
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The fuzzy segmentation filter type is instantiated here using the input
  //  and binary image types as template parameters.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::SimpleFuzzyConnectednessScalarImageFilter< 
                                                  InputImageType, 
                                                  BinaryImageType 
                                                    >  FuzzySegmentationFilterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  The fuzzy connectedness segmentation filter is created by invoking the
  //  \code{New()} method and assigning the result to a
  //  \doxygen{SmartPointer}.
  //
  //  \index{itk::SimpleFuzzy\-Connectedness\-Scalar\-Image\-Filter!New()}
  //  \index{itk::SimpleFuzzy\-Connectedness\-Scalar\-Image\-Filter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FuzzySegmentationFilterType::Pointer fuzzysegmenter = 
                                         FuzzySegmentationFilterType::New();
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  The affinity map can be accessed through the type \code{FuzzySceneType}
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  typedef FuzzySegmentationFilterType::FuzzySceneType  FuzzySceneType; 
  // Software Guide : EndCodeSnippet
 




  //  Software Guide : BeginLatex
  //  
  // We instantiate reader and writer types
  //
  //  Software Guide : EndLatex 
  typedef  itk::ImageFileReader< InputImageType  >    ReaderType;
  typedef  itk::ImageFileWriter< BinaryImageType >    WriterType;
  typedef  itk::ImageFileWriter< FuzzySceneType  >    FuzzyWriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  FuzzyWriterType::Pointer fwriter = FuzzyWriterType::New();

  reader->SetFileName(  argv[1] );
  writer->SetFileName(  argv[2] );
  fwriter->SetFileName( argv[3] );

 
  InputImageType::IndexType index;

  index[0] = atoi(argv[4]);
  index[1] = atoi(argv[5]);

  const double varianceMultiplier = atof( argv[6] );

  //  Software Guide : BeginLatex
  //  
  //  The output of the reader is passed as input to the ConfidenceConnected image filter.
  //  Then the filter is executed in order to obtain estimations of the mean and variance
  //  gray values for the region to be segmented.
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  confidenceConnectedFilter->SetInput( reader->GetOutput()  );
  confidenceConnectedFilter->SetMultiplier( varianceMultiplier );
  confidenceConnectedFilter->SetNumberOfIterations( 2 );
  confidenceConnectedFilter->AddSeed( index );

  confidenceConnectedFilter->Update();
  // Software Guide : EndCodeSnippet


  WriterType::Pointer confidenceWriter = WriterType::New();
  confidenceWriter->SetInput( confidenceConnectedFilter->GetOutput() );
  confidenceWriter->SetFileName("confidenceConnectedPreprocessing.png");
  confidenceWriter->Update();


  //  Software Guide : BeginLatex
  //  
  //  The input that is passed to the fuzzy segmentation filter is taken from
  //  the reader.
  //
  //  \index{itk::Simple\-Fuzzy\-Connectedness\-Scalar\-Image\-Filter!SetInput()}
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  fuzzysegmenter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet

  const double  meanEstimation      = confidenceConnectedFilter->GetMean();
  const double  varianceEstimation  = confidenceConnectedFilter->GetVariance();

  std::cout << "Mean     estimation = " << meanEstimation     << std::endl;
  std::cout << "Variance estimation = " << varianceEstimation << std::endl;


  //  Software Guide : BeginLatex
  //  
  //  The parameters of the fuzzy segmentation filter are defined here. A seed
  //  point is provided with the method \code{SetObjectsSeed()} in order to
  //  initialize the region to be grown.  Estimated values for the mean and
  //  variance of the object intensities are also provided with the methods
  //  \code{SetMean()} and \code{SetVariance()}, respectively. A threshold
  //  value for generating the binary object is preset with the method
  //  \code{SetThreshold()}.  For details describing the role of the mean and
  //  variance on the computation of the segmentation, please see
  //  \cite{Udupa1996}. 
  //
  //  \index{itk::Simple\-Fuzzy\-Connectedness\-Scalar\-Image\-Filter!SetObjectsSeed()}
  //  \index{itk::Simple\-Fuzzy\-Connectedness\-Scalar\-Image\-Filter!SetMean()}
  //  \index{itk::Simple\-Fuzzy\-Connectedness\-Scalar\-Image\-Filter!SetVariance()}
  //  \index{itk::Simple\-Fuzzy\-Connectedness\-Scalar\-Image\-Filter!SetThreshold()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  fuzzysegmenter->SetObjectSeed( index );
  fuzzysegmenter->SetMean( meanEstimation );
  fuzzysegmenter->SetVariance( varianceEstimation );
  fuzzysegmenter->SetThreshold( 0.5 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  The execution of the fuzzy segmentation filter is triggered by the
  //  \code{Update()} method.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  fuzzysegmenter->Update();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  writer->SetInput( fuzzysegmenter->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  fwriter->SetInput( fuzzysegmenter->GetFuzzyScene() );
  fwriter->Update();
  // Software Guide : EndCodeSnippet


  return 0;
}




