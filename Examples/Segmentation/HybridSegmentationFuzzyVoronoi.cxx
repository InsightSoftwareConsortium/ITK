/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    HybridSegmentationFuzzyVoronoi.cxx
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
// This example illustrates the use of the \doxygen{SimpleFuzzyConnectednessScalarImageFilter}
// and \doxygen{VoronoiSegmentationImageFilter} to build a hybrid segmentation that integrates fuzzy
// connectedness with the Voronoi Diagram Classification.
//
// First, we include the header files of the two filters.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkSimpleFuzzyConnectednessScalarImageFilter.h"
#include "itkVoronoiSegmentationImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkImage.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkRescaleIntensityImageFilter.h"


int main( int argc, char **argv )
{


  if( argc < 9 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage seedX seedY " << std::endl;
    std::cerr << " estimateMean estimateVariance (used by FuzzySegmentation) " << std::endl;
    std::cerr << " meanTolerance standardDeviationTolerance (used by VoronoiSegmentation) " << std::endl;
    return 1;
    }




  //  Software Guide : BeginLatex
  //  
  //  Next, we declare the pixel type and image dimension and 
  //  specify the image type to be used as input.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  unsigned char     InputPixelType;
  const    unsigned int      Dimension = 2;
  
  typedef itk::Image< InputPixelType, Dimension >  InputImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  Fuzzy Connectedness segmentation is performed first to generate a
  //  rough segmentation that yields a sample of tissue from a region to be segmented.
  //  A binary result, representing the sample, is used as a prior for the
  //  next step.  We use here the \doxygen{SimpleFuzzyConnectednessScalarImageFilter} 
  //  but we may also utilize any other image segmentation filter instead.  The result
  //  of the segmentation produced by the fuzzy segmentation filter is 
  //  stored in a binary image.  Below, we declare the type of the image using a
  //  pixel type and a spatial dimension.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   bool      BinaryPixelType;
  
  typedef itk::Image< BinaryPixelType, Dimension >      BinaryImageType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  The fuzzy segmentation filter type is instantiated here using the input
  //  and binary image types as template parameters.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   itk::SimpleFuzzyConnectednessScalarImageFilter< 
                               InputImageType, 
                               BinaryImageType >        FuzzySegmentationFilterType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  The fuzzy connectedness segmentation filter is created by invoking the 
  //  \code{New()} method and assigning the result to a \code{SmartPointer}.
  //
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!New()}
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FuzzySegmentationFilterType::Pointer fuzzysegmenter = 
                                               FuzzySegmentationFilterType::New();
  // Software Guide : EndCodeSnippet





  
  //  Software Guide : BeginLatex
  //  
  //  In the next step of the hybrid segmenttaion method the prior generated
  //  from the fuzzy segmentation method is used to build a homogeneity measurement for the
  //  object.  A \doxygen{VoronoiSegmentationImageFilter} uses the homogeneity measurement
  //  to drive iterative subdivision of Voronoi regions and to generate the final
  //  segmentation result (for details, please read \cite{Imielinska2000b}).  
  //  In this example, the result of the \doxygen{VoronoiSegmentationImageFilter} is sent to a
  //  writer. Its output type is conveniently declared as one that is compatible with the
  //  writer.  
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef unsigned char OutputPixelType;

  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  // Software Guide : EndCodeSnippet

                        

  //  Software Guide : BeginLatex
  //
  //  The following lines declare the type of the Voronoi Diagram Classification filter
  //  and create one filter object.
  //
  //  \index{itk::VoronoiSegmentationImageFilter!New()}
  //  \index{itk::VoronoiSegmentationImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  itk::VoronoiSegmentationImageFilter< 
                                    InputImageType, 
                                    OutputImageType > VoronoiSegmentationFilterType;

  VoronoiSegmentationFilterType::Pointer voronoisegmenter = 
                                     VoronoiSegmentationFilterType::New();
  // Software Guide : EndCodeSnippet
  




  //  Software Guide : BeginLatex
  //
  // \begin{figure} \center
  // \includegraphics[width=7cm]{BrainT1Slice.eps}
  // \includegraphics[width=7cm]{HybridSegmentationFuzzyVoronoiOutput.eps}
  // \caption{Segmentation results for the hybrid segmentation approach.}
  // \label{fig:HybridSegmentationFuzzyVoronoiOutput}
  // \end{figure}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginLatex
  //
  // \begin{figure} \center
  // \includegraphics[width=7cm]{FatMRISlice.eps}
  // \includegraphics[width=7cm]{HybridSegmentationFuzzyVoronoiOutput2.eps}
  // \caption{Another segmentation results for the hybrid segmentation approach.}
  // \label{fig:HybridSegmentationFuzzyVoronoiOutput2}
  // \end{figure}
  //
  //  Software Guide : EndLatex 


  //
  // We instantiate reader and writer types
  //
  typedef  itk::ImageFileReader< InputImageType > ReaderType;
  typedef  itk::ImageFileWriter<  OutputImageType  > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );




  //  Software Guide : BeginLatex
  //  
  //  The input that is passed to the FuzzySegmentationFilter is taken from the reader.
  //
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!SetInput()}
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  fuzzysegmenter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet

  
  InputImageType::IndexType index;

  index[0] = atoi(argv[3]);
  index[1] = atoi(argv[4]);


  const float  mean              = atof(argv[5]);
  const float  variance          = atof(argv[6]);

  const float  meanTolerance     = atof( argv[7] );
  const float  stdTolerance      = atof( argv[8] );


  //  Software Guide : BeginLatex
  //  
  //  The parameters of the FuzzySegmentationFilter are defined here. A seed
  //  point is provided with the method \code{SetObjectsSeed()} in order to
  //  initialize the region to be grown.  Estimated values for the mean and
  //  variance of the object intensities are also provided with the methods
  //  \code{SetMean()} and \code{SetVariance()}, respectively. A threshold value
  //  for generating the binary object is preset with the method
  //  \code{SetThreshold()}.  For details describing the role of the mean and
  //  variance on the computation of the segmentation see \cite{Udupa1996}. 
  //
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!SetObjectsSeed()}
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!SetMean()}
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!SetVariance()}
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!SetThreshold()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  fuzzysegmenter->SetObjectsSeed( index );
  fuzzysegmenter->SetMean( mean );
  fuzzysegmenter->SetVariance( variance );
  fuzzysegmenter->SetThreshold( 0.5 );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The execution of the FuzzySegmentationFilter is triggered with the
  //  \code{Update()} method.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  fuzzysegmenter->Update();
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  The input to the Voronoi Diagram Classification filter is fetched from the reader and the prior is
  //  fetched from the fuzzy segmentation filter.
  //
  //  \index{itk::VoronoiSegmentationImageFilter!SetInput()}
  //  \index{itk::VoronoiSegmentationImageFilter!TakeAPrior()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  voronoisegmenter->SetInput( reader->GetOutput() );
  voronoisegmenter->TakeAPrior( fuzzysegmenter->GetOutput());
  // Software Guide : EndCodeSnippet






  //  Software Guide : BeginLatex
  //  
  //  The tolerance levels for testing the mean and standard deviation are set
  //  with the methods \code{SetMeanPercentError()} and
  //  \code{SetSTDPercentError()}. Note that the
  //  \doxygen{FuzzySegmentationFilter} uses \emph{variance} as parameter while
  //  the \doxygen{VoronoiSegmentationImageFilter} uses the tolerance of the
  //  \emph{standard deviation} as a parameter. For more details on how these parameters should be
  //  selected see \cite{Imielinska2000b}.
  //  
  //  \index{itk::VoronoiSegmentationImageFilter!SetMeanPercentError()}
  //  \index{itk::VoronoiSegmentationImageFilter!SetSTDPercentError()}
  //
  //  Software Guide : EndLatex 
 
  // Software Guide : BeginCodeSnippet
  voronoisegmenter->SetMeanPercentError( meanTolerance );
  voronoisegmenter->SetSTDPercentError(  stdTolerance );
  // Software Guide : EndCodeSnippet






  //  Software Guide : BeginLatex
  // 
  //  The \emph{resolution} of the Voronoi Diagram Classification can be choosen with the
  //  method \code{SetMinRegion()}. 
  //
  //  \index{itk::VoronoiSegmentationImageFilter!SetMinRegion()}
  //
  //  Software Guide : EndLatex 

  
  // Software Guide : BeginCodeSnippet
  voronoisegmenter->SetMinRegion( 5 );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  The execution of the VoronoiSegmentationFilter is triggered with the
  //  \code{Update()} method.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  voronoisegmenter->Update();
  // Software Guide : EndCodeSnippet


  

  
  //  Software Guide : BeginLatex
  //
  //  The output of the Voronoi Diagram Classification is an image mask with zeros
  //  everywhere an ones inside the segmented object. This image will appear
  //  black on many image viewers since they usually do not stretch the
  //  graylevels. We add here a \doxygen{RescaleIntensityImageFilter} in
  //  order to expand the dynamic range to more typical values. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::RescaleIntensityImageFilter< 
                                  OutputImageType,
                                  OutputImageType >   ScalerFilterType;
  
  ScalerFilterType::Pointer scaler = ScalerFilterType::New();

  scaler->SetOutputMinimum(   0 );
  scaler->SetOutputMaximum( 255 );

  scaler->SetInput( voronoisegmenter->GetOutput() );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  // The output of the rescaler is passed to the writer. The invocation
  // of the \code{Update()} method on the writer triggers the execution of
  // the pipeline.
  //  
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  writer->SetInput( scaler->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  We execute this program on the image \code{BrainT1Slice.png} available
  //  in the directory \code{Insight/Examples/Data}. The following parameters
  //  are passed to the command line:
  // 
  //  \begin{verbatim}
  //  HybridSegmentationFuzzyVoronoi BrainT1Slice.png Output.png 140 125 140 25 0.2 2.0
  //  \end{verbatim}
  //
  //  Parameters $(140,125)$ specify the index position of a seed point in the image,
  //  $140$ and $25$ are the estimated mean and standard deviation, respectively, of the
  //  object to be segmented; and finally $0.2$ and $2.0$ are the tolerace for
  //  the mean and standard deviation, respectively.  Figure
  //  \ref{fig:HybridSegmentationFuzzyVoronoiOutput} shows the input image and
  //  the binary mask resulting from the segmentation.
  //
  //  Note that in order to segment successfully other images, one has to play with setting
  //  proper parameters that are best chosen for a new data. For example, when
  //  segmenting the input image \code{FatMRISlice.png} we apply the following
  //  new set of parameters parameters. 
  // 
  //  \begin{verbatim}
  //  HybridSegmentationFuzzyVoronoi FatMRISlice.png Output.png 80 200 140 300 0.3 3.0
  //  \end{verbatim}
  //
  //  Figure \ref{fig:HybridSegmentationFuzzyVoronoiOutput2} shows the input
  //  image and the binary mask resulting from this segmentation.
  //  
  //  Similarly, we can segment color (RGB) and other multi-channel images. Examples illustrating
  //  it will be posted soon on the ITK site.
  //  We have built user friendly interfaces for each filter in the Hybrid Segmentation Engine
  //  that allow setting up interactively parameters for segmentation. This environment makes
  //  building pipeline for new hybrid methods, and running segmentation algorithms more efficient.
  //  
  //
  //  Software Guide : EndLatex 



  return 0;

}




