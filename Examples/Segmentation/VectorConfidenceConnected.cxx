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

//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {VisibleWomanEyeSlice.png}
//    OUTPUTS: {VectorConfidenceConnectedOutput1.png}
//    ARGUMENTS:    70 120 7 1
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {VisibleWomanEyeSlice.png}
//    OUTPUTS: {VectorConfidenceConnectedOutput2.png}
//    ARGUMENTS:    23 93 7 1
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {VisibleWomanEyeSlice.png}
//    OUTPUTS: {VectorConfidenceConnectedOutput3.png}
//    ARGUMENTS:    66 66 3 1
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  This example illustrates the use of the confidence connected concept
//  applied to images with vector pixel types. The confidence connected
//  algorithm is implemented for vector images in the class
//  \doxygen{VectorConfidenceConnected}. The basic difference between the
//  scalar and vector version is that the vector version uses the covariance
//  matrix instead of a variance, and a vector mean instead of a scalar mean.
//  The membership of a vector pixel value to the region is measured using the
//  Mahalanobis distance as implemented in the class
//  \subdoxygen{Statistics}{MahalanobisDistanceThresholdImageFunction}.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkVectorConfidenceConnectedImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRGBPixel.h"


int main( int argc, char *argv[] )
{
  if( argc < 7 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0]
              << " inputImage  outputImage"
              << " seedX seedY"
              << " multiplier iterations" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  We now define the image type using a particular pixel type and
  //  dimension. In this case the \code{float} type is used for the pixels
  //  due to the requirements of the smoothing filter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 2;

  typedef unsigned char                           PixelComponentType;
  typedef itk::RGBPixel< PixelComponentType >     InputPixelType;
  typedef itk::Image< InputPixelType, Dimension > InputImageType;
  // Software Guide : EndCodeSnippet

  typedef unsigned char                            OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;


  // We instantiate reader and writer types
  //
  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  //  Software Guide : BeginLatex
  //
  //  We now declare the type of the region-growing filter. In this case it
  //  is the \doxygen{VectorConfidenceConnectedImageFilter}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::VectorConfidenceConnectedImageFilter< InputImageType,
                                   OutputImageType > ConnectedFilterType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Then, we construct one filter of this class using the \code{New()}
  //  method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ConnectedFilterType::Pointer confidenceConnected
                                                 = ConnectedFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Next we create a simple, linear data processing pipeline.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  confidenceConnected->SetInput( reader->GetOutput() );
  writer->SetInput( confidenceConnected->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  \code{VectorConfidenceConnectedImageFilter} requires two
  //  parameters.  First, the multiplier factor $f$ defines how large the
  //  range of intensities will be. Small values of the multiplier will
  //  restrict the inclusion of pixels to those having similar intensities to
  //  those already in the current region. Larger values of the multiplier
  //  relax the accepting condition and result in more generous growth of the
  //  region. Values that are too large will cause the region to grow into
  //  neighboring regions which may actually belong to separate anatomical
  //  structures.
  //
  //  \index{itk::Vector\-Confidence\-Connected\-Image\-Filter!SetMultiplier()}
  //
  //  Software Guide : EndLatex

  const double multiplier = atof( argv[5] );

  // Software Guide : BeginCodeSnippet
  confidenceConnected->SetMultiplier( multiplier );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The number of iterations is typically determined based on the
  //  homogeneity of the image intensity representing the anatomical
  //  structure to be segmented. Highly homogeneous regions may only require
  //  a couple of iterations. Regions with ramp effects, like MRI images with
  //  inhomogeneous fields, may require more iterations. In practice, it seems
  //  to be more relevant to carefully select the multiplier factor than the
  //  number of iterations.  However, keep in mind that there is no reason to
  //  assume that this algorithm should converge to a stable region. It is
  //  possible that by letting the algorithm run for more iterations the
  //  region will end up engulfing the entire image.
  //
  //  \index{itk::Vector\-Confidence\-Connected\-Image\-Filter!SetNumberOfIterations()}
  //
  //  Software Guide : EndLatex

  const unsigned int iterations = atoi( argv[6] );

  // Software Guide : BeginCodeSnippet
  confidenceConnected->SetNumberOfIterations( iterations );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The output of this filter is a binary image with zero-value pixels
  //  everywhere except on the extracted region. The intensity value to be
  //  put inside the region is selected with the method
  //  \code{SetReplaceValue()}.
  //
  //  \index{itk::Vector\-Confidence\-Connected\-Image\-Filter!SetReplaceValue()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  confidenceConnected->SetReplaceValue( 255 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The initialization of the algorithm requires the user to provide a seed
  //  point. This point should be placed in a \emph{typical} region of the
  //  anatomical structure to be segmented. A small neighborhood around the
  //  seed point will be used to compute the initial mean and standard
  //  deviation for the inclusion criterion. The seed is passed in the form
  //  of an \doxygen{Index} to the \code{SetSeed()} method.
  //
  //  \index{itk::Vector\-Confidence\-Connected\-Image\-Filter!SetSeed()}
  //  \index{itk::Vector\-Confidence\-Connected\-Image\-Filter!SetInitialNeighborhoodRadius()}
  //
  //  Software Guide : EndLatex

  InputImageType::IndexType  index;

  index[0] = atoi( argv[3] );
  index[1] = atoi( argv[4] );


  // Software Guide : BeginCodeSnippet
  confidenceConnected->SetSeed( index );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The size of the initial neighborhood around the seed is defined with the
  //  method \code{SetInitialNeighborhoodRadius()}. The neighborhood will be
  //  defined as an $N$-Dimensional rectangular region with $2r+1$ pixels on
  //  the side, where $r$ is the value passed as initial neighborhood radius.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  confidenceConnected->SetInitialNeighborhoodRadius( 3 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The invocation of the \code{Update()} method on the writer triggers the
  //  execution of the pipeline.  It is usually wise to put update calls in a
  //  \code{try/catch} block in case errors occur and exceptions are thrown.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Now let's run this example using as input the image
  //  \code{VisibleWomanEyeSlice.png} provided in the directory
  //  \code{Examples/Data}. We can easily segment the major anatomical
  //  structures by providing seeds in the appropriate locations. For example,
  //
  //  \begin{center}
  //  \begin{tabular}{|l|c|c|c|c|}
  //  \hline
  //  Structure & Seed Index & Multiplier & Iterations & Output Image \\ \hline
  //  Rectum & $(70,120)$ & 7 & 1 & Second from left in Figure \ref{fig:VectorConfidenceConnectedOutput} \\ \hline
  //  Rectum & $(23, 93)$ & 7 & 1 & Third  from left in Figure \ref{fig:VectorConfidenceConnectedOutput} \\ \hline
  //  Vitreo & $(66, 66)$ & 3 & 1 & Fourth from left in Figure \ref{fig:VectorConfidenceConnectedOutput} \\ \hline
  //  \end{tabular}
  //  \end{center}
  //
  // \begin{figure} \center
  // \includegraphics[width=0.24\textwidth]{VisibleWomanEyeSlice}
  // \includegraphics[width=0.24\textwidth]{VectorConfidenceConnectedOutput1}
  // \includegraphics[width=0.24\textwidth]{VectorConfidenceConnectedOutput2}
  // \includegraphics[width=0.24\textwidth]{VectorConfidenceConnectedOutput3}
  // \itkcaption[VectorConfidenceConnected segmentation results]{Segmentation results of
  // the VectorConfidenceConnected filter for various seed points.}
  // \label{fig:VectorConfidenceConnectedOutput}
  // \end{figure}
  //
  // The coloration of muscular tissue makes it easy to distinguish them from
  // the surrounding anatomical structures. The optic vitrea on the other hand
  // has a coloration that is not very homogeneous inside the eyeball and
  // does not facilitate a full segmentation based only on color.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginLatex
  //
  //  The values of the final mean vector and covariance matrix used for the
  //  last iteration can be queried using the methods \code{GetMean()} and
  //  \code{GetCovariance()}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef ConnectedFilterType::MeanVectorType       MeanVectorType;
  typedef ConnectedFilterType::CovarianceMatrixType CovarianceMatrixType;

  const MeanVectorType & mean = confidenceConnected->GetMean();
  const CovarianceMatrixType & covariance
                                       = confidenceConnected->GetCovariance();

  std::cout << "Mean vector = "       << mean       << std::endl;
  std::cout << "Covariance matrix = " << covariance << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
