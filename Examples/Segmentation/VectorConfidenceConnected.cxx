/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    VectorConfidenceConnected.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//  
//  This example illustrates the use of the Confidence connected concept
//  applied to images with vector pixel types. The Confidence connected
//  algorithm is implemented for vector images in the class
//  \doxygen{VectorConfidenceConnected}. The basic difference between the
//  scalar and vector version is that the vector version uses the covariance
//  matrix instead of a variance, and a vector mean instead of a scalar mean.
//  The membership of a vector pixel value to the region is measured using the
//  Mahalanobis distance as implemented in the class
//  \subdoxygen{Statistics}{MahalanobisDistanceImageFunction}.
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
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage seedX seedY multiplier iterations" << std::endl;
    return 1;
    }




  //  Software Guide : BeginLatex
  //  
  //  We now declare the image type using a pixel type and a particular
  //  dimension. In this case the \code{float} type is used for the pixels due
  //  to the requirements of the smoothing filter. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   unsigned char                         PixelComponentType;
  typedef   itk::RGBPixel< PixelComponentType >   InputPixelType;
  
  const     unsigned int    Dimension = 2;
  
  typedef itk::Image< InputPixelType, Dimension >  InputImageType;
  // Software Guide : EndCodeSnippet


  typedef unsigned char OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

                        
  //
  // We instantiate reader and writer types
  //
  typedef  itk::ImageFileReader<  InputImageType   > ReaderType;
  typedef  itk::ImageFileWriter<  OutputImageType  > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );




  //  Software Guide : BeginLatex
  //  
  //  We now declare the type of the region growing filter. In this case it is
  //  the \doxygen{VectorConfidenceConnectedImageFilter}. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  itk::VectorConfidenceConnectedImageFilter< 
                                    InputImageType, 
                                    OutputImageType > ConnectedFilterType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  Then, we  construct one filter of this class using the \code{New()} method. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ConnectedFilterType::Pointer confidenceConnected = ConnectedFilterType::New();
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  Now it is time to connect the pipeline. This is pretty linear in our
  //  example.  
  //  
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet

  confidenceConnected->SetInput( reader->GetOutput() );

  writer->SetInput( confidenceConnected->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \doxygen{VectorConfidenceConnectedImageFilter} has two parameters
  //  to be defined. First, the factor $f$ that the defines how large
  //  the range of intensities will be. Small values of the multiplier
  //  will restrict the inclusion of pixels to those having very
  //  similar intensities to those in the current region. Larger
  //  values of the multiplier will relax the accepting condition and
  //  will result in more generous growth of the region. Values that
  //  are too large will make the region ingest neighbor regions in
  //  the image that may actually belong to separate anatomical
  //  structures.
  //
  //  \index{itk::VectorConfidenceConnectedImageFilter!SetMultiplier()}
  //
  //  Software Guide : EndLatex 

  const double multiplier = atof( argv[5] );

  // Software Guide : BeginCodeSnippet
  confidenceConnected->SetMultiplier( multiplier );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The number of iterations may be decided based on the homogeneity of the
  //  intensities of the anatomical structure to be segmented. Highly homogeneous
  //  regions may only require a couple of iterations. Regions with ramp
  //  effects, like MRI images with inhomogenous fields, may require
  //  more iterations. In practice, it seems to be more relevant to carefully
  //  select the multiplier factor than the number of iterations.
  //  However, keep in mind that there is no reason to assume that this
  //  algorithm should converge to a stable region. It is possible that by
  //  letting the algorithm run for more iterations the region will end up
  //  engulfing the entire image.
  //
  //  \index{itk::VectorConfidenceConnectedImageFilter!SetNumberOfIterations()}
  //
  //  Software Guide : EndLatex 

  const unsigned int iterations = atoi( argv[6] );
    
  // Software Guide : BeginCodeSnippet
  confidenceConnected->SetNumberOfIterations( iterations );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The output of this filter is a binary image with zero-value pixels
  //  everywhere except on the extracted region. The intensity value to be put
  //  inside the region is selected with the method \code{SetReplaceValue()}
  //
  //  \index{itk::VectorConfidenceConnectedImageFilter!SetReplaceValue()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  confidenceConnected->SetReplaceValue( 255 );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  The initialization of the algorithm requires the user to provide a seed
  //  point. It is convenient to select this point to be placed in a
  //  \emph{typical} region of the anatomical structure to be segmented. A
  //  small neighborhood around the seed point will be used to compute the
  //  initial mean and standard deviation for the inclusion criterion. The seed
  //  is passed in the form of a \doxygen{Index} to the \code{SetSeed()} method.
  //
  //  \index{itk::VectorConfidenceConnectedImageFilter!SetSeed()}
  //  \index{itk::VectorConfidenceConnectedImageFilter!SetInitialNeighborhoodRadius()}
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
  //  Let's now run this example using as input the image
  //  \code{VisibleWomanEyeSlice.png} provided in the directory
  //  \code{Insight/Examples/Data}. We can easily segment the major anatomical
  //  structures by providing seeds in the appropriate locations. For example,
  //
  //  \begin{center}
  //  \begin{tabular}{|l|c|c|c|c|}
  //  \hline
  //  Structure & Seed Index & Multiplier & Iterations & Output Image \\ \hline \\ \hline
  //  Rectum & $(70,120)$ & 50 & 10 & Second from left in Figure \ref{fig:VectorConfidenceConnectedOutput} \\ \hline
  //  Rectum & $(23, 93)$ & 45 & 10 & Third  from left in Figure \ref{fig:VectorConfidenceConnectedOutput} \\ \hline
  //  Vitreo & $(66, 66)$ & 15 & 20 & Fourth from left in Figure \ref{fig:VectorConfidenceConnectedOutput} \\ \hline
  //  \end{tabular}
  //  \end{center}
  //
  // \begin{figure} \center
  // \includegraphics[width=0.24\textwidth]{VisibleWomanEyeSlice.eps}
  // \includegraphics[width=0.24\textwidth]{VectorConfidenceConnectedOutput1.eps}
  // \includegraphics[width=0.24\textwidth]{VectorConfidenceConnectedOutput2.eps}
  // \includegraphics[width=0.24\textwidth]{VectorConfidenceConnectedOutput3.eps}
  // \itkcaption[VectorConfidenceConnected segmentation results]{Segmentation results of
  // the VectorConfidenceConnected filter for various seed points.}
  // \label{fig:VectorConfidenceConnectedOutput}
  // \end{figure}
  //
  // The coloration of muscular tissue makes it easy to distinguish from the
  // surrounding anatomical strucures. The optic nerf on the other hand has
  // similar coloration to neighborhor structures.
  //
  //  Software Guide : EndLatex 




  return 0;

}




