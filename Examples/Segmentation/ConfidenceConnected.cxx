/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ConfidenceConnected.cxx
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
// The following example illustrates the use of the
// \doxygen{ConfidenceConnectedImageFilter}. This filter is based on the use
// of the flood fill iterator. Most of the algorithmic complexity of a region
// growing method comes from the strategy used for visiting the neighbor
// pixels. The flood fill iterator assumes this responsibility and greatly
// simplifies the implementation of a region growing approach. The work left to
// the algorithm is to establish a criterion for deciding whether a particular
// pixel should be included in the current region or not.
//
// \index{itk::FloodFillIterator!In Region Growing}
// \index{itk::ConfidenceConnectedImageFilter|textbf}
// \index{itk::ConfidenceConnectedImageFilter!header}
//
// The criterion used by the \doxygen{ConfidenceConnectedImageFilter} is based on
// simple statistics of the current region. First, the algorithm computes the
// mean and standard deviation of intensity values for all the pixels currently
// included in the region. A user-provided factor is used to multiply the
// standard deviation and define a range aroun the mean. Neighbor pixels whose
// intensity values fall inside the range are accepted to be included in the
// region. When no more neighbor pixes are found that can satisfy the
// criterion, the algorithm considered to have finished its first iteration. At
// that point, the mean and standard deviation of intensity levels are
// recomputed using all the pixels currently included in the region. These mean
// and standard deviation define a new intensity range that is used for
// visiting the current neighbors in search of pixels whose intensity falls
// inside the range.  This iterative process is repated a number of times as
// defined by the user. The following equation illustrates the inclusion 
// criterion used by this filter.
//
// \begin{equation}
// I(\mathbf{X}) \in [ m - f \sigma , m + f \sigma ]
// \end{equation}
//
// where $m$ and $\sigma$ are the mean and standard deviation of the  region
// intensities, $f$ is a factor defined by the user. $I()$ is the image and
// $\mathbf{X}$ is the position of the particular neighbor pixel being
// considered for inclusion in the region.
//
// Let's look at the minimal code required to use this algorithm. First, the
// following header defining the \doxygen{ConfidenceConnectedImageFilter} class
// must be included.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkConfidenceConnectedImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkImage.h"
#include "itkCastImageFilter.h"



//  Software Guide : BeginLatex
//
//  Noise present in the image can reduce the capacity of this filter to grow
//  large regions. When faced with noisy images, it is usually convenient to
//  pre-process the image by using an edge-preserving smoothing filter. Any of
//  the filters discussed in section \ref{sec:EdgePreservingSmoothingFilters}
//  could be used to this end. In this particular example we use the
//  \doxygen{CurvatureFlowImageFilter}, henceforth we need to include its header
//  file.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkCurvatureFlowImageFilter.h"
// Software Guide : EndCodeSnippet




#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


int main( int argc, char **argv )
{


  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage seedX seedY " << std::endl;
    return 1;
    }




  //  Software Guide : BeginLatex
  //  
  //  We declare now the image type using a pixel type and a particular
  //  dimension. In this case the \code{float} type is used for the pixels due
  //  to the requirements of the smoothing filter. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   float           InternalPixelType;
  const     unsigned int    Dimension = 2;
  
  typedef itk::Image< InternalPixelType, Dimension >  InternalImageType;
  // Software Guide : EndCodeSnippet


  typedef unsigned char OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  typedef itk::CastImageFilter< 
                        InternalImageType, 
                        OutputImageType    >    CastingFilterType;
  
  CastingFilterType::Pointer caster = CastingFilterType::New();
                        
  //
  // We instantiate reader and writer types
  //
  typedef  itk::ImageFileReader< InternalImageType > ReaderType;
  typedef  itk::ImageFileWriter<  OutputImageType  > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );



  //  Software Guide : BeginLatex
  //  
  //  
  //  The smoothing filter type is instantiated using the image type as
  //  template parameter.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   itk::CurvatureFlowImageFilter< 
                               InternalImageType, 
                               InternalImageType >  CurvatureFlowImageFilterType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  Then, the filter is created by invoking the \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  CurvatureFlowImageFilterType::Pointer smoothing = 
                         CurvatureFlowImageFilterType::New();
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  We declare now the type of the region growing filter. In this case it is
  //  the \doxygen{ConfidenceConnectedImageFilter}. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  itk::ConfidenceConnectedImageFilter< 
                                    InternalImageType, 
                                    InternalImageType > ConnectedFilterType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  then, we  construct one filter of this class using the \code{New()} method. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ConnectedFilterType::Pointer confidenceConnected = ConnectedFilterType::New();
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  Now is time for connecting the pipeline. This is pretty linear in our
  //  example. A file reader is added at the beginning of the pipeline and a
  //  caster filter and writer are added at the end. The caster filter is
  //  required here to convert \code{float} pixel types to the integers types
  //  since only a few image file formats support \code{float} types.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  smoothing->SetInput( reader->GetOutput() );

  confidenceConnected->SetInput( smoothing->GetOutput() );

  caster->SetInput( confidenceConnected->GetOutput() );

  writer->SetInput( caster->GetOutput() );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  The \doxygen{CurvatureFlowImageFilter} requires a couple of parameter to
  //  be defined. The following are typical values for $2D$ images. However
  //  they may have to be adjusted depending on the amount of noise present in
  //  the input image.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  smoothing->SetNumberOfIterations( 5 );

  smoothing->SetTimeStep( 0.125 );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The \doxygen{ConfidenceConnectedImageFilter} has two parameters to be
  //  defined. First, the factor $f$ that the defines how large the range of
  //  intensities will be. Small values of the multiplier will restrict the
  //  inclusion of pixels to those having very similar intensities to those in
  //  the current region. Larger values of the multiplier will relax the
  //  accepting condition and will result in more generous growth of the
  //  region. Too large values will make the region ingest neighbor regions in
  //  the image that may actually belong to separate anatomical structures.
  //
  //  \index{itk::ConfidenceConnectedImageFilter!SetMultiplier()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  confidenceConnected->SetMultiplier( 2.5 );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The number of iteration may be decided based on how homogeneous are the
  //  intensities on the anatomical structure to be segmented. Very homogeneous
  //  regions may require only a couple of iterations. Regions with ramp
  //  effects, like for example MRI images with inhomogenous fields may require
  //  more iterations. In practice, it seems to be more relevant to carefully
  //  select the multiplier factor rather than the number of iterations.
  //  However keep in mind that there is no reason to assume that this
  //  algorithm should converge to a stable region. It is possible that by
  //  letting the algorithm run for more iterations the region will end up
  //  engulfing the entire image.
  //
  //  \index{itk::ConfidenceConnectedImageFilter!SetNumberOfIterations()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  confidenceConnected->SetNumberOfIterations( 5 );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The output of this filter is a binary image with zero-value pixels
  //  everywhere except on the extracted region. The intensity value to be put
  //  inside the region is selected with the method \code{SetReplaceValue()}
  //
  //  \index{itk::ConfidenceConnectedImageFilter!SetReplaceValue()}
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
  //  \index{itk::ConfidenceConnectedImageFilter!SetSeed()}
  //  \index{itk::ConfidenceConnectedImageFilter!SetInitialNeighborhoodRadius()}
  //
  //  Software Guide : EndLatex 

  InternalImageType::IndexType  index;
  
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
  confidenceConnected->SetInitialNeighborhoodRadius( 2 );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The invokation of the \code{Update()} method on the writer triggers the
  //  execution of the pipeline.  It is usually wise to put update calls in
  //  \code{try/catch} block in case errors ocurr and exceptions are thrown.
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
  //  \code{BrainProtonDensitySlice.png} provided in the directory
  //  \code{Insight/Examples/Data}. We can easily segment the major anatomical
  //  structures by providing seeds in the appropriate locations. For example
  //
  //  \begin{center}
  //  \begin{tabular}{|l|c|c|}
  //  \hline
  //  Structure & Seed Index & Output Image \\
  //  \hline
  //  White matter & $(60,116)$ & Second from left in Figure \ref{fig:ConfidenceConnectedOutput} \\ 
  //  Ventricle    & $(81,112)$ & Third  from left in Figure \ref{fig:ConfidenceConnectedOutput} \\ 
  //  Gray matter  & $(107,69)$ & Fourth from left in Figure \ref{fig:ConfidenceConnectedOutput} \\ 
  //  \hline
  //  \end{tabular}
  //  \end{center}
  //
  // \begin{figure} \center
  // \includegraphics[width=4cm]{BrainProtonDensitySlice.eps}
  // \includegraphics[width=4cm]{ConfidenceConnectedOutput1.eps}
  // \includegraphics[width=4cm]{ConfidenceConnectedOutput2.eps}
  // \includegraphics[width=4cm]{ConfidenceConnectedOutput3.eps}
  // \caption{Segmentation results of the ConfidenceConnected filter for various seed points.}
  // \label{fig:ConfidenceConnectedOutput}
  // \end{figure}
  //
  //  It can be noticed that the gray matter is not being completly segmented.
  //  This illustrates the vulnerability of the region growing methods when the
  //  anatomical structures to be segmented do not have a homogeneous
  //  statistical distribution over the image space. You may want to experiment
  //  with differnt numbers of iterations to verify how the accepted region
  //  will extend.
  //
  //  Software Guide : EndLatex 




  return 0;

}




