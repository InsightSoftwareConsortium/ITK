/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ConnectedThresholdImageFilter.cxx
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
// \doxygen{ConnectedThresholdImageFilter}. This filter is based on the use
// of the flood fill iterator. Most of the algorithmic complexity of a region
// growing method comes from the strategy used for visiting the neighbor
// pixels. The flood fill iterator assumes this responsibility and greatly
// simplifies the implementation of a region growing approach. The work left to
// the algorithm is to establish a criterion for deciding whether a particular
// pixel should be included in the current region or not.
//
// \index{itk::FloodFillIterator!In Region Growing}
// \index{itk::ConnectedThresholdImageFilter|textbf}
// \index{itk::ConnectedThresholdImageFilter!header}
//
// The criterion used by the \doxygen{ConnectedThresholdImageFilter} is based
// on an interval of intensity values provided by the user. Values of lower and
// upper threshold should be provided. The region growing algorithm will then
// include in the region only those pixels whose intensities are inside the
// interval.
//
// \begin{equation}
// I(\mathbf{X}) \in [ \mbox{lower}, \mbox{upper} ]
// \end{equation}
//
// Let's look at the minimal code required to use this algorithm. First, the
// following header defining the \doxygen{ConnectedThresholdImageFilter} class
// must be included.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkConnectedThresholdImageFilter.h"
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


int main( int argc, char *argv[])
{


  if( argc < 7 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage seedX seedY lowerThreshold upperThreshold" << std::endl;
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
  //  the \doxygen{ConnectedThresholdImageFilter}. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  itk::ConnectedThresholdImageFilter< 
                                    InternalImageType, 
                                    InternalImageType > ConnectedFilterType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  then, we  construct one filter of this class using the \code{New()} method. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ConnectedFilterType::Pointer connectedThreshold = ConnectedFilterType::New();
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

  connectedThreshold->SetInput( smoothing->GetOutput() );

  caster->SetInput( connectedThreshold->GetOutput() );

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
  //  The \doxygen{ConnectedThresholdImageFilter} has two main parameters to be
  //  defined. They are the lower and upper thresholds of the interval in which
  //  intensity values should be in order to be included in the region. Setting
  //  these two values too close will not allow enough flexibility to the
  //  region to grow. Setting them too far apart will result in a region that
  //  engulfes the image. 
  //
  //  \index{itk::ConnectedThresholdImageFilter!SetUpper()}
  //  \index{itk::ConnectedThresholdImageFilter!SetLower()}
  //
  //  Software Guide : EndLatex 

  const InternalPixelType lowerThreshold = atof( argv[5] );
  const InternalPixelType upperThreshold = atof( argv[6] );

  // Software Guide : BeginCodeSnippet
  connectedThreshold->SetLower(  lowerThreshold  );
  connectedThreshold->SetUpper(  upperThreshold  );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  The output of this filter is a binary image with zero-value pixels
  //  everywhere except on the extracted region. The intensity value to be put
  //  inside the region is selected with the method \code{SetReplaceValue()}
  //
  //  \index{itk::ConnectedThresholdImageFilter!SetReplaceValue()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  connectedThreshold->SetReplaceValue( 255 );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  The initialization of the algorithm requires the user to provide a seed
  //  point. It is convenient to select this point to be placed in a
  //  \emph{typical} region of the anatomical structure to be segmented. The
  //  seed is passed in the form of a \doxygen{Index} to the \code{SetSeed()}
  //  method.
  //
  //  \index{itk::ConnectedThresholdImageFilter!SetSeed()}
  //
  //  Software Guide : EndLatex 

  InternalImageType::IndexType  index;
  
  index[0] = atoi( argv[3] );
  index[1] = atoi( argv[4] );


  // Software Guide : BeginCodeSnippet
  connectedThreshold->SetSeed( index );
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
  //  structures by providing seeds in the appropriate locations and defining
  //  values for the lower and upper thresholds. For example
  //
  //  \begin{center}
  //  \begin{tabular}{|l|c|c|c|c|}
  //  \hline
  //  Structure & Seed Index & Lower & Upper & Output Image \\ \hline \\ \hline
  //  White matter & $(60,116)$ & 150 & 180 & Second from left in Figure \ref{fig:ConnectedThresholdOutput} \\ \hline
  //  Ventricle    & $(81,112)$ & 210 & 250 & Third  from left in Figure \ref{fig:ConnectedThresholdOutput} \\ \hline
  //  Gray matter  & $(107,69)$ & 180 & 210 & Fourth from left in Figure \ref{fig:ConnectedThresholdOutput} \\ \hline
  //  \end{tabular}
  //  \end{center}
  //
  // \begin{figure} \center
  // \includegraphics[width=4cm]{BrainProtonDensitySlice.eps}
  // \includegraphics[width=4cm]{ConnectedThresholdOutput1.eps}
  // \includegraphics[width=4cm]{ConnectedThresholdOutput2.eps}
  // \includegraphics[width=4cm]{ConnectedThresholdOutput3.eps}
  // \caption{Segmentation results of the ConnectedThreshold filter for various seed points.}
  // \label{fig:ConnectedThresholdOutput}
  // \end{figure}
  //
  //  It can be noticed that the gray matter is not being completly segmented.
  //  This illustrates the vulnerability of the region growing methods when the
  //  anatomical structures to be segmented do not have a homogeneous
  //  statistical distribution over the image space. You may want to experiment
  //  with different values of the lower and upper thresholds  in order to
  //  verify how the accepted region will extend.
  //
  //  Another option for completing regions is to take advantage of the
  //  functionality provided by the \doxygen{ConnectedThresholdImageFilter} for
  //  managing multiple seeds. The seeds can be passed one by one to the filter
  //  using the \code{AddSeed()} method. You could imagine a User Interface in
  //  which an operator clicks on multiple points of the object to be segmented
  //  and each selected point is passed as a seed to this filter.
  //
  //  Software Guide : EndLatex 




  return 0;

}




