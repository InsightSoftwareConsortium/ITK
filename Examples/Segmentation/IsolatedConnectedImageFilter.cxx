/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    IsolatedConnectedImageFilter.cxx
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
// \doxygen{IsolatedConnectedImageFilter}.  This filter is a close variant of
// the \doxygen{ConnectedThresholdImageFilter}.  In this filter two seeds and a
// lower threshold are provided by the user. The filter will grow a region
// connected to the first seed and \textbf{not connected} to the second one. In
// order to do this, the filter finds an intensity value that could be used as
// upper threshold for the first seed. A binary search is used to find the
// value that separates both seeds.
//
// The current code follows closely the previous examples. Only the relevant
// pieces of code are hightlighted here. 
//
// Software Guide : EndLatex 

//  Software Guide : BeginLatex
//  
//  The header of the \doxygen{IsolatedConnectedImageFilter} is included below.
//
//  \index{itk::IsolatedConnectedImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkIsolatedConnectedImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkImage.h"
#include "itkCastImageFilter.h"
#include "itkCurvatureFlowImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


int main( int argc, char *argv[] )
{


  if( argc < 7 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage seedX1 seedY1";
    std::cerr << " lowerThreshold seedX2 seedY2" << std::endl;
    return 1;
    }




  //  Software Guide : BeginLatex
  //  
  //  We declare the image type using a pixel type and a particular
  //  dimension. 
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



  typedef   itk::CurvatureFlowImageFilter< 
                               InternalImageType, 
                               InternalImageType >  CurvatureFlowImageFilterType;


    CurvatureFlowImageFilterType::Pointer smoothing = 
                         CurvatureFlowImageFilterType::New();



  //  Software Guide : BeginLatex
  //  
  //  The type of the \doxygen{IsolatedConnectedImageFilter} is instantiated in
  //  the lines below. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  itk::IsolatedConnectedImageFilter< 
                                    InternalImageType, 
                                    InternalImageType > ConnectedFilterType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  One filter of this class is constructed using the \code{New()} method. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ConnectedFilterType::Pointer isolatedConnected = ConnectedFilterType::New();
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  Now it is time to connect the pipeline. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  smoothing->SetInput( reader->GetOutput() );

  isolatedConnected->SetInput( smoothing->GetOutput() );

  caster->SetInput( isolatedConnected->GetOutput() );

  writer->SetInput( caster->GetOutput() );
  // Software Guide : EndCodeSnippet


  smoothing->SetNumberOfIterations( 5 );
  smoothing->SetTimeStep( 0.125 );




  //  Software Guide : BeginLatex
  //
  //  The \doxygen{IsolatedConnectedImageFilter} expects as parameters one
  //  threshold and two seeds. We take all of them from the command line
  //  arguments.
  //
  //  \index{itk::IsolatedConnectedImageFilter!SetLower()}
  //  \index{itk::IsolatedConnectedImageFilter!SetSeed1()}
  //  \index{itk::IsolatedConnectedImageFilter!SetSeed2()}
  //
  //  Software Guide : EndLatex 


  InternalImageType::IndexType  indexSeed1;
  
  indexSeed1[0] = atoi( argv[3] );
  indexSeed1[1] = atoi( argv[4] );

  const InternalPixelType lowerThreshold = atof( argv[5] );

  InternalImageType::IndexType  indexSeed2;
  
  indexSeed2[0] = atoi( argv[6] );
  indexSeed2[1] = atoi( argv[7] );


  // Software Guide : BeginCodeSnippet
  isolatedConnected->SetLower(  lowerThreshold  );
  
  isolatedConnected->SetSeed1( indexSeed1 );
  isolatedConnected->SetSeed2( indexSeed2 );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  As in the \doxygen{ConnectedThresholdImageFilter} we must now provide the
  //  intensity value to be used for the output pixels accepted in the region
  //  and at least one seed point to define the initial region.
  //
  //  \index{itk::IsolatedConnectedImageFilter!SetReplaceValue()}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  isolatedConnected->SetReplaceValue( 255 );
  // Software Guide : EndCodeSnippet
 

  
  //  Software Guide : BeginLatex
  //  
  //  The invocation of the \code{Update()} method on the writer triggers the
  //  execution of the pipeline.  
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
  //  The intensity value allowing us to separate both regions can be recovered
  //  with the method \code{GetIsolatedValue()}
  //
  //  \index{itk::IsolatedConnectedImageFilter!GetIsolatedValue()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  std::cout << "Isolated Value Found = ";
  std::cout << isolatedConnected->GetIsolatedValue()  << std::endl;
  // Software Guide : EndCodeSnippet





  
  //  Software Guide : BeginLatex
  //
  //  Let's now run this example using as input the image
  //  \code{BrainProtonDensitySlice.png} provided in the directory
  //  \code{Insight/Examples/Data}. We can easily segment the major
  //  anatomical structures by providing seed pairs in the appropriate
  //  locations and defining values for the lower threshold. It is
  //  important to keep in mind in this and the previous examples that
  //  the segmentation is being performed in the smoothed version of
  //  the image. The selection of threshold values should henceforth
  //  be done in the smoothed image since the distribution of
  //  intensities could be quite different from that of the input
  //  image.  As a reminder of this fact, Figure
  //  \ref{fig:IsolatedConnectedImageFilterOutput} presents, from left
  //  to right, the input image and the result of smoothing with the
  //  \doxygen{CurvatureFlowImageFilter} followed by segmentation
  //  results.
  //
  //  This filter is intended to be used in cases where adjacent anatomical
  //  structures are difficult to separate. Selecting one seed in one structure
  //  and the other seed in the adjacent structure creates the appropriate
  //  setup for computing the threshold that will separate both structures.
  //  The following table presents the parameters used to obtain the images
  //  shown in Figure~\ref{fig:IsolatedConnectedImageFilterOutput}.
  //
  //  \begin{center}
  //  \begin{tabular}{|l|c|c|c|c|c|}
  //  \hline
  //  Adjacent Structures & Seed1 & Seed2 & Lower & Isolated value found & Output Image \\ \hline \\ \hline
  //  Gray matter vs White matter & $(61,140)$ & $(63,43)$ & $150$ & $183.31$ & 
  //  Third from left in Figure \ref{fig:IsolatedConnectedImageFilterOutput} \\ \hline
  //  \end{tabular}
  //  \end{center}
  //
  // \begin{figure} \center
  // \includegraphics[width=0.32\textwidth]{BrainProtonDensitySlice.eps}
  // \includegraphics[width=0.32\textwidth]{IsolatedConnectedImageFilterOutput0.eps}
  // \includegraphics[width=0.32\textwidth]{IsolatedConnectedImageFilterOutput1.eps}
  // \caption{Segmentation results of the IsolatedConnectedImageFilter.}
  // \label{fig:IsolatedConnectedImageFilterOutput}
  // \end{figure}
  //
  //
  //  Software Guide : EndLatex 




  return 0;

}




