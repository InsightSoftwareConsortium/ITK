/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    BinomialBlurImageFilter.cxx
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
//  The \doxygen{BinomialBlurImageFilter} computes a nearest neighbor average
//  along each dimension. The process is repeated a number of times as selected
//  by the user. In principle, after a large number of iterations the result
//  will approach the convolution with a Gaussian. 
//
//  \index{itk::BinomialBlurImageFilter|textbf}
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

//  Software Guide : BeginLatex
//
//  The first step required for using this filter is to include its header file
//
//  \index{itk::BinomialBlurImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkBinomialBlurImageFilter.h"
// Software Guide : EndCodeSnippet




int main( int argc, char ** argv )
{


  if( argc < 4 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  numberOfRepetitions" << std::endl;
    return 1;
    }

  
  //  Software Guide : BeginLatex
  //
  //  Types should be choosen for the pixels of the input and output images.
  //  Image types can be instantiated using the pixel type and dimension.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef    float    InputPixelType;
  typedef    float    OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet




  typedef itk::ImageFileReader< InputImageType >  ReaderType;




  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated using both the input image and the
  //  output image types. Then a filter object is created.
  //
  //  \index{itk::BinomialBlurImageFilter!instantiation}
  //  \index{itk::BinomialBlurImageFilter!New()}
  //  \index{itk::BinomialBlurImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::BinomialBlurImageFilter<
                 InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet



  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  const unsigned int repetitions = atoi( argv[3] );



  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  an image reader is used as source. The number of repetitions is set with
  //  the \code{SetRepetitions()} method. Computation time will
  //  increase linearly with the number of repetitions selected. Finally, the
  //  filter can be executed by calling the \code{Update()} method.
  //
  //  \index{itk::BinomialBlurImageFilter!Update()}
  //  \index{itk::BinomialBlurImageFilter!SetInput()}
  //  \index{itk::BinomialBlurImageFilter!SetRepetitions()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  filter->SetRepetitions( repetitions );
  filter->Update();
  // Software Guide : EndCodeSnippet




  // 
  // This section connects the filter output to a writer 
  //
  typedef unsigned char WritePixelType;

  typedef itk::Image< WritePixelType, 2 > WriteImageType;

  typedef itk::RescaleIntensityImageFilter< 
               OutputImageType, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  

  typedef itk::ImageFileWriter< WriteImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
 
  rescaler->SetInput( filter->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();
  



  //  Software Guide : BeginLatex
  //  
  // \begin{figure}
  // \center
  // \includegraphics[width=6cm]{BrainProtonDensitySlice.eps}
  // \includegraphics[width=6cm]{BinomialBlurImageFilterOutput.eps}
  // \caption[BinomialBlurImageFilter output]{Effect of the
  // BinomialBlurImageFilter on a slice from a MRI Proton Density image  of the
  // brain.}
  // \label{fig:BinomialBlurImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:BinomialBlurImageFilterInputOutput} illustrates the
  //  effect of this filter on a MRI proton density image of the brain. 
  //  
  //  Note that the sigma of the equivalent Gaussian is fixed. The effect of
  //  every iteration of this filter on the spatial spectrum will be to
  //  multiply it with a sinus cardinal function.
  //
  //  Software Guide : EndLatex 




  return 0;

}

