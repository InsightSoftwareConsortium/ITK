/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRandomConstIteratorWithIndex.cxx
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
// \doxygen{ImageRandomConstIteratorWithIndex} was developed to randomly
// sample pixel values.  When incremented or decremented, it jumps to a random
// location in its image region.
//
// \index{itk::ImageRandomConstIteratorWithIndex!sample size}
// \index{itk::ImageRandomConstIteratorWithIndex!begin and end positions}
//
// The user must specify a sample size when creating this iterator. The sample
// size defines the end position for the iterator.  \code{IsAtEnd()} returns
// \code{true} when the current sample number equals the sample size.
// \code{IsAtBegin()} returns \code{true} when the current sample number equals
// zero.  Another important difference from other iterators is that
// \doxygen{ImageRandomConstIteratorWithIndex} may visit the same pixel
// location more than once.
//
// \index{itk::ImageRandomConstIteratorWithIndex!example of using|(}
// \index{itk::ImageRandomConstIteratorWithIndex!and statistics}
// Let's use the random iterator to estimate some simple image statistics. The next
// example calculates an estimate of the arithmetic mean of pixel values.
//
// First, include the appropriate header and declare pixel and image types.
//
// Software Guide : EndLatex

#include "itkImage.h"
// Software Guide : BeginCodeSnippet
#include "itkImageRandomConstIteratorWithIndex.h"
// Software Guide : EndCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int main( int argc, char ** argv )
{
  // Verify the number of parameters on the command line.
  if ( argc < 3 )
    {
      std::cerr << "Missing parameters. " << std::endl;
      std::cerr << "Usage: " << std::endl;
      std::cerr << argv[0]
                << " inputImageFile numberOfSamples"
                << std::endl;
      return -1;
    }

// Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 2;

  typedef unsigned short  PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;  
  typedef itk::ImageRandomConstIteratorWithIndex< ImageType >  ConstIteratorType;
// Software Guide : EndCodeSnippet
  
  typedef itk::ImageFileReader< ImageType > ReaderType;

  ImageType::ConstPointer inputImage;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  try
    {
      reader->Update();
      inputImage = reader->GetOutput();
    }
  catch ( itk::ExceptionObject &err)
    {
      std::cout << "ExceptionObject caught !" << std::endl; 
      std::cout << err << std::endl; 
      return -1;
    }

// Software Guide : BeginLatex
//
// The input image has been read as \code{inputImage}.  We now create an
// iterator with a number of samples set by command line argument. The call to
// \code{ReinitializeSeed} seeds the random number generator.  The iterator is
// initialized over the entire valid image region.
//
//  \index{itk::ImageRandomConstIteratorWithIndex!SetNumberOfSamples()}
//  \index{itk::ImageRandomConstIteratorWithIndex!ReinitializeSeed()}
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ConstIteratorType inputIt(  inputImage,  inputImage->GetRequestedRegion() );
  inputIt.SetNumberOfSamples( ::atoi( argv[2]) );
  inputIt.ReinitializeSeed();
// Software Guide : EndCodeSnippet

// Software Guide: BeginLatex
//
// Now take the specified number of samples and calculate their average value.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  float mean = 0.0f;
  for ( inputIt.GoToBegin(); ! inputIt.IsAtEnd(); ++inputIt)
    {
      mean += static_cast<float>( inputIt.Get() );
    }
  mean = mean / ::atof( argv[2] );
  
// Software Guide : EndCodeSnippet
  std::cout << "Mean estimate with " << argv[2] << " samples is " << mean << std::endl;

// Software Guide : BeginLatex
//
// Table~\ref{fig:ImageRandomConstIteratorWithIndexExample} shows the results
// of running this example on several of the data files from
// \code{Insight/Examples/Data} with a range of sample sizes.
//
// \begin{table}
// \begin{center}
// \begin{tabular}[]{rc|c|c|c}
// & \multicolumn{4}{c}{\emph{Sample Size}} \\
// & \code{\textbf{10}} & \code{\textbf{100}} & \code{\textbf{1000}} & \code{\textbf{10000}} \\
// \cline{2-5}
// \code{RatLungSlice1.mha} & 50.5 & 52.4 & 53.0 & 52.4 \\
// \code{RatLungSlice2.mha} & 46.7 & 47.5 & 47.4 & 47.6  \\
// \code{BrainT1Slice.png} & 47.2 & 64.1 & 68.0 & 67.8  \\ 
// \end{tabular}
// \protect\label{fig:ImageRandomConstIteratorWithIndexExample}
// \caption[Estimating mean image value with
// ImageRandomConstIteratorWithIndex]{Estimates of mean image pixel value
// using the ImageRandomConstIteratorWithIndex at different sample sizes.}
// \end{center}
// \end{table}
//
// \index{itk::ImageRandomConstIteratorWithIndex!example of using|)}
// Software Guide : EndLatex
  
  return 0;
}
