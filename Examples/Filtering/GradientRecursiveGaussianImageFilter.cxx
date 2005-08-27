/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    GradientRecursiveGaussianImageFilter.cxx
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

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

//  Software Guide : BeginLatex
//
//  This example illustrates the use of the \doxygen{GradientRecursiveGaussianImageFilter}. 
//
//  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter}
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

//  Software Guide : BeginLatex
//
//  The first step required to use this filter is to include its header
//  file.
//
//  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkGradientRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 4 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile   outputVectorImageFile   sigma" << std::endl;
    return EXIT_FAILURE;
    }

  
  //  Software Guide : BeginLatex
  //
  //  Types should be instantiated based on the pixels of the input and
  //  output images.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef    float    InputPixelType;
  typedef    float    OutputComponentPixelType;

  typedef    itk::CovariantVector< OutputComponentPixelType > OutputPixelType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  With them, the input and output image types can be instantiated.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< InputPixelType,  3 >   InputImageType;
  typedef itk::Image< OutputPixelType, 3 >   OutputImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType >  ReaderType;


  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated using both the input image and the
  //  output image types.
  //
  //  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::GradientRecursiveGaussianImageFilter<
                        InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  A filter object is created by invoking the \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter!New()}
  //  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  an image reader is used as source.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The standard deviation of the Gaussian smoothing kernel is now set.
  //
  //  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter!SetSigma()}
  //  \index{SetSigma()!itk::Gradient\-Recursive\-Gaussian\-Image\-Filter}
  //
  //  Software Guide : EndLatex 
  const double sigma = atof( argv[3] );


  // Software Guide : BeginCodeSnippet
  filter->SetSigma( sigma );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally the filter is executed by invoking the \code{Update()} method.
  //
  //  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter!Update()}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  If connected to other filters in a pipeline, this filter will
  //  automatically update when any downstream filters are updated.  For
  //  example, we may connect this gradient magnitude filter to an image file
  //  writer and then update the writer.
  //
  //  Software Guide : EndLatex 


  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
 

  // Software Guide : BeginCodeSnippet
  writer->SetInput( filter->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet
  

  //  Software Guide : BeginLatex
  // 
  // 
  //
  //  Software Guide : EndLatex 


  return EXIT_SUCCESS;
}

