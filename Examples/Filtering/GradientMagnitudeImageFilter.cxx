/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    GradientMagnitudeImageFilter.cxx
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
//  The magnitude of the image gradient is extensively used in image analysis
//  mainly to help in the determination of object countours and the separation
//  of homogeneous regions. The \code{itk::GradientMagnitudeImageFilter}
//  computes the magnitude of the image gradient at each pixel location by a
//  simple finite differences approach. In the case of 2D, the computation is
//  equivalent to convolving the image with masks of type
//
//  \begin{center}
//  \begin{picture}(200,50)
//  \put( 5.0,32.0){\framebox(30.0,15.0){-1}} 
//  \put(35.0,32.0){\framebox(30.0,15.0){0}} 
//  \put(65.0,32.0){\framebox(30.0,15.0){1}} 
//  \put(105.0,17.0){\framebox(20.0,15.0){1}} 
//  \put(105.0,32.0){\framebox(20.0,15.0){0}} 
//  \put(105.0,47.0){\framebox(20.0,15.0){-1}} 
//  \end{picture}
//  \end{center}
//
//  adding the sum of their squares and computing the square root of the sum.
//
//  This filter will work on images off any dimension thanks to the internal
//  use of NeighborhoodIterators and NeighborhoodOperators.
//
//  \index{itk::GradientMagnitudeImageFilter|textbf}
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


//  Software Guide : BeginLatex
//
//  The first step required for using this filter is to include its header file
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkGradientMagnitudeImageFilter.h"
// Software Guide : EndCodeSnippet




int main( int argc, char ** argv )
{


  if( argc < 3 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile " << std::endl;
    return 1;
    }

  
  //  Software Guide : BeginLatex
  //
  //  Types should be choosen for the pixels of the input and output images.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef float                              InputPixelType;
  typedef float                              OutputPixelType;
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
  //  The gradient magnitude filter type is now instantiated using both the
  //  input image and the output image types.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::GradientMagnitudeImageFilter<
               InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet



  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  A filter object is created by invoking the \code{New()} method and
  //  assigning the result to a SmartPointer.
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
  //  Finally the filter is executed by invoking the \code{Update()} method.
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  filter->Update();
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  If the output of this filter has been connected to other filters down the
  //  pipeline, updating any of the downstream filters would have triggered the
  //  execution of this one. For example, a writer filter could have been used
  //  after the gradient magnitude.
  //
  //  Software Guide : EndLatex 


  typedef itk::ImageFileWriter< InputImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
 
  // Software Guide : BeginCodeSnippet
  writer->SetInput( filter->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  Attention should be paid to the image type choosen for representing the
  //  output image since the dynamic range of gradient magnitudes is usually
  //  quite smaller than the dynamic range of the input image intensities. As
  //  always, there are exceptions to this rule, in particular when synthetic
  //  images with high contrast objects are provided as input.
  //
  //  This filter do not apply any type of smoothing before computing the
  //  gradients. This makes it very sensitive to noise and probably not the
  //  best choise for scale space analysis. 
  //
  //  Software Guide : EndLatex 




  return 0;

}

