/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ResampleImageFilter.cxx
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
//  Resampling an image is a very important task in image analysis. It is
//  specially important in the frame of image registration. The
//  \code{itk::ResampleImageFilter} implements image resampling through the use
//  of \code{itk::Transforms}. The inputs expected by this filter are an image,
//  a transform and an interpolator. The space coordinates of the image are
//  mapped through the transform in order to generate a new image. The extent
//  and spacing of the resulting image are selected by the user. Resampling is
//  performed in space coordinates, not pixel/grid coordinates. It is quite
//  important to ensure that image spacing is properly set on the images
//  involved. The interpolator is required since the mapping from one space to
//  the other will often require to evaluate the intensity of the image in a
//  non-grid position. 
//
//  \index{itk::ResampleImageFilter|textbf}
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"



//  Software Guide : BeginLatex
//
//  The header file corresponding to this filter should be included first. 
//
//  \index{itk::ResampleImageFilter!header|textbf}
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkResampleImageFilter.h"
// Software Guide : EndCodeSnippet




//  Software Guide : BeginLatex
//
//  The header files corresponding to the transform and interpolator should
//  also be included.
//
//  \index{itk::ResampleImageFilter!header|textbf}
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkAffineTransform.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
// Software Guide : EndCodeSnippet




int main( int argc, char ** argv )
{


  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile   outputImageFile1  outputImageFile2" << std::endl;
    return 1;
    }


  //  Software Guide : BeginLatex
  //
  //  The dimension and pixel types for input and output image must be defined
  //  and with them the image types can be instantiated.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  const     unsigned int   Dimension = 2;
  typedef   unsigned char  InputPixelType;
  typedef   unsigned char  OutputPixelType;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );



  //  Software Guide : BeginLatex
  //
  //  Using the image and transform types it is now possible to instantiate the
  //  filter type and create the filter object. 
  //
  //  \index{itk::ResampleImageFilter!instantiation}
  //  \index{itk::ResampleImageFilter!New()}
  //  \index{itk::ResampleImageFilter!Pointer}
  // 
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ResampleImageFilter<
                  InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The transform type is instantiated typically using the image dimension
  //  and the type used for representing space coordinates.
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  typedef itk::AffineTransform< double, Dimension >  TransformType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  An instance of the transform object is instantiated and passed to the
  //  resample filter.
  //
  //  \index{itk::ResampleImageFilter!SetTransform()}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  TransformType::Pointer transform = TransformType::New();
 
  filter->SetTransform( transform );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  The interpolator type is instantiated using the full image type and the
  //  type used for representing space coordinates.
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  typedef itk::NearestNeighborInterpolateImageFunction< 
                       InputImageType, double >  InterpolatorType;
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  An instance of the interpolator object is instantiated and passed to the
  //  resample filter.
  //
  //  \index{itk::ResampleImageFilter!SetInterpolator()}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
 
  filter->SetInterpolator( interpolator );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  Given that some pixels of the output image may end up being mapped
  //  outside the extent of the input image it is necessary to decide what
  //  values to assign to them. This is done by invoking the
  //  \code{SetDefaultPixelValue()} method.
  //
  //  \index{itk::ResampleImageFilter!SetDefaultPixelValue()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetDefaultPixelValue( 0 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The sampling grid of the output space is specified with the spacing along
  //  each dimension and the origin.
  //
  //  \index{itk::ResampleImageFilter!SetOutputOrigin()}
  //  \index{itk::ResampleImageFilter!SetOutputSpacing()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  double spacing[ Dimension ];
  spacing[0] = 1.0; // pixel spacing in millimeters along X
  spacing[1] = 1.0; // pixel spacing in millimeters along Y

  filter->SetOutputSpacing( spacing );

  double origin[ Dimension ];
  origin[0] = 0.0;  // X space coordinate of origin
  origin[1] = 0.0;  // Y space coordinate of origin

  filter->SetOutputOrigin( origin );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The extension of the sampling grid on the output image is defined by a
  //  \code{SizeType} and is set using the \code{SetSize()} method.
  //
  //  \index{itk::ResampleImageFilter!SetOutputRegion()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  InputImageType::SizeType   size;

  size[0] = 300;  // number of pixels along X
  size[1] = 300;  // number of pixels along Y

  filter->SetSize( size );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  The input to the filter can be taken from any other filter, for example a
  //  reader. The output can be passed down the pipeline to other filters, for
  //  example a writer. An update call on any downstream filter will trigger
  //  the execution of the mean filter.
  //
  //  \index{itk::ResampleImageFilter!SetInput()}
  //  \index{itk::ResampleImageFilter!GetOutput()}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  // 
  // \begin{figure}
  // \center
  // \includegraphics[height=6cm]{BrainProtonDensitySlice.eps}
  // \includegraphics[height=6cm]{ResampleImageFilterOutput1.eps}
  // \caption{Effect of the Resample filter}
  // \label{fig:ResampleImageFilterOutput1}
  // \end{figure}
  //
  //  Figure \ref{fig:ResampleImageFilterOutput1} illustrates the effect of
  //  this filter on a slice of MRI brain image using an affine transform
  //  containing an identity transform. Note that any analysis of the behavior
  //  of this filter must be done on the space coordinate system in
  //  millimeters, not in the frame of the sampling grid in pixels. The figure
  //  shows the resulting image in the lower left quarter of the extent. This
  //  may seem odd if analyzed on terms of the grid but is quite clear when
  //  seen in the frame of space coordinates.  Figure
  //  \ref{fig:ResampleImageFilterOutput1} is particularly missleading because
  //  the images are rescaled to fit nicely on the text of this book.  Figure
  //  \ref{fig:ResampleImageFilterOutput1Analysis} clarifies the situation. It
  //  shows the two same images placed on a equally scaled coordinate system.
  //  It becomes clear here that an identity transform is being used to map the
  //  image data, and that simply, we has requested to sample additional empty
  //  space around the image. The input image is 181x217 pixels in size and we
  //  have requested an output of 300x300 pixels. The spacing of the input
  //  image is 1.0 and the spacing of the output image is 1.0x1.0.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=12cm]{ResampleImageFilterOutput1Analysis.eps}
  // \caption{Analysis of the resample image done in a common coordinate system.}
  // \label{fig:ResampleImageFilterOutput1Analysis}
  // \end{figure}
  //
  //  Software Guide : EndLatex 




  //  Software Guide : BeginLatex
  //
  //  Let's now set values on the transform. Note that the transform supplied
  //  is the one that maps points from the output space to the input space.
  //  The following code sets up a translation.
  //
  //  \index{itk::AffineTransform!Translate()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  TransformType::OutputVectorType translation;

  translation[0] = 30;  // X translation in millimeters
  translation[1] = 50;  // Y translation in millimeters

  transform->Translate( translation );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The execution of the filter is triggered by updating the writer at the
  //  end of the pipeline.
  //
  //  Software Guide : EndLatex 

  writer->SetFileName( argv[3] );

  // Software Guide : BeginCodeSnippet
  writer->Update();  
  // Software Guide : EndCodeSnippet



  return 0;

}

