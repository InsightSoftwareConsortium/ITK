/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ResampleImageFilter2.cxx
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
//  When using the ResampleImageFilter it is important to keep in mind that
//  \code{itk::Image} has an internal transform that indicates how to map from
//  pixels to physical space. By default this internal transform is configured
//  using the origin and spacing of the image. During the computation of the
//  resampled image all the pixels in the output region are visited. This visit
//  is performed using \code{ImageIterators} which walk in the integer
//  grid-space of the image. For each pixel, the internal
//  \code{IndexToPhysicalPoint} transform of the image is used to compute the
//  corresponding space coordinates.
//
//  For example, the pixel of index $I=(20,50)$ in an image of origin $O=(19.0,
//  29.0)$ and pixel spacing $S=(1.3,1.5)$ corresponds to the spatial position
// 
//  \begin{equation}
//  P = I \times S + O
//  \end{equation}
//
//  Which in this case leads to 
//  $P=( 20 \times 1.3 + 19.0, 50 \times 1.5 + 29.0 )$ and finally $P=(45.0, 104.0)$
//
//  The space coordinates of $P$ are mapped using the transform $T$ supplied to
//  the \code{ResampleImageFilter} in order to map the point $P$ to the input
//  image space point $Q = T(P)$.  Finally, the internal transform of the input
//  image is used to find the grid point corresponding to the space point $Q$.
//
//  The whole process is illustrated in figure
//  \ref{fig:ResampleImageFilterTransformComposition}. In order to correctly
//  interpret the process of the ResampleImageFilter you should be aware of the
//  origin and spacing settings of both the input and output images.
//
//  \index{itk::ResampleImageFilter|Image internal transform}
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkAffineTransform.h"
#include "itkNearestNeighborInterpolateImageFunction.h"



int main( int argc, char ** argv )
{


  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile"; 
    std::cerr << "  [exampleAction={0,1,2,3}]" << std::endl;
    return 1;
    }

  int exampleAction = 0;
 
  if( argc >= 4 )
    {
    exampleAction = atoi( argv[3] );
    }

  const     unsigned int   Dimension = 2;
  typedef   unsigned char  InputPixelType;
  typedef   unsigned char  OutputPixelType;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;


  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  typedef itk::ResampleImageFilter<
                  InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  typedef itk::AffineTransform< double, Dimension >  TransformType;

  TransformType::Pointer transform = TransformType::New();
 

  typedef itk::NearestNeighborInterpolateImageFunction< 
                       InputImageType, double >  InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
 
  filter->SetInterpolator( interpolator );




  //  Software Guide : BeginLatex
  //
  //  In order to facilitate the interpretation of the transform we set the
  //  default pixel value to a distintly visible gray level.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetDefaultPixelValue( 50 );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  Let's set up a uniform spacing 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  double spacing[ Dimension ];
  spacing[0] = 1.0; // pixel spacing in millimeters along X
  spacing[1] = 1.0; // pixel spacing in millimeters along Y

  filter->SetOutputSpacing( spacing );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  Let us now set up a non-zero origin for the output image. Note that the
  //  values provided here will be those of the space coordinates for the pixel
  //  of index $(0,0)$.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  double origin[ Dimension ];
  origin[0] = 30.0;  // X space coordinate of origin
  origin[1] = 40.0;  // Y space coordinate of origin

  filter->SetOutputOrigin( origin );
  // Software Guide : EndCodeSnippet


  InputImageType::SizeType   size;

  size[0] = 300;  // number of pixels along X
  size[1] = 300;  // number of pixels along Y

  filter->SetSize( size );

  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );


  //  Software Guide : BeginLatex
  //
  //  We set the transform to identity in order to better appreciate the effect
  //  of the origin selection.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  transform->SetIdentity();

  filter->SetTransform( transform );
  // Software Guide : EndCodeSnippet

  
  if( exampleAction == 0 )
    {
    writer->Update();
    }

  //  Software Guide : BeginLatex
  //
  //  The output resulting from these filter settings is analyzed in figure
  //  \ref{fig:ResampleImageFilterTransformComposition}
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=12cm]{ResampleImageFilterTransformComposition.eps}
  // \caption{Effect of selecting the origin of the output image}
  // \label{fig:ResampleImageFilterTransformComposition}
  // \end{figure}
  //
  //  With this change we can better appreciate the effect of the previous
  //  translation transform on the image resampling. Figure
  //  \ref{fig:ResampleImageFilterTransformComposition} illustrates how the
  //  output image point with index $I=(0,0)$ has space coordinates
  //  $P=(30,40)$.  The identity transform maps this point to $Q=(30,40)$ in
  //  the input image space. Because the input image in this case happens to
  //  have spacing $(1.0,1.0)$ and origin $(0.0,0.0)$, the physical point
  //  $Q=(30,40)$ maps to the pixel with index $I=(30,40)$.
  //
  //  Software Guide : EndLatex 


  
  return 0;

}

