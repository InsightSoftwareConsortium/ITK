/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ResampleImageFilter3.cxx
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
//  Now that the principles behind the \code{ResampleImageFilter} has been
//  layed out, let's have some fun with it !
//
//  Figure \ref{fig:ResampleImageFilterTransformComposition6} illustrates the
//  generic case of the resampling process. The origin and spacing of the
//  output image has been choosen to be different from those of the input
//  image.  The circles represent the \emph{center} of pixels. They are
//  inscribed in a rectangle representing the \emph{coverage} of this pixel.
//  The spacing specifies the distance between pixel centers along every
//  dimension.
//
//  The transform applied is a rotation of $30$ degrees. It is important to
//  note here that the transform supplied to the \code{ResampleImageFilter} is
//  a \emph{counter-clockwise} rotation. This transform rotates the
//  \emph{coordinate system} of the output image 30 degrees counter-clockwise.
//  When the two images are relocated in a common coordinate system --- as in
//  figure \ref{fig:ResampleImageFilterTransformComposition6} --- the result is
//  that the frame of the output image appears rotated 30 degrees
//  \emph{clockwise}.  If the output image is seen with its coordinate system
//  straighten up, the image content appears rotated 30 degrees
//  \emph{counter-clockwise}. Before continue reading this section, you may
//  want to meditate a bit on this fact while enjoying a cup of coffee.
//
// \begin{figure}
// \center
// \includegraphics[width=12cm]{ResampleImageFilterTransformComposition6.eps}
// \caption{Effect of selecting the origin of the output image}
// \label{fig:ResampleImageFilterTransformComposition6}
// \end{figure}
//
//  The following code implements these condition with the only difference of
//  selecting a spacing 40 times smaller and a number of pixels 40 times larger
//  in both dimensions. Without these change, few detail will be recognizable
//  on the images.  Note that the spacing and origin of the input image should
//  be prepared in advance by using other means since this filter cannot alter
//  in any way the actual content of the input image.
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
    std::cerr << "  [exampleAction={0,1}]" << std::endl;
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
  filter->SetDefaultPixelValue( 100 );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The spacing is selected here to be 40 times smaller than the one
  //  illustrated in figure \ref{fig:ResampleImageFilterTransformComposition6}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  double spacing[ Dimension ];
  spacing[0] = 40.0 / 40.0; // pixel spacing in millimeters along X
  spacing[1] = 30.0 / 40.0; // pixel spacing in millimeters along Y

  filter->SetOutputSpacing( spacing );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  Let us now set up the origin of the output image. Note that the values
  //  provided here will be those of the space coordinates for the pixel of
  //  index $(0,0)$.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  double origin[ Dimension ];
  origin[0] =  50.0;  // X space coordinate of origin
  origin[1] = 130.0;  // Y space coordinate of origin

  filter->SetOutputOrigin( origin );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  The output image size is defined to be 40 times the one illustrated on
  //  the figure.
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  InputImageType::SizeType   size;

  size[0] = 5 * 40;  // number of pixels along X
  size[1] = 4 * 40;  // number of pixels along Y

  filter->SetSize( size );
  // Software Guide : EndCodeSnippet





  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );




  //  Software Guide : BeginLatex
  //
  //  Rotations are performed around the origin of coordinates. The process of
  //  positioning the output image frame as it is shown in the figure reguires
  //  three steps. First, the image origin must be moved to the origin of the
  //  coordinate system, this is done by applying a translationof
  //  $(-50,-130.0))$. In a second step, a rotation of 30 degrees is performed.
  //  The third and final step implies to translate back the origing to the
  //  previous location, which can be done with a translation of $(50.0,130)$.
  //  Note that the AffineTransform uses a second boolean argument to specify
  //  if the current modification of the transform should be pre-composed or
  //  post-composed with the current transform content.
  //
  //  Angles are specified in Radians to the Affine transform.
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  TransformType::OutputVectorType translation1;

  translation1[0] =   -50.0;
  translation1[1] =  -130.0;
  
  transform->Translate( translation1 );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  const double degreesToRadians = atan(1.0) / 45.0;
  transform->Rotate2D( 30.0 * degreesToRadians, false );

  filter->SetTransform( transform );
  // Software Guide : EndCodeSnippet

  
   // Software Guide : BeginCodeSnippet
  TransformType::OutputVectorType translation2;

  translation2[0] =    50.0;
  translation2[1] =   130.0;
  
  transform->Translate( translation2, false );
  // Software Guide : EndCodeSnippet

 

  
  if( exampleAction == 0 )
    {
      try 
        {
        writer->Update();
        }
      catch( itk::ExceptionObject & excep )
        {
        std::cerr << "Exception catched !" << std::endl;
        std::cerr << excep << std::endl;
        }
    }


  

  return 0;


}

