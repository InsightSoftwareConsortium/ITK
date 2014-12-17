/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

//  Software Guide : BeginLatex
//
//  Previous examples have described the basic principles behind the
//  \doxygen{ResampleImageFilter}. Now it's time to have some fun with it.
//
//  Figure \ref{fig:ResampleImageFilterTransformComposition6} illustrates the
//  general case of the resampling process. The origin and spacing of the
//  output image has been selected to be different from those of the input
//  image.  The circles represent the \emph{center} of pixels. They are
//  inscribed in a rectangle representing the \emph{coverage} of this pixel.
//  The spacing specifies the distance between pixel centers along every
//  dimension.
//
//  The transform applied is a rotation of $30$ degrees. It is important to
//  note here that the transform supplied to the
//  \doxygen{ResampleImageFilter} is a \emph{clockwise} rotation. This
//  transform rotates the \emph{coordinate system} of the output image 30
//  degrees clockwise.  When the two images are relocated in a common
//  coordinate system---as in Figure
//  \ref{fig:ResampleImageFilterTransformComposition6}---the result is that
//  the frame of the output image appears rotated 30 degrees
//  \emph{clockwise}.  If the output image is seen with its coordinate system
//  vertically aligned---as in Figure
//  \ref{fig:ResampleImageFilterOutput9}---the image content appears rotated
//  30 degrees \emph{counter-clockwise}. Before continuing to read this
//  section, you may want to meditate a bit on this fact while enjoying a cup
//  of (Colombian) coffee.
//
// \begin{figure}
// \center
// \includegraphics[height=6cm]{ResampleImageFilterInput2x3}
// \includegraphics[height=4cm]{ResampleImageFilterOutput9}
// \itkcaption[Effect of a rotation on the resampling filter.]{Effect of a
// rotation on the resampling filter. Input image at left, output image at
// right.}
// \label{fig:ResampleImageFilterOutput9}
// \end{figure}
//
// \begin{figure}
// \center
// \includegraphics[width=\textwidth]{ResampleImageFilterTransformComposition6}
// \itkcaption[Input and output image placed in a common reference
// system]{Input and output image placed in a common reference system.}
// \label{fig:ResampleImageFilterTransformComposition6}
// \end{figure}
//
//  The following code implements the conditions illustrated in Figure
//  \ref{fig:ResampleImageFilterTransformComposition6} with two differences:
//  the output spacing is $40$ times smaller and there are $40$ times more
//  pixels in both dimensions. Without these changes, few
//  details will be recognizable in the images.  Note that the spacing and
//  origin of the input image should be prepared in advance by using other
//  means since this filter cannot alter the actual content of the
//  input image in any way.
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkAffineTransform.h"
#include "itkNearestNeighborInterpolateImageFunction.h"


int main( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile";
    std::cerr << "  [exampleAction={0,1}]" << std::endl;
    return EXIT_FAILURE;
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
  //  default pixel value to value be distinct from the image background.
  //
  //  \index{itk::ResampleImageFilter!SetDefaultPixelValue()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetDefaultPixelValue( 100 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The spacing is selected here to be 40 times smaller than the one
  //  illustrated in Figure \ref{fig:ResampleImageFilterTransformComposition6}.
  //
  //  \index{itk::ResampleImageFilter!SetOutputSpacing()}
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
  //  We will preserve the orientation of the input image by using the following call.
  //
  //  \index{itk::ResampleImageFilter!SetOutputOrigin()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetOutputDirection( reader->GetOutput()->GetDirection() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Let us now set up the origin of the output image. Note that the values
  //  provided here will be those of the space coordinates for the output
  //  image pixel of index $(0,0)$.
  //
  //  \index{itk::ResampleImageFilter!SetOutputOrigin()}
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
  //  The output image size is defined to be $40$ times the one illustrated
  //  on the Figure \ref{fig:ResampleImageFilterTransformComposition6}.
  //
  //  \index{itk::ResampleImageFilter!SetSize()}
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
  //  Rotations are performed around the origin of physical coordinates---not
  //  the image origin nor the image center. Hence, the process of
  //  positioning the output image frame as it is shown in Figure
  //  \ref{fig:ResampleImageFilterTransformComposition6} requires three
  //  steps.  First, the image origin must be moved to the origin of the
  //  coordinate system. This is done by applying a translation equal to the
  //  negative values of the image origin.
  //
  //  \index{itk::AffineTransform!Translate()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::OutputVectorType translation1;
  translation1[0] =   -origin[0];
  translation1[1] =   -origin[1];
  transform->Translate( translation1 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  In a second step, a rotation of $30$ degrees is performed. In the
  //  \doxygen{AffineTransform}, angles are specified in
  //  \emph{radians}. Also, a second boolean argument is used to specify if
  //  the current modification of the transform should be pre-composed or
  //  post-composed with the current transform content. In this case the
  //  argument is set to \code{false} to indicate that the rotation should be
  //  applied \emph{after} the current transform content.
  //
  //  \index{itk::AffineTransform!Rotate2D()}
  //  \index{itk::AffineTransform!Composition}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const double degreesToRadians = std::atan(1.0) / 45.0;
  transform->Rotate2D( -30.0 * degreesToRadians, false );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The third and final step implies translating the image origin back to
  //  its previous location. This is be done by applying a translation equal
  //  to the origin values.
  //
  //  \index{itk::AffineTransform!Translate()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::OutputVectorType translation2;
  translation2[0] =   origin[0];
  translation2[1] =   origin[1];
  transform->Translate( translation2, false );
  filter->SetTransform( transform );
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


  //  Software Guide : BeginLatex
  //
  //  Figure \ref{fig:ResampleImageFilterOutput9} presents the actual input
  //  and output images of this example as shown by a correct viewer which
  //  takes spacing into account. Note the \emph{clockwise} versus
  //  \emph{counter-clockwise} effect discussed previously between the
  //  representation in Figure
  //  \ref{fig:ResampleImageFilterTransformComposition6} and Figure
  //  \ref{fig:ResampleImageFilterOutput9}.
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  As a final exercise, let's track the mapping of an individual pixel.
  //  Keep in mind that the transformation is initiated by walking through
  //  the pixels of the \emph{output} image. This is the only way to ensure
  //  that the image will be generated without holes or redundant
  //  values. When you think about transformation it is always useful to
  //  analyze things from the output image towards the input image.
  //
  //  Let's take the pixel with index $I=(1,2)$ from the output image. The
  //  physical coordinates of this point in the output image reference system
  //  are $P=( 1 \times 40.0 + 50.0, 2 \times 30.0 + 130.0 ) = (90.0,190.0)$
  //  millimeters.
  //
  //  This point $P$ is now mapped through the \doxygen{AffineTransform} into
  //  the input image space.  The operation subtracts the origin,
  //  applies a $30$ degrees rotation and adds the origin back. Let's follow
  //  those steps.  Subtracting the origin from $P$ leads to
  //  $P1=(40.0,60.0)$, the rotation maps $P1$ to $P2=( 40.0 \times cos
  //  (30.0) + 60.0 \times sin (30.0), 40.0 \times sin(30.0) - 60.0 \times
  //  cos(30.0)) = (64.64,31.96)$.  Finally this point is translated back by
  //  the amount of the image origin. This moves $P2$ to
  //  $P3=(114.64,161.96)$.
  //
  //  The point $P3$ is now in the coordinate system of the input image. The
  //  pixel of the input image associated with this physical position is
  //  computed using the origin and spacing of the input image. $I=( ( 114.64 -
  //  60.0 )/ 20.0 , ( 161 - 70.0 ) / 30.0 )$ which results in $I=(2.7,3.0)$.
  //  Note that this is a non-grid position since the values are non-integers.
  //  This means that the gray value to be assigned to the output image pixel
  //  $I=(1,2)$ must be computed by interpolation of the input image values.
  //
  //  In this particular code the interpolator used is simply a\newline
  //  \doxygen{NearestNeighborInterpolateImageFunction} which will assign the
  //  value of the closest pixel. This ends up being the pixel of index
  //  $I=(3,3)$ and can be seen from Figure
  //  \ref{fig:ResampleImageFilterTransformComposition6}.
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
