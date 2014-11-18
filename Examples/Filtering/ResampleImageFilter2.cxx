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
//  During the computation of the resampled image all the pixels in the
//  output region are visited. This visit is performed using
//  \code{ImageIterators} which walk in the integer grid-space of the
//  image. For each pixel, we need to convert grid position to space
//  coordinates using the image spacing and origin.
//
//  For example, the pixel of index $I=(20,50)$ in an image of origin
//  $O=(19.0, 29.0)$ and pixel spacing $S=(1.3,1.5)$ corresponds to the
//  spatial position
//
//  \begin{equation}
//  P[i] = I[i] \times S[i] + O[i]
//  \end{equation}
//
//  which in this case leads to $P=( 20 \times 1.3 + 19.0, 50 \times 1.5 +
//  29.0 )$ and finally $P=(45.0, 104.0)$
//
//  The space coordinates of $P$ are mapped using the transform $T$ supplied
//  to the \doxygen{ResampleImageFilter} in order to map the point $P$ to the
//  input image space point $Q = T(P)$.
//
//  The whole process is illustrated in Figure
//  \ref{fig:ResampleImageFilterTransformComposition1}. In order to correctly
//  interpret the process of the ResampleImageFilter you should be aware of the
//  origin and spacing settings of both the input and output images.
//
//  \index{itk::ResampleImageFilter!Image internal transform}
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
    std::cerr << "  [exampleAction={0,1,2,3,4}]" << std::endl;
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
  //  default pixel value to a value distinct from the image background.
  //
  //  \index{itk::ResampleImageFilter!SetDefaultPixelValue()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetDefaultPixelValue( 50 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Let's set up a uniform spacing for the output image.
  //
  //  \index{itk::ResampleImageFilter!SetOutputSpacing()}
  //
  //  Software Guide : EndLatex

    {
    // Software Guide : BeginCodeSnippet
    // pixel spacing in millimeters along X & Y
    const double spacing[ Dimension ] = { 1.0, 1.0 };
    filter->SetOutputSpacing( spacing );
    // Software Guide : EndCodeSnippet
    }

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
  //  Additionally, we will specify a non-zero origin. Note that the values
  //  provided here will be those of the space coordinates for the pixel of
  //  index $(0,0)$.
  //
  //  \index{itk::ResampleImageFilter!SetOutputOrigin()}
  //
  //  Software Guide : EndLatex

    {
    // Software Guide : BeginCodeSnippet
    // space coordinate of origin
    const double origin[ Dimension ] = { 30.0, 40.0 };
    filter->SetOutputOrigin( origin );
    // Software Guide : EndCodeSnippet
    }


  InputImageType::SizeType   size;

  size[0] = 300;  // number of pixels along X
  size[1] = 300;  // number of pixels along Y
  filter->SetSize( size );

  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );


  //  Software Guide : BeginLatex
  //
  //  We set the transform to identity in order to better appreciate the
  //  effect of the origin selection.
  //
  //  \index{itk::AffineTransform!SetIdentity()}
  //  \index{itk::ResampleImageFilter!SetTransform()}
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
  //  The output resulting from these filter settings is analyzed in Figure
  //  \ref{fig:ResampleImageFilterTransformComposition1}.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=\textwidth]{ResampleImageFilterTransformComposition1}
  // \itkcaption[ResampleImageFilter selecting the origin of the output
  // image]{ResampleImageFilter selecting the origin of the output image.}
  // \label{fig:ResampleImageFilterTransformComposition1}
  // \end{figure}
  //
  //  In the figure, the output image point with index $I=(0,0)$ has space
  //  coordinates $P=(30,40)$.  The identity transform maps this point to
  //  $Q=(30,40)$ in the input image space. Because the input image in this
  //  case happens to have spacing $(1.0,1.0)$ and origin $(0.0,0.0)$, the
  //  physical point $Q=(30,40)$ maps to the pixel with index $I=(30,40)$.
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  The code for a different selection of origin and image size is
  //  illustrated below.  The resulting output is presented in Figure
  //  \ref{fig:ResampleImageFilterTransformComposition2}.
  //
  //  \index{itk::ResampleImageFilter!SetSize()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  size[0] = 150;  // number of pixels along X
  size[1] = 200;  // number of pixels along Y
  filter->SetSize( size );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  \index{itk::ResampleImageFilter!SetOutputOrigin()}
  //
  //  Software Guide : EndLatex

    {
    // Software Guide : BeginCodeSnippet
    // space coordinate of origin
    const double origin[ Dimension ] = { 60.0, 30.0 };
    filter->SetOutputOrigin( origin );
    // Software Guide : EndCodeSnippet
    }


  if( exampleAction == 1 )
    {
    writer->Update();
    }


  //  Software Guide : BeginLatex
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=\textwidth]{ResampleImageFilterTransformComposition2}
  // \itkcaption[ResampleImageFilter origin in the output
  // image]{ResampleImageFilter origin in the output image.}
  // \label{fig:ResampleImageFilterTransformComposition2}
  // \end{figure}
  //
  //  The output image point with index $I=(0,0)$ now has space coordinates
  //  $P=(60,30)$.  The identity transform maps this point to $Q=(60,30)$ in
  //  the input image space. Because the input image in this case happens to
  //  have spacing $(1.0,1.0)$ and origin $(0.0,0.0)$, the physical point
  //  $Q=(60,30)$ maps to the pixel with index $I=(60,30)$.
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  Let's now analyze the effect of a non-zero origin in the input image.
  //  Keeping the output image settings of the previous example, we modify
  //  only the origin values on the file header of the input image. The new
  //  origin assigned to the input image is $O=(50,70)$. An identity
  //  transform is still used as input for the ResampleImageFilter. The
  //  result of executing the filter with these parameters is presented in
  //  Figure \ref{fig:ResampleImageFilterTransformComposition3}.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=\textwidth]{ResampleImageFilterTransformComposition3}
  // \itkcaption[ResampleImageFilter selecting the origin of the input
  // image]{Effect of selecting the origin of the input
  // image with ResampleImageFilter.} \label{fig:ResampleImageFilterTransformComposition3}
  // \end{figure}
  //
  //  The pixel with index $I=(56,120)$ on the output image has coordinates
  //  $P=(116,150)$ in physical space. The identity transform maps $P$ to the
  //  point $Q=(116,150)$ on the input image space. The coordinates of $Q$ are
  //  associated with the pixel of index $I=(66,80)$ on the input image.
  //
  //  Software Guide : EndLatex


  if( exampleAction == 2 )
    {
    writer->Update();
    }


  //  Software Guide : BeginLatex
  //
  //  Now consider the effect of the output spacing on the process of image
  //  resampling.  In order to simplify the analysis, let's set the origin
  //  back to zero in both the input and output images.
  //
  //  \index{itk::ResampleImageFilter!SetOutputOrigin()}
  //
  //  Software Guide : EndLatex

    {
    // Software Guide : BeginCodeSnippet
    // space coordinate of origin
    const double origin[ Dimension ] = { 0.0, 0.0 };
    filter->SetOutputOrigin( origin );
    // Software Guide : EndCodeSnippet
    }

  //  Software Guide : BeginLatex
  //
  //  We then specify a non-unit spacing for the output image.
  //
  //  \index{itk::ResampleImageFilter!SetOutputSpacing()}
  //
  //  Software Guide : EndLatex

    {
    // Software Guide : BeginCodeSnippet
    // pixel spacing in millimeters
    const double spacing[ Dimension ] = { 2.0, 3.0 };
    filter->SetOutputSpacing( spacing );
    // Software Guide : EndCodeSnippet
    }

  //  Software Guide : BeginLatex
  //
  //  Additionally, we reduce the output image extent, since the new pixels
  //  are now covering a larger area of $2.0\mbox{mm} \times 3.0\mbox{mm}$.
  //
  //  \index{itk::ResampleImageFilter!SetSize()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  size[0] = 80;  // number of pixels along X
  size[1] = 50;  // number of pixels along Y
  filter->SetSize( size );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  With these new parameters the physical extent of the output image is
  //  $160$ millimeters by $150$ millimeters.
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  Before attempting to analyze the effect of the resampling image filter
  //  it is important to make sure that the image viewer used to display the
  //  input and output images takes the spacing into account and
  //  appropriately scales the images on the screen. Please note that images
  //  in formats like PNG are not capable of representing origin and
  //  spacing. The toolkit assumes trivial default values for them. Figure
  //  \ref{fig:ResampleImageFilterOutput7} (center) illustrates the effect of
  //  using a naive viewer that does not take pixel spacing into account. A
  //  correct display is presented at the right in the same figure\footnote{A
  //  viewer is provided with ITK under the name of MetaImageViewer. This
  //  viewer takes into account pixel spacing.}.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.32\textwidth]{ResampleImageFilterOutput7}
  // \includegraphics[width=0.32\textwidth]{ResampleImageFilterOutput7b}
  // \itkcaption[ResampleImageFilter use of naive viewers]{Resampling with
  // different spacing seen by a naive viewer (center) and a correct viewer
  // (right), input image (left).}
  // \label{fig:ResampleImageFilterOutput7}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=\textwidth]{ResampleImageFilterTransformComposition4}
  // \itkcaption[ResampleImageFilter and output image spacing]{Effect of selecting
  // the spacing on the output image.}
  // \label{fig:ResampleImageFilterTransformComposition4}
  // \end{figure}
  //
  // The filter output is analyzed in a common coordinate system with the
  // input from Figure \ref{fig:ResampleImageFilterTransformComposition4}. In
  // this figure, pixel $I=(33,27)$ of the output image is located at
  // coordinates $P=(66.0,81.0)$ of the physical space. The identity
  // transform maps this point to $Q=(66.0,81.0)$ in the input image physical
  // space. The point $Q$ is then associated to the pixel of index
  // $I=(66,81)$ on the input image, because this image has zero origin and
  // unit spacing.
  //
  //  Software Guide : EndLatex

  if( exampleAction == 3 )
    {
    writer->Update();
    }


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.42\textwidth]{BrainProtonDensitySlice2x3}
  // \includegraphics[width=0.42\textwidth]{BrainProtonDensitySlice2x3b}
  // \itkcaption[ResampleImageFilter naive viewers]{Input image with $2 \times
  //  3 \mbox{mm}$ spacing as seen with a naive viewer (left) and a correct
  //  viewer (right).\label{fig:ResampleImageFilterInput2}}
  // \end{figure}
  //
  //  The input image spacing is also an important factor in the process of
  //  resampling an image. The following example illustrates the effect of
  //  non-unit pixel spacing on the input image. An input image similar to
  //  the those used in Figures
  //  \ref{fig:ResampleImageFilterTransformComposition1} to
  //  \ref{fig:ResampleImageFilterTransformComposition4} has been
  //  resampled to have pixel spacing of $2\mbox{mm} \times 3\mbox{mm}$. The
  //  input image is presented in Figure \ref{fig:ResampleImageFilterInput2}
  //  as viewed with a naive image viewer (left) and with a correct image
  //  viewer (right).
  //
  //  The following code is used to transform this non-unit spacing input image
  //  into another non-unit spacing image located at a non-zero origin. The
  //  comparison between input and output in a common reference system is
  //  presented in figure \ref{fig:ResampleImageFilterTransformComposition5}.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginLatex
  //
  //  Here we start by selecting the origin of the output image.
  //
  //  \index{itk::ResampleImageFilter!SetOutputOrigin()}
  //
  //  Software Guide : EndLatex

    {
    // Software Guide : BeginCodeSnippet
    // space coordinate of origin
    const double origin[ Dimension ] = { 25.0, 35.0 };
    filter->SetOutputOrigin( origin );
    // Software Guide : EndCodeSnippet
    }

  //  Software Guide : BeginLatex
  //
  //  We then select the number of pixels along each dimension.
  //
  //  \index{itk::ResampleImageFilter!SetSize()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  size[0] = 40;  // number of pixels along X
  size[1] = 45;  // number of pixels along Y
  filter->SetSize( size );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Finally, we set the output pixel spacing.
  //
  //  \index{itk::ResampleImageFilter!SetOutputSpacing()}
  //
  //  Software Guide : EndLatex

    {
    // Software Guide : BeginCodeSnippet
    const double spacing[ Dimension ] = { 4.0, 4.5 };
    filter->SetOutputSpacing( spacing );
    // Software Guide : EndCodeSnippet
    }

  if( exampleAction == 4 )
    {
    writer->Update();
    }


  //  Software Guide : BeginLatex
  //
  // Figure \ref{fig:ResampleImageFilterTransformComposition5} shows the
  // analysis of the filter output under these conditions. First, notice that
  // the origin of the output image corresponds to the settings
  // $O=(25.0,35.0)$ millimeters, spacing $(4.0,4.5)$ millimeters and size
  // $(40,45)$ pixels.  With these parameters the pixel of index $I=(10,10)$
  // in the output image is associated with the spatial point of coordinates
  // $P=(10 \times 4.0 + 25.0, 10 \times 4.5 + 35.0)) =(65.0,80.0)$. This
  // point is mapped by the transform---identity in this particular case---to
  // the point $Q=(65.0,80.0)$ in the input image space. The point $Q$ is
  // then associated with the pixel of index $I=( ( 65.0 - 0.0 )/2.0 - (80.0
  // - 0.0)/3.0) =(32.5,26.6)$. Note that the index does not fall on a grid
  // position. For this reason the value to be assigned to the output pixel
  // is computed by interpolating values on the input image around the
  // non-integer index $I=(32.5,26.6)$.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=\textwidth]{ResampleImageFilterTransformComposition5}
  // \itkcaption[ResampleImageFilter with non-unit spacing]{Effect of non-unit
  // spacing on the input and output images.}
  // \label{fig:ResampleImageFilterTransformComposition5}
  // \end{figure}
  //
  //  Note also that the discretization of the image is more visible on the
  //  output presented on the right side of Figure
  //  \ref{fig:ResampleImageFilterTransformComposition5} due to the choice of a
  //  low resolution---just $40 \times 45$ pixels.
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
