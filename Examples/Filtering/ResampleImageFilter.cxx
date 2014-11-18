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

//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {ResampleImageFilterOutput1.png}
//    ARGUMENTS:    0
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {ResampleImageFilterOutput2.png}
//    ARGUMENTS:    1
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {ResampleImageFilterOutput3.png}
//    ARGUMENTS:    2
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {ResampleImageFilterOutput4.png}
//    ARGUMENTS:    3
//  Software Guide : EndCommandLineArgs
//
//  Software Guide : BeginLatex
//
//  Resampling an image is a very important task in image analysis. It is
//  especially important in the frame of image registration. The
//  \doxygen{ResampleImageFilter} implements image resampling through the use
//  of \doxygen{Transform}s. The inputs expected by this filter are an image,
//  a transform and an interpolator. The space coordinates of the image are
//  mapped through the transform in order to generate a new image. The extent
//  and spacing of the resulting image are selected by the user. Resampling
//  is performed in space coordinates, not pixel/grid coordinates. It is
//  quite important to ensure that image spacing is properly set on the
//  images involved. The interpolator is required since the mapping from one
//  space to the other will often require evaluation of the intensity of the
//  image at non-grid positions.
//
//  \index{itk::ResampleImageFilter}
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


//  Software Guide : BeginLatex
//
//  The header file corresponding to this filter should be included first.
//
//  \index{itk::ResampleImageFilter!header}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkResampleImageFilter.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  The header files corresponding to the transform and interpolator must
//  also be included.
//
//  \index{itk::AffineTransform!header}
//  \index{itk::Nearest\-Neighbor\-Interpolate\-Image\-Function!header}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkAffineTransform.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile";
    std::cerr << "  [exampleAction={0,1,2,3}]" << std::endl;
    return EXIT_FAILURE;
    }

  int exampleAction = 0;

  if( argc >= 4 )
    {
    exampleAction = atoi( argv[3] );
    }

  //  Software Guide : BeginLatex
  //
  //  The dimension and pixel types for input and output image must be
  //  defined and with them the image types can be instantiated.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int                               Dimension = 2;
  typedef unsigned char                            InputPixelType;
  typedef unsigned char                            OutputPixelType;
  typedef itk::Image< InputPixelType,  Dimension > InputImageType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  //  Software Guide : BeginLatex
  //
  //  Using the image and transform types it is now possible to instantiate
  //  the filter type and create the filter object.
  //
  //  \index{itk::ResampleImageFilter!instantiation}
  //  \index{itk::ResampleImageFilter!New()}
  //  \index{itk::ResampleImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ResampleImageFilter<InputImageType,OutputImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The transform type is typically defined using the image dimension
  //  and the type used for representing space coordinates.
  //
  //  \index{itk::AffineTransform!instantiation}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  typedef itk::AffineTransform< double, Dimension >  TransformType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  An instance of the transform object is instantiated and passed to the
  //  resample filter. By default, the parameters of the transform are set to
  //  represent the identity transform.
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
  //  The interpolator type is defined using the full image type and the type
  //  used for representing space coordinates.
  //
  //  \index{itk::Nearest\-Neighbor\-Interpolate\-Image\-Function!instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::NearestNeighborInterpolateImageFunction<
                       InputImageType, double >  InterpolatorType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  An instance of the interpolator object is instantiated and passed to
  //  the resample filter.
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
  // pixel spacing in millimeters along X and Y
  const double spacing[ Dimension ] = { 1.0, 1.0 };
  filter->SetOutputSpacing( spacing );

  // Physical space coordinate of origin for X and Y
  const double origin[ Dimension ] = { 0.0, 0.0 };
  filter->SetOutputOrigin( origin );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  InputImageType::DirectionType direction;
  direction.SetIdentity();
  filter->SetOutputDirection( direction );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The extent of the sampling grid on the output image is defined by a
  //  \code{SizeType} and is set using the \code{SetSize()} method.
  //
  //  \index{itk::ResampleImageFilter!SetSize()}
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
  //  The input to the filter can be taken from any other filter, for example
  //  a reader. The output can be passed down the pipeline to other filters,
  //  for example a writer. An update call on any downstream filter will
  //  trigger the execution of the resampling filter.
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
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.44\textwidth]{ResampleImageFilterOutput1}
  // \itkcaption[Effect of the Resample filter]{Effect of the resample filter.}
  // \label{fig:ResampleImageFilterOutput1}
  // \end{figure}
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=\textwidth]{ResampleImageFilterOutput1Analysis}
  // \itkcaption[Analysis of resampling in common coordinate system]{Analysis of
  // the resample image done in a common coordinate system.}
  // \label{fig:ResampleImageFilterOutput1Analysis}
  // \end{figure}
  //
  //  Figure \ref{fig:ResampleImageFilterOutput1} illustrates the effect of
  //  this filter on a slice of MRI brain image using an affine transform
  //  containing an identity transform. Note that any analysis of the
  //  behavior of this filter must be done on the space coordinate system in
  //  millimeters, not with respect to the sampling grid in pixels. The
  //  figure shows the resulting image in the lower left quarter of the
  //  extent. This may seem odd if analyzed in terms of the image grid but is
  //  quite clear when seen with respect to space coordinates.  Figure
  //  \ref{fig:ResampleImageFilterOutput1} is particularly misleading
  //  because the images are rescaled to fit nicely on the text of this book.
  //  Figure \ref{fig:ResampleImageFilterOutput1Analysis} clarifies the
  //  situation. It shows the two same images placed on an equally-scaled
  //  coordinate system.  It becomes clear here that an identity transform is
  //  being used to map the image data, and that simply, we have requested to
  //  resample additional empty space around the image. The input image is
  //  $181 \times 217$ pixels in size and we have requested an output of $300
  //  \times 300$ pixels. In this case, the input and output images both have
  //  spacing of $1mm \times 1mm$ and origin of $(0.0,0.0)$.
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  Let's now set values on the transform. Note that the supplied transform
  //  represents the mapping of points from the output space to the input
  //  space.  The following code sets up a translation.
  //
  //  \index{itk::AffineTransform!Translate()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::OutputVectorType translation;
  translation[0] = -30;  // X translation in millimeters
  translation[1] = -50;  // Y translation in millimeters
  transform->Translate( translation );
  // Software Guide : EndCodeSnippet


  if( exampleAction == 1 )
    {
    writer->Update();
    }


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.44\textwidth]{ResampleImageFilterOutput2}
  // \itkcaption[ResampleImageFilter with a translation by
  // $(-30,-50)$]{ResampleImageFilter with a translation by $(-30,-50)$.}
  // \label{fig:ResampleImageFilterOutput2}
  // \end{figure}
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=\textwidth]{ResampleImageFilterOutput2Analysis}
  // \itkcaption[ResampleImageFilter. Analysis of a translation by
  // $(-30,-50)$]{ResampleImageFilter. Analysis of a translation by
  // $(-30,-50)$.}
  // \label{fig:ResampleImageFilterOutput2Analysis}
  // \end{figure}
  //
  // The output image resulting from the translation can be seen in Figure
  // \ref{fig:ResampleImageFilterOutput2}. Again, it is better to interpret
  // the result in a common coordinate system as illustrated in Figure
  // \ref{fig:ResampleImageFilterOutput2Analysis}.
  //
  // Probably the most important thing to keep in mind when resampling images
  // is that the transform is used to map points from the \textbf{output}
  // image space into the \textbf{input} image space. In this case, Figure
  // \ref{fig:ResampleImageFilterOutput2Analysis} shows that the translation
  // is applied to every point of the output image and the resulting position
  // is used to read the intensity from the input image. In this way, the
  // gray level of the point $P$ in the output image is taken from the point
  // $T(P)$ in the input image. Where $T$ is the transformation. In the
  // specific case of the Figure
  // \ref{fig:ResampleImageFilterOutput2Analysis}, the value of point
  // $(105,188)$ in the output image is taken from the point $(75,138)$ of
  // the input image because the transformation applied was a translation of
  // $(-30,-50)$.
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  It is sometimes useful to intentionally set the default output value to
  //  a distinct gray value in order to highlight the mapping of the image
  //  borders. For example, the following code sets the default external
  //  value of $100$.  The result is shown in the right side of Figure
  //  \ref{fig:ResampleImageFilterOutput3Analysis}.
  //
  //  \index{itk::ResampleImageFilter!SetDefaultPixelValue()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetDefaultPixelValue( 100 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=\textwidth]{ResampleImageFilterOutput3Analysis}
  // \itkcaption[ResampleImageFilter highlighting image
  // borders]{ResampleImageFilter highlighting image borders with
  // SetDefaultPixelValue().}
  // \label{fig:ResampleImageFilterOutput3Analysis}
  // \end{figure}
  //
  //  With this change we can better appreciate the effect of the previous
  //  translation transform on the image resampling. Figure
  //  \ref{fig:ResampleImageFilterOutput3Analysis} illustrates how the point
  //  $(30,50)$ of the output image gets its gray value from the point $(0,0)$
  //  of the input image.
  //
  //  Software Guide : EndLatex

  if( exampleAction == 2 )
    {
    writer->Update();
    }


  return EXIT_SUCCESS;
}
