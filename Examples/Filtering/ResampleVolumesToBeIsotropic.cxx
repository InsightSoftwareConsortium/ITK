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

// Software Guide : BeginLatex
//
// It is unfortunate that it is still very common to find medical image
// datasets that have been acquired with large inter-slice spacings that
// result in voxels with anisotropic shapes. In many cases these voxels have
// ratios of $[1:5]$ or even $[1:10]$ between the resolution in the plane $(x,y)$
// and the resolution along the $z$ axis. These datasets are close to
// \textbf{useless} for the purpose of computer-assisted image analysis. The
// abundance of datasets acquired with anisotropic voxel sizes bespeaks a
// dearth of understanding of the third dimension and its importance for
// medical image analysis in clinical settings and radiology reading rooms.
// Datasets acquired with large anisotropies bring with them the regressive
// message: \emph{``I do not think 3D is informative''}.
// They stubbornly insist: \emph{``all that you need to know, can be known
// by looking at individual slices, one by one''}. However, the fallacy of this
// statement is made evident by simply viewing the slices when
// reconstructed in any of the orthogonal planes. The rectangular pixel shape
// is ugly and distorted, and cripples any signal processing algorithm not
// designed specifically for this type of image.
//
// Image analysts have a long educational battle to fight in the radiological
// setting in order to bring the message that 3D datasets acquired with
// anisotropies larger than $[1:2]$ are simply dismissive of the most fundamental
// concept of digital signal processing: The Shannon Sampling
// Theorem~\cite{Shannon1948,Shannon1949}.
//
// Facing the inertia of many clinical imaging departments and their
// blithe insistence that these images are ``good enough''
// for image processing, some image analysts have stoically tried
// to deal with these poor datasets. These image analysts usually
// proceed to subsample the high in-plane resolution and to super-sample the
// inter-slice resolution with the purpose of faking the type of dataset that
// they should have received in the first place: an \textbf{isotropic} dataset.
// This example is an illustration of how such an operation can be performed using
// the filters available in the Insight Toolkit.
//
// Note that this example is not presented here as a \emph{solution} to the
// problem of anisotropic datasets.  On the contrary, this is simply a
// \emph{dangerous palliative} which will only perpetuate the errant
// convictions of image acquisition departments. The real solution to the
// problem of the
// anisotropic dataset is to educate radiologists regarding the
// principles of image processing. If you really care about the technical
// decency of the medical image processing field, and you really care about
// providing your best effort to the patients who will receive health care
// directly or indirectly affected by your processed images, then it is your
// duty to reject anisotropic datasets and to patiently explain to your
// radiologist why anisotropic data are problematic for processing, and require
// crude workarounds which handicap your ability to draw accurate
// conclusions from the data and preclude his or her ability to provide
// quality care. Any barbarity such as a $[1:5]$ anisotropy ratio should be
// considered as a mere collection of slices, and not an authentic 3D dataset.
//
// Please, before employing the techniques covered in this section, do kindly
// invite your fellow radiologist to see the dataset in an orthogonal
// slice. Magnify that image in a viewer without any linear interpolation
// until you see the daunting reality of the rectangular pixels. Let her/him
// know how absurd it is to process digital data which have been sampled at
// ratios of $[1:5]$ or $[1:10]$.  Then, inform them that your only option is
// to throw away all that high in-plane
// resolution and to \emph{make up} data between the slices in order to
// compensate for the low resolution.  Only then will you be justified in
// using the following code.
//
// \index{Anisotropic data sets}
// \index{Subsampling}
// \index{Supersampling}
// \index{Resampling}
//
// Software Guide : EndLatex


// Software Guide : BeginLatex
//
// Let's now move into the code. It is appropriate for you to experience
// guilt\footnote{A feeling of regret or remorse for having committed some
// improper act; a recognition of one's own responsibility for doing something
// wrong.}, because your use the code below is the
// evidence that we have lost one more battle on the quest for real 3D dataset
// processing.
//
// This example performs subsampling on the in-plane resolution and performs
// super-sampling along the inter-slices resolution. The subsampling process
// requires that we preprocess the data with a smoothing filter in order to
// avoid the occurrence of aliasing effects due to overlap of the spectrum in
// the frequency domain~\cite{Shannon1948,Shannon1949}. The smoothing is
// performed here using the \code{RecursiveGaussian} filter, because it
// provides a convenient run-time performance.
//
// The first thing that you will need to do in order to resample this ugly
// anisotropic dataset is to include the header files for the
// \doxygen{ResampleImageFilter}, and the Gaussian smoothing filter.
//
// Software Guide : EndLatex

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

// Software Guide : BeginCodeSnippet
#include "itkResampleImageFilter.h"
#include "itkRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The resampling filter will need a Transform in order to map point
// coordinates and will need an interpolator in order to compute intensity
// values for the new resampled image. In this particular case we use the
// \doxygen{IdentityTransform} because the image is going to be resampled by
// preserving the physical extent of the sampled region. The Linear
// interpolator is used as a common trade-off\footnote{Although arguably we should use
// one type of interpolator for the in-plane subsampling process and another
// one for the inter-slice supersampling.  But again, one should wonder why we
// apply any technical sophistication here, when we are covering up for
// an improper acquisition of medical data, trying
// to make it look as if it was correctly acquired.}.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkIdentityTransform.h"
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Note that, as part of the preprocessing of the image, in this example we are
// also rescaling the range of intensities. This operation has already been
// described as Intensity Windowing. In a real clinical application, this step
// requires careful consideration of the range of intensities that contain
// information about the anatomical structures that are of interest for the
// current clinical application. It practice you may want to remove this step
// of intensity rescaling.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkIntensityWindowingImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  lower upper " << std::endl;
    return EXIT_FAILURE;
    }

// Software Guide : BeginLatex
//
// We make explicit now our choices for the pixel type and dimension of the
// input image to be processed, as well as the pixel type that we intend to use
// for the internal computation during the smoothing and resampling.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  const     unsigned int    Dimension = 3;

  typedef   unsigned short  InputPixelType;
  typedef   float           InternalPixelType;

  typedef itk::Image< InputPixelType,    Dimension >   InputImageType;
  typedef itk::Image< InternalPixelType, Dimension >   InternalImageType;
// Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType  >  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << excep << std::endl;
    }

  typedef itk::IntensityWindowingImageFilter<
                                  InputImageType,
                                  InternalImageType >  IntensityFilterType;

  IntensityFilterType::Pointer intensityWindowing = IntensityFilterType::New();

  intensityWindowing->SetWindowMinimum( atoi( argv[3] ) );
  intensityWindowing->SetWindowMaximum( atoi( argv[4] ) );

  intensityWindowing->SetOutputMinimum(   0.0 );
  intensityWindowing->SetOutputMaximum( 255.0 ); // floats but in the range of chars.

  intensityWindowing->SetInput( reader->GetOutput() );


// Software Guide : BeginLatex
//
// We instantiate the smoothing filter that will be used on the preprocessing
// for subsampling the in-plane resolution of the dataset.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::RecursiveGaussianImageFilter<
                                InternalImageType,
                                InternalImageType > GaussianFilterType;
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// We create two instances of the smoothing filter: one will smooth along the
// $X$ direction while the other will smooth along the $Y$ direction. They are
// connected in a cascade in the pipeline, while taking their input from the
// intensity windowing filter. Note that you may want to skip the intensity
// windowing scale and simply take the input directly from the reader.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  GaussianFilterType::Pointer smootherX = GaussianFilterType::New();
  GaussianFilterType::Pointer smootherY = GaussianFilterType::New();

  smootherX->SetInput( intensityWindowing->GetOutput() );
  smootherY->SetInput( smootherX->GetOutput() );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// We must now provide the settings for the resampling itself. This is done by
// searching for a value of isotropic resolution that will provide a trade-off
// between the evil of subsampling and the evil of supersampling. We advance
// here the conjecture that the geometrical mean between the in-plane and the
// inter-slice resolutions should be a convenient isotropic resolution to use.
// This conjecture is supported on nothing other than intuition and common
// sense. You can rightfully argue that this choice deserves a more technical
// consideration, but then, if you are so concerned about the technical integrity
// of the image sampling process, you should not be using this code, and should
// discuss these issues with the radiologist who
// acquired this ugly anisotropic dataset.
//
// We take the image from the input and then request its array of pixel spacing
// values.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  InputImageType::ConstPointer inputImage = reader->GetOutput();

  const InputImageType::SpacingType& inputSpacing = inputImage->GetSpacing();
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// and apply our ad-hoc conjecture that the correct anisotropic resolution
// to use is the geometrical mean of the in-plane and inter-slice resolutions.
// Then set this spacing as the Sigma value to be used for the Gaussian
// smoothing at the preprocessing stage.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  const double isoSpacing = std::sqrt( inputSpacing[2] * inputSpacing[0] );

  smootherX->SetSigma( isoSpacing );
  smootherY->SetSigma( isoSpacing );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// We instruct the smoothing filters to act along the $X$ and $Y$ direction
// respectively.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  smootherX->SetDirection( 0 );
  smootherY->SetDirection( 1 );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Now that we have taken care of the smoothing in-plane, we proceed to
// instantiate the resampling filter that will reconstruct an isotropic image.
// We start by declaring the pixel type to be used as the output of this filter,
// then instantiate the image type and the type for the resampling filter.
// Finally we construct an instantiation of the filter.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  typedef   unsigned char   OutputPixelType;

  typedef itk::Image< OutputPixelType,   Dimension >   OutputImageType;

  typedef itk::ResampleImageFilter<
                InternalImageType, OutputImageType >  ResampleFilterType;

  ResampleFilterType::Pointer resampler = ResampleFilterType::New();
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The resampling filter requires that we provide a Transform, which in this
// particular case can simply be an identity transform.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::IdentityTransform< double, Dimension >  TransformType;

  TransformType::Pointer transform = TransformType::New();
  transform->SetIdentity();

  resampler->SetTransform( transform );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The filter also requires an interpolator to be passed to it. In this case we
// chose to use a linear interpolator.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::LinearInterpolateImageFunction<
                          InternalImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  resampler->SetInterpolator( interpolator );
// Software Guide : EndCodeSnippet


  resampler->SetDefaultPixelValue( 255 ); // highlight regions without source


// Software Guide : BeginLatex
//
// The pixel spacing of the resampled dataset is loaded in a \code{SpacingType}
// and passed to the resampling filter.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  OutputImageType::SpacingType spacing;

  spacing[0] = isoSpacing;
  spacing[1] = isoSpacing;
  spacing[2] = isoSpacing;

  resampler->SetOutputSpacing( spacing );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The origin and orientation of the output image is maintained, since we
// decided to resample the image in the same physical extent of the input
// anisotropic image.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  resampler->SetOutputOrigin( inputImage->GetOrigin() );
  resampler->SetOutputDirection( inputImage->GetDirection() );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The number of pixels to use along each dimension in the grid of the
// resampled image is computed using the ratio between the pixel spacings of the
// input image and those of the output image. Note that the computation of the
// number of pixels along the $Z$ direction is slightly different with the
// purpose of making sure that we don't attempt to compute pixels that are
// outside of the original anisotropic dataset.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  InputImageType::SizeType   inputSize =
                    inputImage->GetLargestPossibleRegion().GetSize();

  typedef InputImageType::SizeType::SizeValueType SizeValueType;

  const double dx = inputSize[0] * inputSpacing[0] / isoSpacing;
  const double dy = inputSize[1] * inputSpacing[1] / isoSpacing;

  const double dz = (inputSize[2] - 1 ) * inputSpacing[2] / isoSpacing;
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Finally the values are stored in a \code{SizeType} and passed to the
// resampling filter. Note that this process requires a casting since the
// computations are performed in \code{double}, while the elements of the
// \code{SizeType} are integers.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  InputImageType::SizeType   size;

  size[0] = static_cast<SizeValueType>( dx );
  size[1] = static_cast<SizeValueType>( dy );
  size[2] = static_cast<SizeValueType>( dz );

  resampler->SetSize( size );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Our last action is to take the input for the resampling image filter from
// the output of the cascade of smoothing filters, and then to trigger the
// execution of the pipeline by invoking the \code{Update()} method on the
// resampling filter.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  resampler->SetInput( smootherY->GetOutput() );

  resampler->Update();
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// At this point we should take a moment in silence to reflect on the
// circumstances that have led us to accept this cover-up for the improper
// acquisition of medical data.
//
// Software Guide : EndLatex


  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
  writer->SetInput( resampler->GetOutput() );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }


  return EXIT_SUCCESS;
}
