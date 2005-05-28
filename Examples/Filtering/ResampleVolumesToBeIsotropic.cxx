/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ResampleVolumesToBeIsotropic.cxx
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

// Software Guide : BeginLatex
//
// It is unfortunate that it is still very common to find medical image
// datasets that have been acquired with large inter-sclice spacings that
// result in voxels with anisotropic shapes. In many cases these voxels have
// ratios of $[1:5]$ or even $[1:10]$ between the resolution in the plane $(x,y)$
// and the resolution along the $z$ axis. Such dataset are close to
// \textbf{useless} for the purpose of computer assisted image analysis. The
// persistent tendency for acquiring dataset in such formats just reveals how
// small is the understanding of the third dimension that have been gained in
// the clinical settings and in many radiology reading rooms. Datasets that are
// acquired with such large anisotropies bring with them the retrograde
// message: \emph{``I do not think 3D is informative''}. 
// They repeat stubbornly that: \emph{``all that you need to know, can be known
// by looking at individual slices, one by one''}. However, the fallacy of such
// statement is made evident with the simple act of looking at the slices when
// reconstructed in any of the ortogonal planes. The ugliness of the extreme
// rectangular pixel shapes becomes obvious, along with the clear technical
// realization that no decent signal processing or algorithms can be performed
// in such images.
// 
// Image analysists have a long educational battle to fight in the radiological
// setting in order to bring the message that 3D datasets acquired with
// anisotropies larger than $[1:2]$ are simply dismissive of the most fundamental
// concept of digital signal processing: The Shannon Sampling
// Theorem~\cite{Shannon1948,Shannon1949}.
//
// Face to the inertia of many clinical imaging departments and their
// nearsighted insistency in that those malformed images should be good enough
// for image processing, some image analysist have adopted the stoic position
// of trying to deal with those poor datasets. These image analysist usually
// proceed to subsample the high in-plane resolution and to super-sample the
// inter-slice resolution with the purpose of faking the type of dataset that
// they should have received in the first place: an \textbf{isotropic} dataset.
// This example is an illustration of how such operation can be performed using
// the filter available in the Insight Toolkit. 
//
// Note that this example is not presented here as a \emph{solution} to the
// problem of anisotropic datasets.  On the contrary, this is simply a
// \emph{dangeros palliative} that will help to perpetuate the mistake of the
// image acquisition departments. This code is just an analgesic that will make
// you believe that you don't have pain, while a real and lethal disease is
// growing inside you. The real solution to the problem of atrophic anisotropic
// dataset is to educate radiologist on the fundamental principles of image
// processing. If you really care about the technical decency of the medical
// image processing field, and you really care about providing your best effort
// to the patients who will receive health care directly or indirectly affected
// by your processed images, then it is your duty to reject anisotropic datasets
// and to patiently explain radiologist why a barbarity such as a $[1:5]$
// anisotropy ratio makes a data set to be just ``a collection of slices''
// instead of an authentic 3D datasets. 
//
// Please, before daring to use the example in this section, do kindly invite
// your fellow radiologist to see the dataset in an orthogonal slice. Zoom in
// that image in a viewer without any linear interpolation until you see the
// daunting reality of the rectangular pixels. Let her/him know how absurd is
// to process digital data that have been sampled at ratios of $[1:5]$ or
// $[1:10]$.  Then, let them know that the first thing that you are going to do
// is to throw away all that high in-plane resolution and to \emph{make up}
// data in-between the slices in order to compensate for their low resolution.
// Only then, you would have gained the right to use this code.
//
// \index{Anisotropic data sets}
// \index{Subsampling}
// \index{Supersampling}
// \index{Resampling}
//
// Software Guide : EndLatex 


// Software Guide : BeginLatex
//
// Let's now move into the code.... and, yes, bring with you that
// guilt\footnote{A feeling of regret or remorse for having committed some
// improper act; a recognition of one's own responsibility for doing something
// wrong.}, because the fact that you are going to use the code below, is the
// evidence that we have lost one more battle on the quest for real 3D dataset
// processing.
//
// This example performs subsampling on the in-plane resolution and performs
// super-sampling along the inter-slices resolution. The subsampling process
// requires that we preprocess the data with a smoothing filter in order to
// avoid the ocurrence of aliasing effects due to overlap of the spectrum in
// the frequency domain~\cite{Shannon1948,Shannon1949}. The smoothing is
// performed here using the \code{RecursiveGaussian} filter, given that it
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
// interpolator is used as a common trade-off, although arguably we should use
// one type of interpolator for the in-plane subsampling process and another
// one for the inter-slice supersampling, but again, one should wonder why to
// enter into technical sophistication here, when what we are doing is to
// cover-up for an improper acquisition of medical data, and we are just trying
// to make it look as if it was correctly acquired.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkIdentityTransform.h"
#include "itkLinearInterpolateImageFunction.h"
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
// We made explicit now our choices for the pixel type and dimension of the
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
// We create two instances of the smoothing filter, one will smooth along the
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
// This conjucture is supported on nothing else than intuition and common
// sense. You can rightfully argue that this choice deserves a more technical
// consideration, but then, if you are so inclined to the technical correctness
// of the image sampling process, you should not be using this code, and should
// rather we talking about such technical correctness to the radiologist who
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
  const double isoSpacing = sqrt( inputSpacing[2] * inputSpacing[0] );
  
  smootherX->SetSigma( isoSpacing );
  smootherY->SetSigma( isoSpacing );
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// We instruct the smoothing filters to act along the $X$ and $Y$ direction
// respectively. And define the settings for avoiding the loss of intensity as
// a result of the diffusion process that is inherited from the use of a
// Gaussian filter.
//
// \index{RecursiveGaussianImageFilter!SetNormalizeAcrossScale}
// Software Guide : EndLatex 

 
// Software Guide : BeginCodeSnippet
  smootherX->SetDirection( 0 );
  smootherY->SetDirection( 1 );

  smootherX->SetNormalizeAcrossScale( true );
  smootherY->SetNormalizeAcrossScale( true );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Now that we have taken care of the smoothing in-plane, we proceed to
// instantiate the resampling filter that will reconstruct an isotropic image.
// We start by declaring the pixel type to be use at the output of such filter,
// then instantiate the image type and the type for the resampling filter.
// Finally we construct an instantiation of such a filter.
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
// The resampling filter requires that we provide a Transform, that in this
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
// The origin of the output image is maintained, since we decided to resample
// the image in the same physical extent of the input anisotropic image.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  resampler->SetOutputOrigin( inputImage->GetOrigin() );
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
// computation are performed in \code{double}, while the elements of the
// \code{SizeType} are integers.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  InputImageType::SizeType   size;

  size[0] = static_cast<SizeValueType>( dx );
  size[1] = static_cast<SizeValueType>( dy );
  size[2] = static_cast<SizeValueType>( dx );

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
// At this point we should take some minutes in silence to reflect on the
// circumstances that have lead us to accept to cover-up for the improper
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
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }


  return EXIT_SUCCESS;
}

