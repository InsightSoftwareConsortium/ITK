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
// ratios of $1:5$ or even $1:10$ between the resolution in the plane $(x,y)$
// and the resolution along the $z$ axis. Such dataset are close to
// \textbf{useless} for the purpose of computer assiste image analysis. The
// persistent tendency for acquiring dataset in such formats just reveals how
// few the understanding of the third dimension have been gained in the
// clinical setting and in many radiology reading rooms. Datasets that are
// acquired with such large anisotropies bring with them the retrograde
// message: "I don't think that 3D is informative". They repeat stubornly that
// "all that you need to know can be known by looking at individual slices one
// by one". However, the fallacy of such statement is made evident with the
// simple act of looking at the slices when reconstructed in any of the
// ortogonal planes. The uglines of the extreme rectangular shapes of the
// pixels becomes obvious, along with the clear realization that no decent
// signal processing or algorithms can be performed in such images.
// 
// Image analysists have a long educational battle to fight in the radiological
// setting in order to bring the message that 3D datasets acquired with
// anisotropies larger than 1:2 are simply dismissive of the most fundamental
// concept of digital signal processing: The Shannon Sampling Theorem.
//
// Face to the inertia of many clinical imaging departments and their
// nearsighted insistency in that those malformed images should be good enough
// for image processing, some image analysist have adopted the stoic position
// of trying to deal with those poor datasets. These image analysist usually
// proceed to subsample the high in-plane resolution, and to super-sample the
// inter-slice resolution, with the purpose to fake the type of dataset that
// they should have received in the first place: an \textbf{isotropic} dataset.
//
// This example is an illustration of how such operation can be performed using
// the filter available in the Insight toolkit. Note that this example is not
// presented here as a \em{solution} to the problem of anisotropic datasets.
// This is simply a dangeros paliative that will help to perpetuate the mistake
// of the image acquisition departments. The real solution to the problem of
// atrophic anisotrpic dataset is to educate radiologist on the fundamental
// principles of image processing. If you really care about the technical
// decency of the medical image processing field, it is your duty to reject
// isotropic data set and to patiently explain radiologist why such a barbarity
// as a 1:5 anisotropi ratio makes a data set to be just "a collection of
// slices" instead of an authentic 3D datasets.
//
// Software Guide : EndLatex 



// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkIdentityTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkIntensityWindowingImageFilter.h"

int main( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  lower upper " << std::endl; 
    return EXIT_FAILURE;
    }

  const     unsigned int    Dimension = 3;

  typedef   unsigned short  InputPixelType;
  typedef   float           InternalPixelType;
  typedef   unsigned char   OutputPixelType;

  typedef itk::Image< InputPixelType,    Dimension >   InputImageType;
  typedef itk::Image< InternalPixelType, Dimension >   InternalImageType;
  typedef itk::Image< OutputPixelType,   Dimension >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  try 
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << excep << std::endl;
    }


  InputImageType::ConstPointer inputImage = reader->GetOutput();

  const InputImageType::SpacingType& inputSpacing = inputImage->GetSpacing();

  const double isoSpacing = sqrt( inputSpacing[2] * inputSpacing[0] );

  typedef itk::IntensityWindowingImageFilter< 
                                  InputImageType, 
                                  InternalImageType >  IntensityFilterType;

  IntensityFilterType::Pointer intensityWindowing = IntensityFilterType::New();

  intensityWindowing->SetWindowMinimum( atoi( argv[3] ) );
  intensityWindowing->SetWindowMaximum( atoi( argv[4] ) );

  intensityWindowing->SetOutputMinimum(   0.0 );
  intensityWindowing->SetOutputMaximum( 255.0 ); // floats but in the range of chars.


  typedef itk::RecursiveGaussianImageFilter< 
                                  InternalImageType,
                                  InternalImageType > GaussianFilterType;

  GaussianFilterType::Pointer smootherX = GaussianFilterType::New();
  GaussianFilterType::Pointer smootherY = GaussianFilterType::New();

  intensityWindowing->SetInput( reader->GetOutput() );
  smootherX->SetInput( intensityWindowing->GetOutput() );
  smootherY->SetInput( smootherX->GetOutput() );

  smootherX->SetSigma( isoSpacing );
  smootherY->SetSigma( isoSpacing );

  smootherX->SetDirection( 0 );
  smootherY->SetDirection( 1 );

  smootherX->SetNormalizeAcrossScale( true );
  smootherY->SetNormalizeAcrossScale( true );

  try 
    {
    smootherY->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }


  InternalImageType::ConstPointer smoothedImage = smootherY->GetOutput();

  typedef itk::ResampleImageFilter<
                  InternalImageType, OutputImageType >  ResampleFilterType;

  ResampleFilterType::Pointer resampler = ResampleFilterType::New();

  typedef itk::IdentityTransform< double, Dimension >  TransformType;

  typedef itk::LinearInterpolateImageFunction< 
                                   InternalImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  resampler->SetInterpolator( interpolator );

  resampler->SetDefaultPixelValue( 255 ); // highlight regions without source


  OutputImageType::SpacingType spacing;

  spacing[0] = isoSpacing;
  spacing[1] = isoSpacing;
  spacing[2] = isoSpacing;

  resampler->SetOutputSpacing( spacing );

  // Use the same origin
  resampler->SetOutputOrigin( inputImage->GetOrigin() );


  InputImageType::SizeType   inputSize = inputImage->GetLargestPossibleRegion().GetSize();
  typedef InputImageType::SizeType::SizeValueType SizeValueType;
  InputImageType::SizeType   size;

  size[0] =
    static_cast<SizeValueType>(inputSize[0] * inputSpacing[0] / isoSpacing);
  size[1] =
    static_cast<SizeValueType>(inputSize[1] * inputSpacing[1] / isoSpacing);
  size[2] =
    static_cast<SizeValueType>((inputSize[2] - 1 ) * inputSpacing[2] / isoSpacing);

  resampler->SetSize( size );

  resampler->SetInput( smoothedImage );

  writer->SetInput( resampler->GetOutput() );

  TransformType::Pointer transform = TransformType::New();

  transform->SetIdentity();

  resampler->SetTransform( transform );

  
  try 
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }
// Software Guide : EndCodeSnippet
//
  return EXIT_SUCCESS;
}

