/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DeformableRegistration2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageFileReader.h" 
#include "itkImageFileWriter.h" 


// Software Guide : BeginLatex
//
// This example demostrates how to use the ``demons'' algorithm to deformably
// register two images. The first step is to include the header files:
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkDemonsRegistrationFilter.h"
#include "itkHistogramMatchingImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkWarpImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"
// Software Guide : EndCodeSnippet



int main( int argc, char *argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile " << std::endl;
    return 1;
    }

  // Software Guide : BeginLatex
  //
  // Second, we declare the types of the images.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 2;
  typedef unsigned short PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;
  // Software Guide : EndCodeSnippet

  // Set up the file readers
  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer fixedImageReader   = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName( argv[1] );
  movingImageReader->SetFileName( argv[2] );




  // Software Guide : BeginLatex
  //
  // Image file readers are set up in a similar fashion to previous examples.
  // To support the re-mapping of the moving image intensity we declare an
  // internal image type with a floating point pixel type and cast the input
  // images to the internal image type.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float InternalPixelType;
  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;

  typedef itk::CastImageFilter< FixedImageType, InternalImageType > FixedImageCasterType;
  typedef itk::CastImageFilter< MovingImageType, InternalImageType > MovingImageCasterType;

  FixedImageCasterType::Pointer fixedImageCaster   = FixedImageCasterType::New();
  MovingImageCasterType::Pointer movingImageCaster = MovingImageCasterType::New();

  fixedImageCaster->SetInput( fixedImageReader->GetOutput() );
  movingImageCaster->SetInput( movingImageReader->GetOutput() ); 
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The demons algorithm relies on the assumption that pixels representing the
  // same homologous point on an object have the same intensity on both the
  // fixed and moving images to be registered. In this example, we will
  // preprocess the moving image to match the intensity between the images
  // using the \doxygen{HistogramMatchingImageFilter}. 
  //
  // \index{itk::HistogramMatchingImageFilter|textbf}
  //
  // The basic idea is to match the histograms of the two images at a user
  // specified number of quantile values. For robustness, the histograms are
  // matched so that the background pixels are excluded from both histograms.
  // For MR images, a simple procedure is to exclude all gray values that are
  // smaller than the mean gray value of the image.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::HistogramMatchingImageFilter<
                                    InternalImageType,
                                    InternalImageType >   MatchingFilterType;

  MatchingFilterType::Pointer matcher = MatchingFilterType::New();
  // Software Guide : EndCodeSnippet





  // Software Guide : BeginLatex
  // 
  // For this example, we set the moving image as the source or input image and
  // the fixed image as the reference image.
  //
  // \index{itk::HistogramMatchingImageFilter!SetInput()}
  // \index{itk::HistogramMatchingImageFilter!SetSourceImage()}
  // \index{itk::HistogramMatchingImageFilter!SetReferenceImage()}
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet 
  matcher->SetInput( movingImageCaster->GetOutput() );
  matcher->SetReferenceImage( fixedImageCaster->GetOutput() );
  // Software Guide : EndCodeSnippet




  // Software Guide : BeginLatex
  //
  // We then select the number of bins to represent the histograms and the number
  // of points or quantiles values where the histogram is to be matched.
  //
  // \index{itk::HistogramMatchingImageFilter!SetNumberOfHistogramLevels()}
  // \index{itk::HistogramMatchingImageFilter!SetNumberOfMatchPoints()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  matcher->SetNumberOfHistogramLevels( 1024 );
  matcher->SetNumberOfMatchPoints( 7 );
  // Software Guide : EndCodeSnippet





  // Software Guide : BeginLatex
  //
  // Simple background extraction is done by thresholding at the mean intensity.
  //
  // \index{itk::HistogramMatchingImageFilter!ThresholdAtMeanIntensityOn()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  matcher->ThresholdAtMeanIntensityOn();
  // Software Guide : EndCodeSnippet




  // Software Guide : BeginLatex
  //
  // In the \doxygen{DemonsRegistrationFilter}, the deformation field is represented
  // as an image whose pixels are floating point vectors.
  //
  // \index{itk::DemonsRegistrationFilter|textbf}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Vector< float, Dimension >    VectorPixelType;

  typedef itk::Image<  VectorPixelType, Dimension > DeformationFieldType;

  typedef itk::DemonsRegistrationFilter<
                                InternalImageType,
                                InternalImageType,
                                DeformationFieldType>   RegistrationFilterType;

  RegistrationFilterType::Pointer filter = RegistrationFilterType::New();
  // Software Guide : EndCodeSnippet




  // Software Guide : BeginLatex
  //
  // The input fixed image is simply the output of the fixed image casting filter.
  // The input moving image is the output of the histogram matching filter.
  //
  // \index{itk::DemonsRegistrationFilter!SetFixedImage()}
  // \index{itk::DemonsRegistrationFilter!SetMovingImage()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetFixedImage( fixedImageCaster->GetOutput() );
  filter->SetMovingImage( matcher->GetOutput() );
  // Software Guide : EndCodeSnippet




  // Software Guide : BeginLatex
  //
  // The demons registration filter has two parameters: the number of iterations to
  // be performed and the standard deviation of the Gaussian smoothing kernel to
  // be applied to the deformation field after each iteration.
  // \index{itk::DemonsRegistrationFilter!SetNumberOfIterations()}
  // \index{itk::DemonsRegistrationFilter!SetStandardDeviations()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetNumberOfIterations( 150 );
  filter->SetStandardDeviations( 1.0 );
  // Software Guide : EndCodeSnippet





  // Software Guide : BeginLatex
  // 
  // The registration algorithm is triggered by updating the filter. The filter
  // output is the computed deformation field.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->Update();
  // Software Guide : EndCodeSnippet




  // Software Guide : BeginLatex
  //
  // A \doxygen{WarpImageFilter} can be used to warp the moving image with the
  // output deformation field. Similar to the \doxygen{ResampleImageFilter}, the
  // \doxygen{WarpImageFilter} requires the specification of the input image to be
  // resampled, an input image interpolator, the output image spacing and
  // origin.
  //
  // \index{itk::WarpImageFilter|textbf}
  // \index{itk::WarpImageFilter!SetInput()}
  // \index{itk::WarpImageFilter!SetInterpolator()}
  // \index{itk::WarpImageFilter!SetOutputSpacing()}
  // \index{itk::WarpImageFilter!SetOutputOrigin()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::WarpImageFilter<
                          MovingImageType, 
                          MovingImageType,
                          DeformationFieldType  >     WarperType;

  typedef itk::LinearInterpolateImageFunction<
                                   MovingImageType,
                                   double          >  InterpolatorType;


  WarperType::Pointer warper = WarperType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  warper->SetInput( movingImageReader->GetOutput() );
  warper->SetInterpolator( interpolator );
  warper->SetOutputSpacing( fixedImage->GetSpacing() );
  warper->SetOutputOrigin( fixedImage->GetOrigin() );
  // Software Guide : EndCodeSnippet






  // Software Guide : BeginLatex
  //
  // Unlike the \doxygen{ResampleImageFilter}, the \doxygen{WarpImageFilter} warps or transform
  // the input image with respect to the deformation field represented by an image of vectors.
  // The resulting warped or resampled image is written to file as per previous examples.
  //
  // \index{itk::WarpImageFilter!SetDeformationField()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  warper->SetDeformationField( filter->GetOutput() );
  // Software Guide : EndCodeSnippet




  // Write warped image out to file
  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  
  typedef itk::CastImageFilter< 
                        MovingImageType,
                        OutputImageType > CastFilterType;
                    
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;


  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();

  writer->SetFileName( argv[3] );
  
  caster->SetInput( warper->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  writer->Update();





  // Software Guide : BeginLatex
  //
  // Let's execute this example using rat lung data in the previous example.
  // The associated data files can be found in \code{Insight/Examples/Data}:
  // \begin{itemize}
  // \item \code{RatLungSlice1.mha}
  // \item \code{RatLungSlice2.mha}
  // \end{itemize}
  //
  // \begin{figure} \center
  // \includegraphics[width=0.44\textwidth]{DeformableRegistration2CheckerboardBefore.eps}
  // \includegraphics[width=0.44\textwidth]{DeformableRegistration2CheckerboardAfter.eps}
  // \itkcaption[Demon's deformable registration output]{Checkerboard comparisons
  // before and after demons-based deformable registration.}
  // \label{fig:DeformableRegistration2Output}
  // \end{figure}
  // 
  // The result of the demons-based deformable registration is presented in
  // Figure \ref{fig:DeformableRegistration2Output}. The checkerboard
  // comparision shows that the algorithm was able to recover the misalignment
  // due to expiration.
  //
  // Software Guide : EndLatex

  return 0;

}

