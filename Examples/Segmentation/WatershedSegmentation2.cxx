/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    WatershedSegmentation2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

// Software Guide : BeginLatex
//
// The following example illustrates how to preprocess and segment images using
// the \doxygen{WatershedImageFilter} for the particular case of grayscale
// scalar image.
// 
// Software Guide : EndLatex

#include "itkWatershedImageFilter.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkScalarToRGBPixelFunctor.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"


int main( int argc, char *argv[] )
{


  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage lowerThreshold  outputScaleLevel" << std::endl;
    return 1;
    }

  typedef float                             InternalPixelType;
  typedef itk::RGBPixel<unsigned char>      RGBPixelType;

  const   unsigned int                      Dimension = 3;


  typedef itk::Image< InternalPixelType,  Dimension >  InternalImageType;
  typedef itk::Image< RGBPixelType,       Dimension >  RGBImageType;

                       
  //
  // We instantiate reader and writer types
  //
  typedef  itk::ImageFileReader< InternalImageType   >  ReaderType;
  typedef  itk::ImageFileWriter< RGBImageType  >        WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  //
  //  Instantiate the GradientMagnitude image filter
  //
  typedef   itk::GradientMagnitudeRecursiveGaussianImageFilter<
                                                     InternalImageType,
                                                     InternalImageType 
                                                          > GradientMagnitudeFilterType;

  GradientMagnitudeFilterType::Pointer gradienMagnitudeFilter = GradientMagnitudeFilterType::New();

  gradienMagnitudeFilter->SetInput( reader->GetOutput() );
  gradienMagnitudeFilter->SetSigma( 1.0 );


  //
  //  Instantiate the Watershed filter
  //

  typedef  itk::WatershedImageFilter< 
                              InternalImageType 
                                            > WatershedFilterType;

  WatershedFilterType::Pointer watershedFilter = WatershedFilterType::New();

  watershedFilter->SetInput( gradienMagnitudeFilter->GetOutput() );

  watershedFilter->SetThreshold( atof( argv[3] ) );
  watershedFilter->SetLevel(     atof( argv[4] ) );


  //
  //  Instantiate the filter that will encode the label image
  //  into a color image (random color attribution).
  //

  typedef itk::Functor::ScalarToRGBPixelFunctor< 
                                           unsigned long
                                                    > ColorMapFunctorType;

  typedef WatershedFilterType::OutputImageType  LabeledImageType;

  typedef itk::UnaryFunctorImageFilter< 
                                LabeledImageType,
                                RGBImageType,
                                ColorMapFunctorType
                                                > ColorMapFilterType;

  ColorMapFilterType::Pointer colorMapFilter = ColorMapFilterType::New();

  colorMapFilter->SetInput(  watershedFilter->GetOutput() );

  writer->SetInput( colorMapFilter->GetOutput() );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }


  return 0;

}
