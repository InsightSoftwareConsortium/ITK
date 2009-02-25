/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiScaleHessianBasedMeasureImageFilterTest.cxx
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


#include "itkHessianToObjectnessMeasureImageFilter.h"
#include "itkMultiScaleHessianBasedMeasureImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"

int itkMultiScaleHessianBasedMeasureImageFilterTest( int argc, char *argv[] )
{
  if ( argc < 4 )
    {
    std::cerr << "Missing Parameters: "
              << argv[0]
              << " InputImage"
              << " EnhancedOutputImage ScalesOutputImage "
              << " [SigmaMin SigmaMax NumberOfScales ObjectDimension Bright/Dark]" << std::endl;
    return EXIT_FAILURE;
    }

  // Define the dimension of the images
  const unsigned int Dimension = 2;

  typedef float                                 InputPixelType;
  typedef itk::Image<InputPixelType,Dimension>  InputImageType;


  typedef float                                 OutputPixelType;
  typedef itk::Image<OutputPixelType,Dimension> OutputImageType;

  typedef itk::ImageFileReader<InputImageType>  FileReaderType;

  typedef itk::ImageFileWriter<OutputImageType> FileWriterType;

  typedef itk::NumericTraits< InputPixelType >::RealType RealPixelType;

  typedef itk::SymmetricSecondRankTensor< RealPixelType, Dimension > HessianPixelType;
  typedef itk::Image< HessianPixelType, Dimension >                  HessianImageType;

  // Declare the type of enhancement filter
  typedef itk::HessianToObjectnessMeasureImageFilter< HessianImageType,OutputImageType > ObjectnessFilterType;

  // Declare the type of multiscale enhancement filter
  typedef itk::MultiScaleHessianBasedMeasureImageFilter< InputImageType,HessianImageType, OutputImageType > MultiScaleEnhancementFilterType;

  FileReaderType::Pointer imageReader = FileReaderType::New();
  imageReader->SetFileName(argv[1]);
  try
    {
    imageReader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex << std::endl;
    return EXIT_FAILURE;
    }

  ObjectnessFilterType::Pointer objectnessFilter = ObjectnessFilterType::New();
  objectnessFilter->SetScaleObjectnessMeasure(false);
  objectnessFilter->SetBrightObject(true);
  objectnessFilter->SetAlpha(0.5);
  objectnessFilter->SetBeta(0.5);
  objectnessFilter->SetGamma(5.0);


  MultiScaleEnhancementFilterType::Pointer multiScaleEnhancementFilter = MultiScaleEnhancementFilterType::New();
  multiScaleEnhancementFilter->SetInput(imageReader->GetOutput());
  multiScaleEnhancementFilter->SetHessianToMeasureFilter( objectnessFilter );
  multiScaleEnhancementFilter->SetSigmaStepMethodToLogarithmic();

   if ( argc > 4 )
    {
    multiScaleEnhancementFilter->SetSigmaMinimum( atof(argv[4])  );
    }

  if ( argc > 5 )
    {
    multiScaleEnhancementFilter->SetSigmaMaximum( atof(argv[5]) );
    }

  if ( argc > 6 )
    {
    multiScaleEnhancementFilter->SetNumberOfSigmaSteps( atoi(argv[6]) );
    }

  if ( argc > 7 )
    {
    objectnessFilter->SetObjectDimension( atoi(argv[7]) );
    }

  if ( argc > 8 )
    {
    objectnessFilter->SetBrightObject( atoi(argv[8]) );
    }

  multiScaleEnhancementFilter->GenerateScalesOutputOn();
  multiScaleEnhancementFilter->GenerateHessianOutputOn();

  try
    {
    multiScaleEnhancementFilter->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  FileWriterType::Pointer writer = FileWriterType::New();
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();
  writer->SetInput( multiScaleEnhancementFilter->GetOutput() );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  writer->SetFileName(argv[3]);
  writer->UseCompressionOn();
  writer->SetInput( multiScaleEnhancementFilter->GetScalesOutput() );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  const HessianImageType * hessianImage = 
    multiScaleEnhancementFilter->GetHessianOutput();

  std::cout << "Hessian Image Buffered Region = " << std::endl;
  std::cout << hessianImage->GetBufferedRegion() << std::endl;

  return EXIT_SUCCESS;
}
