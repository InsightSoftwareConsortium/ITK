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
#include "itkRescaleIntensityImageFilter.h"
#include "itkImage.h"

int itkMultiScaleHessianBasedMeasureImageFilterTest( int argc, char *argv[] )
{
  if ( argc < 4 )
    {
    std::cerr << "Missing Parameters: "
              << argv[0]
              << " Input_Image"
              << " Enhanced_Output_Image [SigmaMin SigmaMax NumberOfScales ObjectDimension Bright/Dark]" << std::endl;
    return EXIT_FAILURE;
    }

  // Define the dimension of the images
  const unsigned int Dimension = 2;

  typedef float                                 InputPixelType;
  typedef itk::Image<InputPixelType,Dimension>  InputImageType;


  typedef float                                 OutputPixelType;
  typedef itk::Image<OutputPixelType,Dimension> OutputImageType;

  typedef itk::ImageFileReader<InputImageType>  FileReaderType;


  typedef float                                       WriteOutputPixelType;
  typedef itk::Image<WriteOutputPixelType,Dimension>  WriteOutputImageType;

  typedef itk::ImageFileWriter<WriteOutputImageType> FileWriterType;

  typedef itk::RescaleIntensityImageFilter<OutputImageType, WriteOutputImageType> RescaleFilterType;

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

   if ( argc >= 4 )
    {
    multiScaleEnhancementFilter->SetSigmaMinimum( atof(argv[3])  );
    }

  if ( argc >= 5 )
    {
    multiScaleEnhancementFilter->SetSigmaMaximum( atof(argv[4]) );
    }

  if ( argc >= 6 )
    {
    multiScaleEnhancementFilter->SetNumberOfSigmaSteps( atoi(argv[5]) );
    }

  if ( argc >= 7 )
    {
    objectnessFilter->SetObjectDimension( atoi(argv[6]) );
    }

  if ( argc >= 8 )
    {
    objectnessFilter->SetBrightObject( atoi(argv[7]) );
    }

  try
    {
    multiScaleEnhancementFilter->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  RescaleFilterType::Pointer rescale = RescaleFilterType::New();
  rescale->SetInput(multiScaleEnhancementFilter->GetOutput());
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);

  FileWriterType::Pointer writer = FileWriterType::New();
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();
  writer->SetInput( rescale->GetOutput() );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  return EXIT_SUCCESS;
}
