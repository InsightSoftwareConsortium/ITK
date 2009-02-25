/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHessianToObjectnessMeasureImageFilterTest.cxx
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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImage.h"
#include "itkHessianRecursiveGaussianImageFilter.h"

int itkHessianToObjectnessMeasureImageFilterTest( int argc, char *argv[] )
{
  if ( argc < 3 )
    {
    std::cerr << "Missing Parameters: "
              << argv[0]
              << " Input_Image"
              << " Enhanced_Output_Image [ObjectDimension] [Bright/Dark]" << std::endl;
    return EXIT_FAILURE;
    }

  // Define the dimension of the images
  const unsigned char Dim = 2;

  typedef float PixelType;

  // Declare the types of the images
  typedef itk::Image<PixelType,Dim> ImageType;

  typedef itk::ImageFileReader<ImageType> FileReaderType;
  typedef itk::ImageFileWriter<ImageType> FileWriterType;

  typedef itk::RescaleIntensityImageFilter<ImageType> RescaleFilterType;
 
  // Declare the type of the recursive Gaussian filter
  typedef itk::HessianRecursiveGaussianImageFilter<
                                            ImageType >  GaussianImageFilterType;

  typedef GaussianImageFilterType::OutputImageType        HessianImageType;

  // Delcare the type of objectness measure image filter

  typedef itk::HessianToObjectnessMeasureImageFilter<HessianImageType, ImageType > ObjectnessFilterType;
 
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

  // Create a Gaussian Filter
  GaussianImageFilterType::Pointer gaussianFilter = GaussianImageFilterType::New();

  // Create a vesselness Filter
  ObjectnessFilterType::Pointer objectnessFilter = ObjectnessFilterType::New();

  // Connect the input images
  gaussianFilter->SetInput( imageReader->GetOutput() );
  objectnessFilter->SetInput( gaussianFilter->GetOutput() );
  objectnessFilter->SetScaleObjectnessMeasure(false);
  objectnessFilter->SetBrightObject(true);
  objectnessFilter->SetAlpha(0.5);
  objectnessFilter->SetBeta(0.5);
  objectnessFilter->SetGamma(5.0);

  if ( argc >= 3 )
    {
    objectnessFilter->SetObjectDimension( atoi(argv[3]) );
    }

  if ( argc >= 4 )
    {
    objectnessFilter->SetBrightObject( atoi(argv[4]) );
    }

  try
    {
    objectnessFilter->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }
   
  FileWriterType::Pointer writer = FileWriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(objectnessFilter->GetOutput());
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
