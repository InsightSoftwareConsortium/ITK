/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorGradientMagnitudeImageFilterTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <fstream>
#include "itkVectorGradientMagnitudeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkRGBPixel.h"

int itkVectorGradientMagnitudeImageFilterTest1(int ac, char* av[] )
{
  typedef itk::RGBPixel<unsigned short> RGBPixelType;
  typedef itk::Image<unsigned char, 2> CharImageType;
  typedef itk::Image<RGBPixelType, 2> RGBImageType;
  typedef itk::VectorGradientMagnitudeImageFilter<RGBImageType> FilterType;
  typedef itk::ImageFileReader<RGBImageType> ReaderType;
  typedef itk::RescaleIntensityImageFilter<FilterType::OutputImageType,
    CharImageType> RescaleFilterType; 
  typedef itk::ImageFileWriter<CharImageType> WriterType;

  if(ac < 4)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage Mode\n";
    return -1;
    }

  // Create a reader and filter
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(av[1]);
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(reader->GetOutput());

  int mode = ::atoi( av[3] );
  if ( mode == 1)
    {
      filter->SetUsePrincipleComponentsOn();
    }
  else
    {
      filter->SetUsePrincipleComponentsOff();
    }

  RescaleFilterType::Pointer rescale = RescaleFilterType::New();
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);
  rescale->SetInput( filter->GetOutput() );

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( rescale->GetOutput() );
  writer->SetFileName( av[2] );

  try
    {
      writer->Update();
    }
  catch (itk::ExceptionObject& e)
    {
      std::cerr << "Exception detected: "  << e.GetDescription();
      return -1;
    }
  catch (...)
    {
      std::cerr << "Some other exception occurred" << std::endl;
      return -2;
    }

  std::cout <<  "The gradient image range was (low, high) = ("
            <<  rescale->GetInputMinimum() << ", " << rescale->GetInputMaximum()
            << ")" << std::endl;
  std::cout <<  "Output was scaled, shifted = " << rescale->GetScale() << ", "
            << rescale->GetShift() << std::endl;
 
  return 0;
}
