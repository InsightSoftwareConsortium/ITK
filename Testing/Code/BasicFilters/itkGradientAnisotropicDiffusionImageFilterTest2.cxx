/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientAnisotropicDiffusionImageFilterTest2.cxx
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
#include "itkCastImageFilter.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPNGImageIO.h"
#include "itkPNGImageIOFactory.h"
#include "itkImageRegionIterator.h"


int itkGradientAnisotropicDiffusionImageFilterTest2(int ac, char* av[] )
{
  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage\n";
    return -1;
    }

  typedef float PixelType;
  typedef itk::Image<PixelType, 2> myFloatImage;
  itk::ImageFileReader<myFloatImage>::Pointer input 
    = itk::ImageFileReader<myFloatImage>::New();
  input->SetFileName(av[1]);
  
  // Create a filter
  itk::GradientAnisotropicDiffusionImageFilter<myFloatImage, myFloatImage>
    ::Pointer filter
    = itk::GradientAnisotropicDiffusionImageFilter<myFloatImage, myFloatImage>
    ::New();
  filter->SetNumberOfIterations(16);
  filter->SetConductanceParameter(3.0f);
  filter->SetTimeStep(0.125f);
  
  filter->SetInput(input->GetOutput());

  typedef itk::Image<unsigned char, 2> myUCharImage;
  itk::CastImageFilter<myFloatImage, myUCharImage>::Pointer caster
    = itk::CastImageFilter<myFloatImage, myUCharImage>::New();
  caster->SetInput(filter->GetOutput());
  
  try
    {
    input->Update();
    caster->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return -1;
    }

  // Generate test image
  itk::ImageFileWriter<myUCharImage>::Pointer writer;
    writer = itk::ImageFileWriter<myUCharImage>::New();
    writer->SetInput( caster->GetOutput() );
    std::cout << "Writing " << av[2] << std::endl;
    writer->SetFileName( av[2] );
    writer->Update();

  return 0;
}
