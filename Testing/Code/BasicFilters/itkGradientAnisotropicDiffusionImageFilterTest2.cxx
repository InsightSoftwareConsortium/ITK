/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientAnisotropicDiffusionImageFilterTest2.cxx
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

#include <fstream>
#include "itkCastImageFilter.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkDifferenceImageFilter.h"

typedef float PixelType;
typedef itk::Image<PixelType, 2> myFloatImage;
typedef itk::Image<PixelType, 2> ImageType;
typedef ImageType::Pointer ImagePointer;

namespace {

// compare two images with an intensity tolerance
bool SameImage(ImagePointer testImage, ImagePointer baselineImage)
{
  PixelType intensityTolerance = .001; 
  int radiusTolerance = 0;
  unsigned long numberOfPixelTolerance = 0;
    
  typedef itk::DifferenceImageFilter<ImageType,ImageType> DiffType;
  DiffType::Pointer diff = DiffType::New();
  diff->SetValidInput(baselineImage);
  diff->SetTestInput(testImage);
  diff->SetDifferenceThreshold( intensityTolerance );
  diff->SetToleranceRadius( radiusTolerance );
  diff->UpdateLargestPossibleRegion();

  unsigned long status = diff->GetNumberOfPixelsWithDifferences();


  if (status > numberOfPixelTolerance)
    {
    std::cout << "Number of Different Pixels: " << status << std::endl;
    return false;
    }

  return true;
}
}


int itkGradientAnisotropicDiffusionImageFilterTest2(int ac, char* av[] )
{
  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage\n";
    return -1;
    }

 
  itk::ImageFileReader<myFloatImage>::Pointer input 
    = itk::ImageFileReader<myFloatImage>::New();
  input->SetFileName(av[1]);
  
  // Create a filter
  itk::GradientAnisotropicDiffusionImageFilter<myFloatImage, myFloatImage>
    ::Pointer filter
    = itk::GradientAnisotropicDiffusionImageFilter<myFloatImage, myFloatImage>
    ::New();
  filter->SetNumberOfIterations(10);
  filter->SetConductanceParameter(1.0f);
  filter->SetTimeStep(0.125f);
  
  filter->SetInput(input->GetOutput());

  typedef itk::Image<unsigned char, 2> myUCharImage;
  itk::CastImageFilter<myFloatImage, myUCharImage>::Pointer caster
    = itk::CastImageFilter<myFloatImage, myUCharImage>::New();
  caster->SetInput(filter->GetOutput());
  
  try
    {
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

  myFloatImage::Pointer normalImage = filter->GetOutput();
  normalImage->DisconnectPipeline();

  // We now set up testing when the image spacing is not trivial 1 and
  // perform diffusion with spacing on
  typedef itk::ChangeInformationImageFilter<myFloatImage> ChangeInformationType;
  ChangeInformationType::Pointer changeInfo = ChangeInformationType::New();
  changeInfo->SetInput( input->GetOutput() );
  myFloatImage::SpacingType spacing;
  spacing[0] = input->GetOutput()->GetSpacing()[0]*100.0;
  spacing[1] = input->GetOutput()->GetSpacing()[1]*100.0;
  changeInfo->SetOutputSpacing( spacing );
  changeInfo->ChangeSpacingOn();
  
  
  filter->SetInput( changeInfo->GetOutput() );
  filter->UseImageSpacingOn();
  // need to adjust the time step to the number of iterations equates
  // to the same operation
  filter->SetTimeStep( 100.0 * filter->GetTimeStep() );

  try
    {
    filter->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return EXIT_FAILURE;
    }
  
  // the results with spacing should be about the same as without spacing
  if ( !SameImage( filter->GetOutput(), normalImage ) ) 
    {
    std::cout << "Results varied with spacing enabled!" << std::endl;
    return EXIT_FAILURE; 
    }

  return EXIT_SUCCESS;
}
