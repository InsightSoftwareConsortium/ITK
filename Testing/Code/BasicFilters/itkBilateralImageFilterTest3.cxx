/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBilateralImageFilterTest3.cxx
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
#include "itkBilateralImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPNGImageIO.h"
#include "itkPNGImageIOFactory.h"
#include "itkImageRegionIterator.h"


int itkBilateralImageFilterTest3(int ac, char* av[] )
{
  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage BaselineImage\n";
    return -1;
    }

  typedef unsigned char PixelType;
  typedef itk::Image<PixelType, 2> myImage;
  itk::ImageFileReader<myImage>::Pointer input 
    = itk::ImageFileReader<myImage>::New();
  input->SetFileName(av[1]);
  
  // Create a filter
  typedef itk::BilateralImageFilter<myImage,myImage> FilterType;

  FilterType::Pointer filter1 = FilterType::New();
    filter1->SetInput(input->GetOutput());
  FilterType::Pointer filter2 = FilterType::New();
    filter2->SetInput(filter1->GetOutput());
  FilterType::Pointer filter3 = FilterType::New();
    filter3->SetInput(filter2->GetOutput());

    // Instead of using a single agressive smoothing filter, use 3
    // less aggressive filters.
    //
    // These settings match the "wedding" cake image (cake_easy.png) where
    // the signal to noise ratio is 5 (step heights near 100 units,
    // noise sigma near 20 units). A single filter stage with these
    // settings cuts the noise level in half.  These three stages should
    // reduce the amount of noise by a factor of 8. This is comparable to
    // the noise reduction in using a single stage with parameters
    // (4.0, 50.0).  The difference is that with 3 less aggressive stages
    // the edges are preserved better.
    filter1->SetDomainSigma( 4.0 );
    filter1->SetRangeSigma( 20.0 );
    filter2->SetDomainSigma( 4.0 );
    filter2->SetRangeSigma( 20.0 );
    filter3->SetDomainSigma( 4.0 );
    filter3->SetRangeSigma( 20.0 );
    
  try
    {
    input->Update();
    filter3->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return -1;
    }

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
    writer = itk::ImageFileWriter<myImage>::New();
    writer->SetInput( filter3->GetOutput() );
    writer->SetFileName( av[2] );
    writer->Update();

  return 0;
}
