/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBilateralImageFilterTest4.cxx
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


int itkBilateralImageFilterTest4(int ac, char* av[] )
{
  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage BaselineImage\n";
    return -1;
    }

  typedef unsigned char PixelType;
  const unsigned int dimension = 2;
  typedef itk::Image<PixelType, dimension> myImage;
  itk::ImageFileReader<myImage>::Pointer input 
    = itk::ImageFileReader<myImage>::New();
  input->SetFileName(av[1]);
  
  // Create a filter
  typedef itk::BilateralImageFilter<myImage,myImage> FilterType;
  
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(input->GetOutput());
  
  filter->SetDomainSigma( 4.0 );
  filter->SetRangeSigma( 20.0 );
  
  // Test itkSetVectorMacro and itkGetVectorMacro
  double domainSigma[dimension]; 
  for (unsigned int i = 0; i < dimension; i++)
    {
      domainSigma[i] = 4.0f;
    }
  filter->SetDomainSigma(domainSigma);
  
  float domainSigma2[dimension];
  for (unsigned int i = 0; i < dimension; i++)
    {
      domainSigma2[i] = 4.0f;
    }
  filter->SetDomainSigma(domainSigma2);
  
  // Test itkSetMacro and itkGetMacro
  const double rangeSigma = 50.0f;
  double filterDimensionality = dimension;
  unsigned long  numberOfRangeGaussianSamples = 100;
  
  filter->SetRangeSigma(rangeSigma);
  const double rangeSigma2 = filter->GetRangeSigma();
  std::cout << "filter->GetRangeSigma(): " << rangeSigma2 << std::endl;
  filter->SetFilterDimensionality(filterDimensionality);
  double filterDimensionality2 = filter->GetFilterDimensionality();
  std::cout << "filter->GetFilterDimensionality(): " << filterDimensionality2 << std::endl;
  filter->SetNumberOfRangeGaussianSamples(numberOfRangeGaussianSamples);
  unsigned long numberOfRangeGaussianSamples2 = filter->GetNumberOfRangeGaussianSamples();
  std::cout << "filter->GetNumberOfRangeGaussianSamples(): " << numberOfRangeGaussianSamples2 << std::endl;
 
  try
    {
      input->Update();
      filter->Update();
    }
  catch (itk::ExceptionObject& e)
    {
      std::cerr << "Exception detected: "  << e.GetDescription();
      return -1;
    }
  
  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( av[2] );
  writer->Update();
  
  return 0;
}
