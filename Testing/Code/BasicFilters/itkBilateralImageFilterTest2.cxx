/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBilateralImageFilterTest2.cxx
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
#include "itkImageRegionIterator.h"

int itkBilateralImageFilterTest2(int ac, char* av[] )
{
  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage\n";
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
  
  // these settings reduce the amount of noise by a factor of 10
  // when the original signal to noise level is 5
  filter->SetDomainSigma( 4.0 );
  filter->SetRangeSigma( 50.0 );
  
  
  // Test itkSetVectorMacro
  float domainSigma[dimension];
  for (unsigned int i = 0; i < dimension; i++)
    {
      domainSigma[i] = 4.0f;
    }
  filter->SetDomainSigma(domainSigma);

  // Test itkGetVectorMacro
  const double * domainSigma2 = filter->GetDomainSigma();
  std::cout << "filter->GetDomainSigma(): " << domainSigma2 << std::endl;

  // Test itkSetMacro
  double filterDimensionality = dimension;
  unsigned long  numberOfRangeGaussianSamples = 100;
  filter->SetFilterDimensionality(filterDimensionality); 
  filter->SetNumberOfRangeGaussianSamples(numberOfRangeGaussianSamples);
  
  // Test itkGetMacro
  const double rangeSigma2 = filter->GetRangeSigma();
  std::cout << "filter->GetRangeSigma(): " << rangeSigma2 << std::endl;
  double filterDimensionality2 = filter->GetFilterDimensionality();
  std::cout << "filter->GetFilterDimensionality(): " << filterDimensionality2 << std::endl;
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
  catch (...)
    {
    std::cerr << "Some other exception occurred" << std::endl;
    return -2;
    }

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
    writer = itk::ImageFileWriter<myImage>::New();
    writer->SetInput( filter->GetOutput() );
    writer->SetFileName( av[2] );
    writer->Update();

  return 0;
}
