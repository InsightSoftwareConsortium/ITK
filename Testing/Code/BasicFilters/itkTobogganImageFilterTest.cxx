/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTobogganImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTobogganImageFilter.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPNGImageIOFactory.h"
#include "itkPNGImageIO.h"
#include "itkTextOutput.h"
#include "itkImageRegionIterator.h"


int itkTobogganImageFilterTest(int ac, char* av[] )
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage BaselineImage [OutputImage]\n";
    exit ( 1 );
    }

  // Register one Factory of PNG readers
  itk::PNGImageIOFactory::RegisterOneFactory();
  
  typedef unsigned char PixelType;
  typedef itk::Image<unsigned char, 2> InputImageType;
  typedef itk::Image<float, 2> FloatImageType;
  typedef itk::Image<unsigned char, 2> OutputImageType;
  typedef itk::Image<unsigned long, 2> LongImageType;
  
  
  // Create a pipeline
  typedef itk::CastImageFilter<InputImageType, FloatImageType> InCastType;
  typedef itk::RecursiveGaussianImageFilter<InputImageType,FloatImageType> GaussianFilterType;
  typedef itk::GradientMagnitudeImageFilter<FloatImageType,FloatImageType> GradientMagnitudeFilterType;
  typedef itk::TobogganImageFilter<FloatImageType> FilterType;
  typedef itk::CastImageFilter<LongImageType, OutputImageType> CastType;
  typedef itk::DiscreteGaussianImageFilter<FloatImageType,FloatImageType> DGIFType;
  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter<FloatImageType,FloatImageType> GMGaussianType;
  
  FilterType::Pointer toboggan = FilterType::New();
  GaussianFilterType::Pointer gaussian = GaussianFilterType::New();
  GradientMagnitudeFilterType::Pointer magnitude = GradientMagnitudeFilterType::New();
  CastType::Pointer cast = CastType::New();
  InCastType::Pointer incast = InCastType::New();
  DGIFType::Pointer discretegaussian = DGIFType::New();
  GMGaussianType::Pointer gmgaussian = GMGaussianType::New();
  
  itk::ImageFileReader<InputImageType>::Pointer input 
    = itk::ImageFileReader<InputImageType>::New();

  input->SetFileName(av[1]);
  incast->SetInput ( input->GetOutput() );
  gmgaussian->SetInput ( incast->GetOutput() );
  gmgaussian->SetSigma ( 15.0 );
  toboggan->SetInput ( gmgaussian->GetOutput() );
  cast->SetInput ( toboggan->GetOutput() );
  try
    {
    input->Update();
    cast->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    exit ( 1 );
    }

  // Try to write it out if we need to...
  if ( ac == 4 )
    {
    itk::PNGImageIO::Pointer io;
    io = itk::PNGImageIO::New();
  
    itk::ImageFileWriter<OutputImageType>::Pointer writer;
    writer = itk::ImageFileWriter<OutputImageType>::New();
    writer->SetInput( cast->GetOutput() );
    writer->SetFileName( av[3] );
    writer->SetImageIO( io );
    writer->Update();
    }

  // now read the regression image
  itk::ImageFileReader<OutputImageType>::Pointer baseline 
    = itk::ImageFileReader<OutputImageType>::New();
    baseline->SetFileName(av[2]);

  try
    {
    baseline->Update();
    }
  catch (itk::ImageFileReaderException& e)
    {
    std::cerr << "Exception in file reader: "  << e.GetDescription() << std::endl;
    exit ( 1 );
    }
  
  // compare the two images
  itk::ImageRegionIterator<OutputImageType> it(cast->GetOutput(),cast->GetOutput()->GetBufferedRegion());
  itk::ImageRegionIterator<OutputImageType> rit(baseline->GetOutput(),baseline->GetOutput()->GetBufferedRegion());
  int status = 0;
  unsigned long int classes = 0;
  while (!it.IsAtEnd())
    {
    if (it.Get() != rit.Get())
      {
      std::cout << "Different pixel at: " << it.GetIndex() << " Baseline: " << (int)rit.Get() << " Computed: " << (int)it.Get() << std::endl;
      status++;
      }
    if ( it.Get() > classes )
      {
      classes = it.Get();
      }
    ++it;
    ++rit;  
    }
  std::cout << "Found " << classes << " Different objects" << std::endl;
  std::cout << "Found " << status << " pixel(s) different" << std::endl;
  if ( status > 1 )
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
