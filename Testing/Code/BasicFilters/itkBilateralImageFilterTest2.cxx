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
#include "itkPNGImageIO.h"
#include "itkPNGImageIOFactory.h"
#include "itkImageRegionIterator.h"


int itkBilateralImageFilterTest2(int ac, char* av[] )
{
  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage BaselineImage\n";
    return -1;
    }

  // Register one Factory of PNG readers
  //itk::PNGImageIOFactory::RegisterOneFactory();
  
  typedef unsigned char PixelType;
  typedef itk::Image<PixelType, 2> myImage;
  itk::ImageFileReader<myImage>::Pointer input 
    = itk::ImageFileReader<myImage>::New();
  input->SetFileName(av[1]);
//     itk::PNGImageIO::Pointer io1;
//       io1 = itk::PNGImageIO::New();
//  input->SetImageIO(io1);
    
  
  // Create a filter
  typedef itk::BilateralImageFilter<myImage,myImage> FilterType;

  FilterType::Pointer filter = FilterType::New();
    filter->SetInput(input->GetOutput());

    // these settings reduce the amount of noise by a factor of 10
    // when the original signal to noise level is 5
    filter->SetDomainSigma( 4.0 );
    filter->SetRangeSigma( 50.0 );
    
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

  // Code used to generate the baseline image, commented out when run in
  // regression test mode.
  // 
  // itk::PNGImageIO::Pointer io;
  // io = itk::PNGImageIO::New();
  //
  // itk::ImageFileWriter<myImage>::Pointer writer;
  // writer = itk::ImageFileWriter<myImage>::New();
  // writer->SetInput( filter->GetOutput() );
  // writer->SetFileName( av[2] );
  // writer->SetImageIO( io );
  // writer->Update();

  // now read the regression image
  itk::ImageFileReader<myImage>::Pointer baseline 
    = itk::ImageFileReader<myImage>::New();
    baseline->SetFileName(av[2]);
//     itk::PNGImageIO::Pointer io2;
//       io2 = itk::PNGImageIO::New();
//     baseline->SetImageIO(io2);
    
  try
    {
    baseline->Update();
    }
  catch (itk::ImageFileReaderException& e)
    {
    std::cerr << "Exception in file reader: "  << e.GetDescription() << std::endl;
    return -3;
    }
  catch (...)
    {
    std::cerr << "Some other exception occurred" << std::endl;
    return -4;
    }

  
  // compare the two images
  itk::ImageRegionIterator<myImage> it(filter->GetOutput(),filter->GetOutput()->GetBufferedRegion());
  itk::ImageRegionIterator<myImage> rit(baseline->GetOutput(),baseline->GetOutput()->GetBufferedRegion());
   unsigned long status = 0;
   while (!it.IsAtEnd())
     {
     if (it.Get() != rit.Get())
       {
       status++;
       } 
     ++it;
     ++rit;  
     }

  itk::ObjectFactoryBase::UnRegisterAllFactories();

  return status;
}
