/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorGradientMagnitudeImageFilterTest2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <fstream>
#include "itkVectorGradientMagnitudeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkRGBPixel.h"
#include "itkImageRegionIterator.h"
#include "itkRescaleIntensityImageFilter.h"
#include "vnl/vnl_math.h"


int itkVectorGradientMagnitudeImageFilterTest2(int ac, char* av[] )
{
  typedef itk::RGBPixel<unsigned char> RGBPixelType;
  typedef itk::Image<RGBPixelType, 3> RGBImageType;
  typedef itk::Image<unsigned char, 3> CharImage3Type;
  typedef itk::Image<unsigned char, 2> CharImage2Type;
  typedef itk::VectorGradientMagnitudeImageFilter<RGBImageType> FilterType;
  typedef itk::ImageFileReader<RGBImageType> ReaderType;
  typedef itk::RescaleIntensityImageFilter<FilterType::OutputImageType,
    CharImage3Type> RescaleFilterType; 
  typedef itk::ImageFileWriter<CharImage2Type> WriterType;


  if(ac < 5)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage Mode SliceToExtract\n";
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
  writer->SetFileName( av[2] );
  
  try
    {
      rescale->Update();

      // Extract one slice to write for regression testing
      CharImage3Type::RegionType extractedRegion = rescale->GetOutput()->GetRequestedRegion();
      extractedRegion.SetSize(2,1);
      extractedRegion.SetIndex(2,::atoi(av[4]));

      CharImage2Type::Pointer extractedImage = CharImage2Type::New();
      CharImage2Type::RegionType reg;
      reg.SetSize(0,extractedRegion.GetSize()[0]);
      reg.SetSize(1,extractedRegion.GetSize()[1]);
      reg.SetIndex(0,0);
      reg.SetIndex(1,0);
      extractedImage->SetRegions(reg);
      extractedImage->Allocate();
      double sp[2];
      sp[0] = rescale->GetOutput()->GetSpacing()[0];
      sp[1] = rescale->GetOutput()->GetSpacing()[1];
      extractedImage->SetSpacing(sp);

      itk::ImageRegionIterator<CharImage3Type> in(rescale->GetOutput(), extractedRegion);
      itk::ImageRegionIterator<CharImage2Type> out(extractedImage,
                                                   extractedImage->GetRequestedRegion());
      
      for (; !in.IsAtEnd(); ++in, ++out)
        { out.Set(in.Get()); }
      
      writer->SetInput( extractedImage );
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
