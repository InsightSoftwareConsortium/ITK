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
#include <fstream>
#include "itkVectorGradientMagnitudeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkRGBPixel.h"
#include "vnl/vnl_math.h"


int itkVectorGradientMagnitudeImageFilterTest2(int ac, char* av[] )
{
  typedef itk::RGBPixel<unsigned short> RGBPixelType;
  typedef itk::Image<RGBPixelType, 2> RGBImageType;
  typedef itk::VectorGradientMagnitudeImageFilter<RGBImageType> FilterType;
  typedef itk::ImageFileReader<RGBImageType> ReaderType;
  typedef itk::ImageFileWriter<FilterType::OutputImageType> WriterType;
  typedef itk::ImageFileReader<FilterType::OutputImageType> ScalarReaderType;
  
  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage BaselineImage\n";
    return -1;
    }

  // Create a reader and filter
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(av[1]);
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(reader->GetOutput());

  filter->SetUsePrincipleComponentsOff();
  FilterType::RealType component_weights[3] = {0.30, 0.11, 0.59};
  filter->SetComponentWeights(component_weights);
  try
    {
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
  //  WriterType::Pointer writer = WriterType::New();
  //  writer->SetInput( filter->GetOutput() );
  //  writer->SetFileName( av[2] );
  //  writer->Update();
  
  // now read the regression image
  ScalarReaderType::Pointer baseline = ScalarReaderType::New();
  baseline->SetFileName(av[2]);
    
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
  itk::ImageRegionIterator<FilterType::OutputImageType>
    it(filter->GetOutput(),filter->GetOutput()->GetBufferedRegion());
  itk::ImageRegionIterator<FilterType::OutputImageType>
    rit(baseline->GetOutput(),baseline->GetOutput()->GetBufferedRegion());
  unsigned long status = 0;
  const double tolerance = 1e-5;
  while (!it.IsAtEnd())
    {
      if( vnl_math_abs( it.Get() - rit.Get() ) > tolerance )
        {
          // print out the mismatch location and values
          std::cerr << "diff: " << it.GetIndex() << " " << it.Get()
                    << " should be " << rit.Get() << std::endl;
          
          status++;
        } 
      ++it;
      ++rit;  
    }
  
 // itk::ObjectFactoryBase::UnRegisterAllFactories();
  
  return status;
}
