/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKappaStatisticImageToImageMetricTest.cxx
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
#include "itkKappaStatisticImageToImageMetric.h"
#include "itkImage.h"
#include "itkArray.h"
#include "itkTranslationTransform.h"
#include "itkExceptionObject.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkImageFileReader.h"

#include <iostream>

int itkKappaStatisticImageToImageMetricTest(int argc, char* argv[] )
{

  if (argc < 2)
    {
    std::cout << "Usage: " << argv[0] << " InputFile" << std::endl;
    exit (1);
    }

  typedef itk::Image<unsigned char,2> ImageType;
  typedef itk::TranslationTransform<double, 2> TransformType;
  typedef TransformType::OutputVectorType OffsetType;
  typedef itk::KappaStatisticImageToImageMetric<ImageType,ImageType> MetricType;
  typedef itk::ImageFileReader<ImageType> ReaderType;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double> InterpolatorType;

  ReaderType::Pointer reader = ReaderType::New();
  MetricType::Pointer metric = MetricType::New();
  TransformType::Pointer transform = TransformType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  itk::Array<double> offset(2);                                                        

  reader->SetFileName (argv[1]);
  reader->Update();

  metric->SetMovingImage (reader->GetOutput());
  metric->SetFixedImage (reader->GetOutput());
  metric->SetInterpolator (interpolator);
  metric->SetTransform (transform);
  metric->SetFixedImageRegion (reader->GetOutput()->GetLargestPossibleRegion());
  metric->SetForegroundValue (255);

  metric->Initialize();

  unsigned int numberOfParameters = transform->GetNumberOfParameters();

  std::cout << "First measure matches..." << std::endl;
  for (float x = -200.0; x <= 200.0; x+= 50.0)
    {
    offset[0] = x;
    for (float y = 0.0; y <= 0.0; y+= 10.0)
      {
      offset[1] = y;
      try
        {
        MetricType::DerivativeType derivative( numberOfParameters );
        metric->GetDerivative(offset, derivative);
        std::cout << "Offset: " 
                  << offset 
                  << " = " 
                  << metric->GetValue(offset)
                  << ", "
                  << derivative
                  << std::endl;
        }
      catch( itk::ExceptionObject & exp )
        {
        std::cerr << "Exception thrown while computing the metric " << std::endl;
        std::cerr << exp << std::endl;
        return EXIT_FAILURE;
        }
      }
    }
      
  std::cout << "Now measure complement..." << std::endl;
  metric->ComplementOn();

  for (float x = -200.0; x <= 200.0; x+= 50.0)
    {
    offset[0] = x;
    for (float y = 0.0; y <= 0.0; y+= 10.0)
      {
      offset[1] = y;
      try
        {
        MetricType::DerivativeType derivative( numberOfParameters );
        metric->GetDerivative(offset, derivative);
        std::cout << "Offset: " 
                  << offset 
                  << " = " 
                  << metric->GetValue(offset)
                  << ", "
                  << derivative
                  << std::endl;
        }
      catch( itk::ExceptionObject & exp )
        {
        std::cerr << "Exception thrown while computing the metric " << std::endl;
        std::cerr << exp << std::endl;
        return EXIT_FAILURE;
        }
      }
    }
      
  metric->Print (std::cout);
  return EXIT_SUCCESS;

}

