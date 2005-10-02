/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelStatisticsImageFilterTest.cxx
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

#include <iostream>

#include "itkImage.h"
#include "itkLabelStatisticsImageFilter.h"
#include "itkImageFileReader.h"
#include "itkNumericTraits.h"

#include "itkFilterWatcher.h"

#include "vnl/vnl_math.h"

int itkLabelStatisticsImageFilterTest(int argc, char* argv [] )
{
  std::cout << "itkLabelStatisticsImageFilterTest Start" << std::endl;

  if( argc < 2 )
  {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage labeledImage " << std::endl;
    return EXIT_FAILURE;
  }
  int status = 0;
  typedef itk::Image<unsigned char,2> ImageType;

  typedef itk::ImageFileReader< ImageType >    ReaderType;

  ReaderType::Pointer reader1 = ReaderType::New();
  ReaderType::Pointer reader2 = ReaderType::New();

  reader1->SetFileName( argv[1] );
  reader2->SetFileName( argv[2] );

  typedef itk::LabelStatisticsImageFilter< ImageType, ImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  FilterWatcher filterWatch( filter );
  
  filter->SetInput (      reader1->GetOutput() );
  filter->SetLabelInput ( reader2->GetOutput() );
  filter->UseHistogramsOn();
  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught ! " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int numberOfObjects  = filter->GetNumberOfObjects();
  const unsigned int numberOfLabels   = filter->GetNumberOfLabels();

  typedef FilterType::RealType          RealType;
  typedef FilterType::BoundingBoxType   BoundingBoxType;
  typedef FilterType::RegionType        RegionType;
  typedef FilterType::LabelPixelType    LabelPixelType;

  LabelPixelType labelValue;

  std::cout << "There are " << numberOfLabels << " labels" << std::endl;
  std::cout << "There are " << numberOfObjects << " objects" << std::endl;

// Try two labels: one that exists and one that does not
  for (int i = 0; i < 2; i++)
    {
    // Find an existing label
    if (i == 0)
      {
      labelValue = 0;
      while (!filter->HasLabel(labelValue))
        {
        labelValue++;
        }
      std::cout << "Label Statistics for label " 
                << static_cast<itk::NumericTraits<LabelPixelType>::PrintType>(labelValue)
                << " which exists" << std::endl;
      }
    // Find a non existant label
    if (i != 0)
      {
      labelValue = 0;
      while (filter->HasLabel(labelValue))
        {
        labelValue++;
        }
      std::cout << "Label Statistics for label "
                << static_cast<itk::NumericTraits<LabelPixelType>::PrintType>(labelValue)
                << " which does not exist" << std::endl;
      }
    const RealType min      =  filter->GetMinimum( labelValue );
    const RealType max      =  filter->GetMaximum( labelValue );
    const RealType median   =  filter->GetMedian( labelValue );
    const RealType mean     =  filter->GetMean( labelValue );
    const RealType sigma    =  filter->GetSigma( labelValue );
    const RealType variance =  filter->GetVariance( labelValue );
    const RealType sum      =  filter->GetSum( labelValue );
    const BoundingBoxType box = filter->GetBoundingBox( labelValue );
    const RegionType region = filter->GetRegion( labelValue );
    
    std::cout << "Minimum   = " << min      << std::endl;
    std::cout << "Maximum   = " << max      << std::endl;
    std::cout << "Median    = " << median   << std::endl;
    std::cout << "Mean      = " << mean     << std::endl;
    std::cout << "Sigma     = " << sigma    << std::endl;
    std::cout << "Variance  = " << variance << std::endl;
    std::cout << "Sum       = " << sum      << std::endl;
    std::cout << "Region    = " << region   << std::endl;

    BoundingBoxType::const_iterator itr = box.begin();
    while( itr != box.end() )
      {
      std::cout << "Index = " << *itr << std::endl;
      ++itr;
      }
    }
  return EXIT_SUCCESS;
}

