/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsImageFilterTest.cxx
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

#include <iostream>

#include "itkImage.h"
#include "itkImageRegionIterator.h"

#include "itkStatisticsImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkFilterWatcher.h"
#include "vnl/vnl_math.h"

int itkStatisticsImageFilterTest(int, char* [] )
{
  std::cout << "itkStatisticsImageFilterTest Start" << std::endl;

  int status = 0;
  typedef itk::Image<int,3> FloatImage;

  FloatImage::Pointer    image  = FloatImage::New();
  FloatImage::RegionType region;
  FloatImage::SizeType   size; size.Fill(64);
  FloatImage::IndexType  index; index.Fill(0);

  region.SetIndex (index);
  region.SetSize (size);

  // first try a constant image
  float fillValue = -100.0;
  image->SetRegions( region );
  image->Allocate();
  image->FillBuffer( static_cast< FloatImage::PixelType >( fillValue ) );

  float sum = fillValue * static_cast<float>( region.GetNumberOfPixels() );

  typedef itk::StatisticsImageFilter<FloatImage> FilterType;
  FilterType::Pointer filter = FilterType::New();

  FilterWatcher filterWatch(filter);
  
  filter->SetInput (image);
  filter->UpdateLargestPossibleRegion();

  if (filter->GetMinimum() != fillValue)
    {
    std::cerr << "GetMinimum failed! Got " << filter->GetMinimum() << " but expected " << fillValue << std::endl;
    status++;
    }
  if (filter->GetMaximum() != fillValue)
    {
    std::cerr << "GetMaximum failed! Got " << filter->GetMaximum() << " but expected " << fillValue << std::endl;
    status++;
    }
  if (filter->GetSum() != sum)
    {
    std::cerr << "GetSum failed! Got " << filter->GetSum() << " but expected " << sum << std::endl;
    status++;
    }
  if (filter->GetMean() != fillValue)
    {
    std::cerr << "GetMean failed! Got " << filter->GetMean() << " but expected " << fillValue << std::endl;
    status++;
    }
  if (filter->GetVariance() != 0.0)
    {
    std::cerr << "GetVariance failed! Got " << filter->GetVariance() << " but expected " << 0.0 << std::endl;
    status++;
    }


  // Now generate a real image

  typedef itk::RandomImageSource<FloatImage> SourceType;
  SourceType::Pointer source = SourceType::New();
  unsigned long randomSize[3] = {17, 8, 20};

  source->SetSize(randomSize);
  float minValue = -100.0;
  float maxValue = 1000.0;

  source->SetMin( static_cast< FloatImage::PixelType >( minValue ) );
  source->SetMax( static_cast< FloatImage::PixelType >( maxValue ) );

  filter->SetInput(source->GetOutput());
  filter->UpdateLargestPossibleRegion();
  
  double expectedSigma = sqrt((maxValue-minValue)*(maxValue-minValue)/12.0);
  double epsilon = (maxValue - minValue) * .001;

  if (vnl_math_abs(filter->GetSigma() - expectedSigma) > epsilon)
    {
    std::cerr << "GetSigma failed! Got " << filter->GetSigma() << " but expected " << expectedSigma << std::endl;
    }

  return status;
}
