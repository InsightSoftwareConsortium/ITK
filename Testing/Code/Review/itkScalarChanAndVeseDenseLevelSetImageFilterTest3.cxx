/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $$
  Language:  C++
  Date:      $$
  Version:   $$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkScalarChanAndVeseDenseLevelSetImageFilter.h"
#include "itkScalarChanAndVeseLevelSetFunction.h"
#include "itkScalarChanAndVeseLevelSetFunctionData.h"
#include "itkUnconstrainedRegionBasedLevelSetFunctionSharedData.h"
#include "itkImage.h"
#include "itkTestingMacros.h"


int itkScalarChanAndVeseDenseLevelSetImageFilterTest3( int, char* [] )
{
  const unsigned int Dimension = 3;

  typedef double                                  PixelType;
  typedef itk::Image< PixelType, Dimension >      ImageType;
  typedef itk::Image< float, Dimension >          FeatureImageType;
  typedef ImageType                               OutputImageType;

  typedef itk::ScalarChanAndVeseLevelSetFunctionData< ImageType, FeatureImageType >  DataHelperType;

  typedef itk::UnconstrainedRegionBasedLevelSetFunctionSharedData< ImageType, FeatureImageType, DataHelperType >
    SharedDataHelperType;


  typedef itk::ScalarChanAndVeseLevelSetFunction<
    ImageType, FeatureImageType, SharedDataHelperType >     RegionBasedLevelSetFunctionType;

  RegionBasedLevelSetFunctionType::Pointer function = RegionBasedLevelSetFunctionType::New();

  typedef itk::ScalarChanAndVeseDenseLevelSetImageFilter<
    ImageType, FeatureImageType, OutputImageType,
    RegionBasedLevelSetFunctionType, SharedDataHelperType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  std::cout << "GetNameOfClass() = " << filter->GetNameOfClass() << std::endl;
  filter->Print( std::cout );

  return EXIT_SUCCESS;
}
