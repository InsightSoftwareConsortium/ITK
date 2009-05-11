/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarChanAndVeseSparseLevelSetImageFilterTest1.cxx
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

#include "itkScalarChanAndVeseSparseLevelSetImageFilter.h"
#include "itkScalarChanAndVeseLevelSetFunction.h"
#include "itkScalarChanAndVeseLevelSetFunctionSharedData.h"
#include "itkImage.h"
#include "itkTestingMacros.h"


int itkScalarChanAndVeseSparseLevelSetImageFilterTest1( int, char* [] )
{
  const unsigned int Dimension = 3;

  typedef double                                  PixelType;
  typedef itk::Image< PixelType, Dimension >      ImageType;
  typedef itk::Image< float, Dimension >          FeatureImageType;
  typedef ImageType                               OutputImageType;

  typedef itk::ScalarChanAndVeseLevelSetFunctionSharedData< 
    ImageType, FeatureImageType >      DataHelperType;


  typedef itk::ScalarChanAndVeseLevelSetFunction< 
    ImageType, FeatureImageType, DataHelperType >      RegionBasedLevelSetFunctionType;

  RegionBasedLevelSetFunctionType::Pointer function = RegionBasedLevelSetFunctionType::New();
 
  typedef itk::ScalarChanAndVeseSparseLevelSetImageFilter< 
    ImageType, FeatureImageType, OutputImageType, 
    RegionBasedLevelSetFunctionType >                     FilterType;
 
  FilterType::Pointer filter = FilterType::New();


  return EXIT_SUCCESS;
}
