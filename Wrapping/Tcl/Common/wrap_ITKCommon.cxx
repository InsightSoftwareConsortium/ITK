/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKCommon.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"
namespace _cable_
{
  const char* const package = ITK_WRAP_PACKAGE;
  const char* const package_version = ITK_WRAP_PACKAGE_VERSION;
  const char* const groups[] =
  {
    ITK_WRAP_GROUP(ITKBase),
    ITK_WRAP_GROUP(ITKFilterBase),
    ITK_WRAP_GROUP(ITKInterpolators),
    ITK_WRAP_GROUP(ITKRegions),
    ITK_WRAP_GROUP(ITKTransforms),
#ifndef CSWIG
    ITK_WRAP_GROUP(ITKUtils),
#endif
    ITK_WRAP_GROUP(itkArray),
    ITK_WRAP_GROUP(itkContinuousIndex),
    ITK_WRAP_GROUP(itkDifferenceImageFilter),
    ITK_WRAP_GROUP(itkDenseFiniteDifferenceImageFilter),
    ITK_WRAP_GROUP(itkEventObject),
    ITK_WRAP_GROUP(itkFiniteDifferenceImageFilter),
    ITK_WRAP_GROUP(itkFixedArray),
    ITK_WRAP_GROUP(itkImage),
    ITK_WRAP_GROUP(itkIndex),
    ITK_WRAP_GROUP(itkPoint),
    ITK_WRAP_GROUP(itkSize),
    ITK_WRAP_GROUP(itkVector)
  };
}
#endif
