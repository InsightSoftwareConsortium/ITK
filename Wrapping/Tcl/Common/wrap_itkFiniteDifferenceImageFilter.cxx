/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkFiniteDifferenceImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImage.h"
#include "itkFiniteDifferenceImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"

#define ITK_WRAP_FDIF(x, y) \
  ITK_WRAP_IMAGE_TO_IMAGE(FiniteDifferenceImageFilter, x, y)

ITK_WRAP_CONFIG_GROUP(itkFiniteDifferenceImageFilter);
ITK_WRAP_DEFINE_IMAGE_TYPES();
ITK_WRAP_FDIF(F2, F2);
ITK_WRAP_FDIF(US2, F2);
ITK_WRAP_FDIF(F3, F3);
ITK_WRAP_FDIF(US3, F3);

#endif
