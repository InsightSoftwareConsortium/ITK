/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKInterpolators.cxx
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
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"

#define ITK_WRAP_IIF(f, x) \
  ITK_WRAP_OBJECT_TEMPLATE_2(f##x, f< Image##x, double >)

#define ITK_WRAP_LIIF(x) \
  ITK_WRAP_IIF(LinearInterpolateImageFunction, x) \
  ITK_WRAP_IIF(InterpolateImageFunction, x) \
  ITK_WRAP_OBJECT_TEMPLATE_2(InterpolateImageFunction##x##_Superclass, \
                             InterpolateImageFunction< Image##x, double>::Superclass) \
  ITK_WRAP_OBJECT_TEMPLATE_2(InterpolateImageFunction##x##_Superclass_Superclass, \
                             InterpolateImageFunction< Image##x, double>::Superclass::Superclass) 

#define ITK_WRAP_NNIIF(x) \
  ITK_WRAP_IIF(NearestNeighborInterpolateImageFunction, x)

ITK_WRAP_CONFIG_GROUP(ITKInterpolators);
ITK_WRAP_DEFINE_IMAGE_TYPES();



ITK_WRAP_LIIF(F2);
ITK_WRAP_LIIF(F3);
ITK_WRAP_LIIF(US2);
ITK_WRAP_LIIF(US3);

ITK_WRAP_NNIIF(F2);
ITK_WRAP_NNIIF(F3);
ITK_WRAP_NNIIF(US2);
ITK_WRAP_NNIIF(US3);

#endif
