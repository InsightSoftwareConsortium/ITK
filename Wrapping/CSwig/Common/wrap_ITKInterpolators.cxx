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
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

// Create a macro that will wrap 2 super classes up
// and the class
#define ITK_WRAP_OBJECT2_WITH_SUPERCLASS_2(name, arg1, arg2, wrapname) \
ITK_WRAP_OBJECT2_WITH_SUPERCLASS(name, arg1, arg2, wrapname); \
typedef itk::name<arg1,arg2 >::Superclass::Superclass::Self wrapname##_Superclass2

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(ITKInterpolators);
  namespace wrappers
  {
    // wrap InterpolateImageFunction and two super classes up
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS_2(InterpolateImageFunction, image::F2,
                                       double, 
                                       itkInterpolateImageFunctionF2D);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS_2(InterpolateImageFunction, image::F3, 
                                       double, 
                                       itkInterpolateImageFunctionF3D);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS_2(InterpolateImageFunction, image::US2, 
                                       double, 
                                       itkInterpolateImageFunctionUS2D);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS_2(InterpolateImageFunction, image::US3,
                                       double, 
                                       itkInterpolateImageFunctionUS3D);
    
    // wrap LinearInterpolateImageFunction
    ITK_WRAP_OBJECT2(LinearInterpolateImageFunction, image::F2, double, 
                     itkLinearInterpolateImageFunctionF2D);
    ITK_WRAP_OBJECT2(LinearInterpolateImageFunction, image::F3, double, 
                     itkLinearInterpolateImageFunctionF3D);
    ITK_WRAP_OBJECT2(LinearInterpolateImageFunction, image::US2, double, 
                     itkLinearInterpolateImageFunctionUS2D);
    ITK_WRAP_OBJECT2(LinearInterpolateImageFunction, image::US3, double, 
                     itkLinearInterpolateImageFunctionUS3D);

    // wrap NearestNeighborInterpolateImageFunction 
    ITK_WRAP_OBJECT2(NearestNeighborInterpolateImageFunction, image::F2,
                     double, 
                     itkNearestNeighborInterpolateImageFunctionF2D);
    ITK_WRAP_OBJECT2(NearestNeighborInterpolateImageFunction, image::F3, 
                     double, 
                     itkNearestNeighborInterpolateImageFunctionF3D);
    ITK_WRAP_OBJECT2(NearestNeighborInterpolateImageFunction, image::US2,
                     double, 
                     itkNearestNeighborInterpolateImageFunctionUS2D);
    ITK_WRAP_OBJECT2(NearestNeighborInterpolateImageFunction, image::US3,
                     double, 
                     itkNearestNeighborInterpolateImageFunctionUS3D);

  }
}

#endif
