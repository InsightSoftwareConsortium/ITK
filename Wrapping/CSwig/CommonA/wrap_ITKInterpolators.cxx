/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKInterpolators.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImage.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkBSplineInterpolateImageFunction.h"
#include "itkBSplineResampleImageFunction.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(ITKInterpolators);
  namespace wrappers
  {
    // wrap InterpolateImageFunction and two super classes up
    ITK_WRAP_OBJECT2(InterpolateImageFunction, image::F2,
                                       double, 
                                       itkInterpolateImageFunctionF2D);
    ITK_WRAP_OBJECT2(InterpolateImageFunction, image::F3, 
                                       double, 
                                       itkInterpolateImageFunctionF3D);
    ITK_WRAP_OBJECT2(InterpolateImageFunction, image::US2, 
                                       double, 
                                       itkInterpolateImageFunctionUS2D);
    ITK_WRAP_OBJECT2(InterpolateImageFunction, image::US3,
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
    
    // wrap BSplineInterpolateImageFunction 
    ITK_WRAP_OBJECT2(BSplineInterpolateImageFunction, image::F2,
                     double, 
                     itkBSplineInterpolateImageFunctionF2D);
    ITK_WRAP_OBJECT2(BSplineInterpolateImageFunction, image::F3, 
                     double, 
                     itkBSplineInterpolateImageFunctionF3D);
    ITK_WRAP_OBJECT2(BSplineInterpolateImageFunction, image::US2,
                     double, 
                     itkBSplineInterpolateImageFunctionUS2D);
    ITK_WRAP_OBJECT2(BSplineInterpolateImageFunction, image::US3,
                     double, 
                     itkBSplineInterpolateImageFunctionUS3D);

    ITK_WRAP_OBJECT3(BSplineInterpolateImageFunction, image::F2,
                     double,float, 
                     itkBSplineInterpolateImageFunctionF2DF);
    ITK_WRAP_OBJECT3(BSplineInterpolateImageFunction, image::F3, 
                     double,float, 
                     itkBSplineInterpolateImageFunctionF3DF);
    ITK_WRAP_OBJECT3(BSplineInterpolateImageFunction, image::US2,
                     double,unsigned short, 
                     itkBSplineInterpolateImageFunctionUS2DUS);
    ITK_WRAP_OBJECT3(BSplineInterpolateImageFunction, image::US3,
                     double,unsigned short, 
                     itkBSplineInterpolateImageFunctionUS3DUS);


    // wrap BSplineResampleImageFunction 
    ITK_WRAP_OBJECT2(BSplineResampleImageFunction, image::F2,
                     double, 
                     itkBSplineResampleImageFunctionF2D);
    ITK_WRAP_OBJECT2(BSplineResampleImageFunction, image::F3, 
                     double, 
                     itkBSplineResampleImageFunctionF3D);
    ITK_WRAP_OBJECT2(BSplineResampleImageFunction, image::US2,
                     double, 
                     itkBSplineResampleImageFunctionUS2D);
    ITK_WRAP_OBJECT2(BSplineResampleImageFunction, image::US3,
                     double, 
                     itkBSplineResampleImageFunctionUS3D);



  }
}

#endif
