/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaSurfaceConverter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMetaSurfaceConverter_h
#define __itkMetaSurfaceConverter_h


#include "metaSurface.h"
#include "itkSurfaceSpatialObject.h"
#include "itkSpatialObject.h"

namespace itk 
{

template <unsigned int NDimensions = 3>
class MetaSurfaceConverter
{

public:

  MetaSurfaceConverter();
  ~MetaSurfaceConverter() {};

  typedef itk::SurfaceSpatialObject<NDimensions> SpatialObjectType;


  typedef typename SpatialObjectType::TransformType TransformType;

  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;

  //typedef typename itk::NDimensionalSpatialObject NDimSpatialObject;

  SpatialObjectPointer ReadMeta(const char* name);

  bool WriteMeta(SpatialObjectType* spatialObject,const char* name);

  SpatialObjectPointer MetaSurfaceToSurfaceSpatialObject(MetaSurface * Surface);
  MetaSurface* SurfaceSpatialObjectToMetaSurface(
                                             SpatialObjectType * spatialObject);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMetaSurfaceConverter.txx"
#endif


#endif
