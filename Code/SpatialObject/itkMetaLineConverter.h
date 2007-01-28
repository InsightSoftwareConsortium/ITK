/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaLineConverter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMetaLineConverter_h
#define __itkMetaLineConverter_h


#include "metaLine.h"
#include "itkLineSpatialObject.h"
#include "itkSpatialObject.h"

namespace itk 
{

template <unsigned int NDimensions = 3>
class MetaLineConverter
{

public:

  MetaLineConverter();
  ~MetaLineConverter() {};

  typedef itk::LineSpatialObject<NDimensions> SpatialObjectType;

  typedef typename SpatialObjectType::TransformType TransformType;

  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;

  SpatialObjectPointer ReadMeta(const char* name);

  bool WriteMeta(SpatialObjectType* spatialObject,const char* name);

  SpatialObjectPointer MetaLineToLineSpatialObject(MetaLine * Line);
  MetaLine* LineSpatialObjectToMetaLine(SpatialObjectType * spatialObject);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMetaLineConverter.txx"
#endif


#endif
