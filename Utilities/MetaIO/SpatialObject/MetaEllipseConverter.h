/*=========================================================================

  Program:   itkUNC
  Module:    MetaEllipseConverter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
  Author:    Julien Jomier (julien@jomier.com)

  Copyright (c) 2002 CADDLab @ UNC. All rights reserved.
  See itkUNCCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MetaEllipseConverter__h
#define __MetaEllipseConverter__h

#include "itkEllipseSpatialObject.h"
#include "metaEllipse.h"

template <unsigned int NDimensions = 3>
class MetaEllipseConverter
{

public:

  MetaEllipseConverter();
  ~MetaEllipseConverter() {};

  typedef itk::EllipseSpatialObject<NDimensions> SpatialObjectType;
  typedef typename SpatialObjectType::TransformType TransformType;

  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;

  SpatialObjectPointer ReadMeta(const char* name);

  bool WriteMeta(SpatialObjectType* spatialObject,const char* name);

  SpatialObjectPointer MetaEllipseToEllipseSpatialObject(MetaEllipse * ellipse);
  MetaEllipse* EllipseSpatialObjectToMetaEllipse(SpatialObjectType * spatialObject);

};


#ifndef ITK_MANUAL_INSTANTIATION
  #include "MetaEllipseConverter.txx"
#endif


#endif
