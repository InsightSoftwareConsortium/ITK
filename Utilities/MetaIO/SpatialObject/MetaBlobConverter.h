/*=========================================================================

  Program:   itkUNC
  Module:    MetaBlobConverter.h
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
#ifndef __MetaBlobConverter__h
#define __MetaBlobConverter__h


#include "metaBlob.h"
#include "itkBlobSpatialObject.h"
#include "itkNDimensionalSpatialObject.h"

template <unsigned int NDimensions = 3>
class MetaBlobConverter
{

public:

  MetaBlobConverter();
  ~MetaBlobConverter() {};

  typedef itk::BlobSpatialObject<NDimensions> SpatialObjectType;


  typedef typename SpatialObjectType::TransformType TransformType;

  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;

  //typedef typename itk::NDimensionalSpatialObject NDimSpatialObject;

  SpatialObjectPointer ReadMeta(const char* name);

  bool WriteMeta(SpatialObjectType* spatialObject,const char* name);

  SpatialObjectPointer MetaBlobToBlobSpatialObject(MetaBlob * Blob);
  MetaBlob* BlobSpatialObjectToMetaBlob(SpatialObjectType * spatialObject);

};


#ifndef ITK_MANUAL_INSTANTIATION
  #include "MetaBlobConverter.txx"
#endif


#endif
