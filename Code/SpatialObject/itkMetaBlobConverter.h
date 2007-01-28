/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaBlobConverter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMetaBlobConverter_h
#define __itkMetaBlobConverter_h


#include "metaBlob.h"
#include "itkBlobSpatialObject.h"
#include "itkSpatialObject.h"

namespace itk 
{

template <unsigned int NDimensions = 3>
class MetaBlobConverter
{

public:

  MetaBlobConverter();
  ~MetaBlobConverter() {};

  typedef itk::BlobSpatialObject<NDimensions>       SpatialObjectType;
  typedef typename SpatialObjectType::TransformType TransformType;
  typedef typename SpatialObjectType::Pointer       SpatialObjectPointer;

  SpatialObjectPointer ReadMeta(const char* name);

  bool WriteMeta(SpatialObjectType* spatialObject,const char* name);

  SpatialObjectPointer MetaBlobToBlobSpatialObject(MetaBlob * Blob);
  MetaBlob* BlobSpatialObjectToMetaBlob(SpatialObjectType * spatialObject);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMetaBlobConverter.txx"
#endif


#endif
