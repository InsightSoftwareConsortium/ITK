/*=========================================================================

  Program:   itkUNC
  Module:    MetaImageConverter.h
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
#ifndef __MetaImageConverter__h
#define __MetaImageConverter__h

#include "itkImageSpatialObject.h"
#include "metaImage.h"

template <unsigned int NDimensions = 3, class PixelType = unsigned char>
class MetaImageConverter
{

public:

  MetaImageConverter();
  ~MetaImageConverter() {};

  typedef itk::ImageSpatialObject<NDimensions,PixelType > SpatialObjectType;
  typedef typename SpatialObjectType::TransformType TransformType;

  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;

  SpatialObjectPointer ReadMeta(const char* name);

  bool WriteMeta(SpatialObjectType* spatialObject,const char* name);

  SpatialObjectPointer MetaImageToImageSpatialObject(MetaImage * image);
  MetaImage* ImageSpatialObjectToMetaImage(SpatialObjectType * spatialObject);

};


#ifndef ITK_MANUAL_INSTANTIATION
  #include "MetaImageConverter.txx"
#endif


#endif
