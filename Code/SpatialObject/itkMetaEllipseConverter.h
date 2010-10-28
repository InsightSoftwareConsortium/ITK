/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkMetaEllipseConverter_h
#define __itkMetaEllipseConverter_h

#include "itkEllipseSpatialObject.h"
#include "metaEllipse.h"

namespace itk
{
template< unsigned int NDimensions = 3 >
class ITK_EXPORT MetaEllipseConverter
{
public:

  MetaEllipseConverter();
  ~MetaEllipseConverter() {}

  typedef itk::EllipseSpatialObject< NDimensions >  SpatialObjectType;
  typedef typename SpatialObjectType::TransformType TransformType;

  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;

  SpatialObjectPointer ReadMeta(const char *name);

  bool WriteMeta(SpatialObjectType *spatialObject, const char *name);

  SpatialObjectPointer MetaEllipseToEllipseSpatialObject(MetaEllipse *ellipse);

  MetaEllipse * EllipseSpatialObjectToMetaEllipse(
    SpatialObjectType *spatialObject);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMetaEllipseConverter.txx"
#endif

#endif
