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
#ifndef __itkMetaLandmarkConverter_h
#define __itkMetaLandmarkConverter_h

#include "metaLandmark.h"
#include "itkLandmarkSpatialObject.h"
#include "itkSpatialObject.h"

namespace itk
{
template< unsigned int NDimensions = 3 >
class ITK_EXPORT MetaLandmarkConverter
{
public:

  MetaLandmarkConverter();
  ~MetaLandmarkConverter() {}

  typedef itk::LandmarkSpatialObject< NDimensions > SpatialObjectType;

  typedef typename SpatialObjectType::TransformType TransformType;

  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;

  //typedef typename itk::NDimensionalSpatialObject NDimSpatialObject;

  SpatialObjectPointer ReadMeta(const char *name);

  bool WriteMeta(SpatialObjectType *spatialObject, const char *name);

  SpatialObjectPointer MetaLandmarkToLandmarkSpatialObject(
    MetaLandmark *landmark);

  MetaLandmark * LandmarkSpatialObjectToMetaLandmark(
    SpatialObjectType *spatialObject);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMetaLandmarkConverter.txx"
#endif

#endif
