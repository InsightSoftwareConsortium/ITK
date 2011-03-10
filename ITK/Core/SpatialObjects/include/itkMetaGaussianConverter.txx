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
#ifndef __itkMetaGaussianConverter_txx
#define __itkMetaGaussianConverter_txx

#include "itkMetaGaussianConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaGaussianConverter< NDimensions >
::MetaGaussianConverter()
{}

/** Convert a metaGaussian into a gaussian SpatialObject  */
template< unsigned int NDimensions >
typename MetaGaussianConverter< NDimensions >::SpatialObjectPointer
MetaGaussianConverter< NDimensions >
::MetaGaussianToGaussianSpatialObject(MetaGaussian *gaussian)
{
  SpatialObjectPointer spatialObject = SpatialObjectType::New();

  spatialObject->SetMaximum( gaussian->Maximum() );
  spatialObject->SetRadius( gaussian->Radius() );
  spatialObject->GetProperty()->SetName( gaussian->Name() );
  spatialObject->SetId( gaussian->ID() );
  spatialObject->SetParentId( gaussian->ParentID() );
  return spatialObject;
}

/** Convert a gaussian SpatialObject into a metaGaussian */
template< unsigned int NDimensions >
MetaGaussian *
MetaGaussianConverter< NDimensions >
::GaussianSpatialObjectToMetaGaussian(SpatialObjectType *spatialObject)
{
  MetaGaussian *gaussian = new MetaGaussian(NDimensions);

  if ( spatialObject->GetParent() )
    {
    gaussian->ParentID( spatialObject->GetParent()->GetId() );
    }
  gaussian->Maximum( spatialObject->GetMaximum() );
  gaussian->Radius( spatialObject->GetRadius() );
  gaussian->ID( spatialObject->GetId() );
  return gaussian;
}

/** Read a meta file give the type */
template< unsigned int NDimensions >
typename MetaGaussianConverter< NDimensions >::SpatialObjectPointer
MetaGaussianConverter< NDimensions >
::ReadMeta(const char *name)
{
  SpatialObjectPointer spatialObject;
  MetaGaussian *       gaussian = new MetaGaussian();

  gaussian->Read(name);
  spatialObject = MetaGaussianToGaussianSpatialObject(gaussian);

  return spatialObject;
}

/** Write a meta gaussian file */
template< unsigned int NDimensions >
bool
MetaGaussianConverter< NDimensions >
::WriteMeta(SpatialObjectType *spatialObject, const char *name)
{
  MetaGaussian *gaussian = GaussianSpatialObjectToMetaGaussian(spatialObject);

  gaussian->Write(name);
  return true;
}
} // end namespace itk

#endif
