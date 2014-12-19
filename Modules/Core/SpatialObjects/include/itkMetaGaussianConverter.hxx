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
#ifndef itkMetaGaussianConverter_hxx
#define itkMetaGaussianConverter_hxx

#include "itkMetaGaussianConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaGaussianConverter< NDimensions >
::MetaGaussianConverter()
{}

template< unsigned int NDimensions >
typename MetaGaussianConverter< NDimensions >::MetaObjectType *
MetaGaussianConverter< NDimensions>
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new GaussianMetaObjectType);
}

/** Convert a metaGaussian into a gaussian SpatialObject  */
template< unsigned int NDimensions >
typename MetaGaussianConverter< NDimensions >::SpatialObjectPointer
MetaGaussianConverter< NDimensions >
::MetaObjectToSpatialObject(const MetaObjectType *mo)
{
  const GaussianMetaObjectType *gaussian =
    dynamic_cast<const GaussianMetaObjectType *>(mo);
  if(gaussian == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaGaussian" );
    }

  GaussianSpatialObjectPointer gaussianSO = GaussianSpatialObjectType::New();

  gaussianSO->SetMaximum( gaussian->Maximum() );
  gaussianSO->SetRadius( gaussian->Radius() );
  gaussianSO->GetProperty()->SetName( gaussian->Name() );
  gaussianSO->SetId( gaussian->ID() );
  gaussianSO->SetParentId( gaussian->ParentID() );
  return gaussianSO.GetPointer();
}

/** Convert a gaussian SpatialObject into a metaGaussian */
template< unsigned int NDimensions >
typename MetaGaussianConverter< NDimensions >::MetaObjectType *
MetaGaussianConverter< NDimensions >
::SpatialObjectToMetaObject(const SpatialObjectType *so)
{
  GaussianSpatialObjectConstPointer gaussianSO =
    dynamic_cast<const GaussianSpatialObjectType *>(so);
  GaussianMetaObjectType *gaussian = new GaussianMetaObjectType;
  if(gaussianSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to GaussianSpatialObject");
    }

  if ( gaussianSO->GetParent() )
    {
    gaussian->ParentID( gaussianSO->GetParent()->GetId() );
    }
  gaussian->Maximum( gaussianSO->GetMaximum() );
  gaussian->Radius( gaussianSO->GetRadius() );
  gaussian->ID( gaussianSO->GetId() );
  gaussian->BinaryData(true);
  return gaussian;
}

} // end namespace itk

#endif
