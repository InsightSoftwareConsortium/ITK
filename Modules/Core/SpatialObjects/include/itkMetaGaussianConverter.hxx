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
  const auto * metaGaussian = dynamic_cast<const GaussianMetaObjectType *>(mo);
  if(metaGaussian == nullptr)
    {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaGaussian" );
    }

  GaussianSpatialObjectPointer gaussianSO = GaussianSpatialObjectType::New();

  gaussianSO->SetMaximum(metaGaussian->Maximum());
  gaussianSO->SetRadiusInObjectSpace(metaGaussian->Radius());
  gaussianSO->SetSigmaInObjectSpace(metaGaussian->Sigma());
  gaussianSO->GetProperty().SetName( metaGaussian->Name());
  gaussianSO->SetId(metaGaussian->ID());
  gaussianSO->SetParentId( metaGaussian->ParentID() );
  gaussianSO->GetProperty().SetRed(metaGaussian->Color()[0]);
  gaussianSO->GetProperty().SetGreen(metaGaussian->Color()[1]);
  gaussianSO->GetProperty().SetBlue(metaGaussian->Color()[2]);
  gaussianSO->GetProperty().SetAlpha(metaGaussian->Color()[3]);

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
  auto * metaGaussian = new GaussianMetaObjectType;
  if(gaussianSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to GaussianSpatialObject");
    }

  if ( gaussianSO->GetParent() )
    {
    metaGaussian->ParentID( gaussianSO->GetParent()->GetId() );
    }
  metaGaussian->Maximum(gaussianSO->GetMaximum());
  metaGaussian->Radius(gaussianSO->GetRadiusInObjectSpace());
  metaGaussian->Sigma(gaussianSO->GetSigmaInObjectSpace());
  metaGaussian->ID(gaussianSO->GetId());
  metaGaussian->BinaryData(true);
  metaGaussian->Color(gaussianSO->GetProperty().GetRed(),
    gaussianSO->GetProperty().GetGreen(),
    gaussianSO->GetProperty().GetBlue(),
    gaussianSO->GetProperty().GetAlpha());

  return metaGaussian;
}

} // end namespace itk

#endif
