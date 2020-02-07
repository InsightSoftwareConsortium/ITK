/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkMetaEllipseConverter_hxx
#define itkMetaEllipseConverter_hxx

#include "itkMetaEllipseConverter.h"

namespace itk
{

template <unsigned int NDimensions>
typename MetaEllipseConverter<NDimensions>::MetaObjectType *
MetaEllipseConverter<NDimensions>::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new EllipseMetaObjectType);
}

/** Convert a metaEllipse into an ellipse SpatialObject  */
template <unsigned int NDimensions>
typename MetaEllipseConverter<NDimensions>::SpatialObjectPointer
MetaEllipseConverter<NDimensions>::MetaObjectToSpatialObject(const MetaObjectType * mo)
{
  const auto * ellipseMO = dynamic_cast<const EllipseMetaObjectType *>(mo);
  if (ellipseMO == nullptr)
  {
    itkExceptionMacro(<< "Can't downcast MetaObject to EllipseMetaObject");
  }

  EllipseSpatialObjectPointer ellipseSO = EllipseSpatialObjectType::New();

  typename EllipseSpatialObjectType::ArrayType radii;
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    radii[i] = ellipseMO->Radius()[i];
  }

  ellipseSO->SetRadiusInObjectSpace(radii);
  ellipseSO->GetProperty().SetName(ellipseMO->Name());
  ellipseSO->SetId(ellipseMO->ID());
  ellipseSO->SetParentId(ellipseMO->ParentID());
  ellipseSO->GetProperty().SetRed(ellipseMO->Color()[0]);
  ellipseSO->GetProperty().SetGreen(ellipseMO->Color()[1]);
  ellipseSO->GetProperty().SetBlue(ellipseMO->Color()[2]);
  ellipseSO->GetProperty().SetAlpha(ellipseMO->Color()[3]);

  return ellipseSO.GetPointer();
}

/** Convert an ellipse SpatialObject into a metaEllipse */
template <unsigned int NDimensions>
typename MetaEllipseConverter<NDimensions>::MetaObjectType *
MetaEllipseConverter<NDimensions>::SpatialObjectToMetaObject(const SpatialObjectType * so)
{
  EllipseSpatialObjectConstPointer ellipseSO = dynamic_cast<const EllipseSpatialObjectType *>(so);
  if (ellipseSO.IsNull())
  {
    itkExceptionMacro(<< "Can't downcast SpatialObject to EllipseSpatialObject");
  }

  auto * ellipseMO = new EllipseMetaObjectType(NDimensions);

  auto * radii = new float[NDimensions];

  for (unsigned int i = 0; i < NDimensions; i++)
  {
    radii[i] = ellipseSO->GetRadiusInObjectSpace()[i];
  }

  if (ellipseSO->GetParent())
  {
    ellipseMO->ParentID(ellipseSO->GetParent()->GetId());
  }
  ellipseMO->Radius(radii);
  ellipseMO->ID(ellipseSO->GetId());

  ellipseMO->Color(ellipseSO->GetProperty().GetRed(),
                   ellipseSO->GetProperty().GetGreen(),
                   ellipseSO->GetProperty().GetBlue(),
                   ellipseSO->GetProperty().GetAlpha());

  delete[] radii;
  return ellipseMO;
}

} // end namespace itk

#endif
