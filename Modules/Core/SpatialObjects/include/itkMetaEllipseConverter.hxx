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
#ifndef itkMetaEllipseConverter_hxx
#define itkMetaEllipseConverter_hxx

#include "itkMetaEllipseConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaEllipseConverter< NDimensions >
::MetaEllipseConverter()
{}

template< unsigned int NDimensions >
typename MetaEllipseConverter< NDimensions >::MetaObjectType *
MetaEllipseConverter< NDimensions>
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new EllipseMetaObjectType);
}

/** Convert a metaEllipse into an ellipse SpatialObject  */
template< unsigned int NDimensions >
typename MetaEllipseConverter< NDimensions >::SpatialObjectPointer
MetaEllipseConverter< NDimensions >
::MetaObjectToSpatialObject(const MetaObjectType *mo)
{
  const EllipseMetaObjectType *ellipseMO = dynamic_cast<const EllipseMetaObjectType *>(mo);
  if(ellipseMO == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't downcast MetaObject to EllipseMetaObject");
    }

  EllipseSpatialObjectPointer ellipseSO = EllipseSpatialObjectType::New();

  double spacing[NDimensions];

  typename EllipseSpatialObjectType::ArrayType radius;

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    radius[i] = ellipseMO->Radius()[i];
    spacing[i] = ellipseMO->ElementSpacing()[i];
    }

  ellipseSO->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  ellipseSO->SetRadius(radius);
  ellipseSO->GetProperty()->SetName( ellipseMO->Name() );
  ellipseSO->SetId( ellipseMO->ID() );
  ellipseSO->SetParentId( ellipseMO->ParentID() );
  ellipseSO->GetProperty()->SetRed(ellipseMO->Color()[0]);
  ellipseSO->GetProperty()->SetGreen(ellipseMO->Color()[1]);
  ellipseSO->GetProperty()->SetBlue(ellipseMO->Color()[2]);
  ellipseSO->GetProperty()->SetAlpha(ellipseMO->Color()[3]);

  return ellipseSO.GetPointer();
}

/** Convert an ellipse SpatialObject into a metaEllipse */
template< unsigned int NDimensions >
typename MetaEllipseConverter< NDimensions>::MetaObjectType *
MetaEllipseConverter< NDimensions >
::SpatialObjectToMetaObject(const SpatialObjectType *so)
{
  EllipseSpatialObjectConstPointer ellipseSO =
    dynamic_cast<const EllipseSpatialObjectType *>(so);
  if(ellipseSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to EllipseSpatialObject");
    }

  EllipseMetaObjectType *ellipseMO = new EllipseMetaObjectType(NDimensions);

  float *radius = new float[NDimensions];

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    radius[i] = ellipseSO->GetRadius()[i];
    }

  if ( ellipseSO->GetParent() )
    {
    ellipseMO->ParentID( ellipseSO->GetParent()->GetId() );
    }
  ellipseMO->Radius(radius);
  ellipseMO->ID( ellipseSO->GetId() );

  ellipseMO->Color( ellipseSO->GetProperty()->GetRed(),
                  ellipseSO->GetProperty()->GetGreen(),
                  ellipseSO->GetProperty()->GetBlue(),
                  ellipseSO->GetProperty()->GetAlpha() );

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    ellipseMO->ElementSpacing(i, ellipseSO->GetIndexToObjectTransform()
                            ->GetScaleComponent()[i]);
    }

  delete[] radius;
  return ellipseMO;
}

} // end namespace itk

#endif
