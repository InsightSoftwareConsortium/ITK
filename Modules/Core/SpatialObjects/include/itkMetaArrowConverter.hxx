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
#ifndef itkMetaArrowConverter_hxx
#define itkMetaArrowConverter_hxx

#include "itkMetaArrowConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaArrowConverter< NDimensions >
::MetaArrowConverter()
{
}

template< unsigned int NDimensions >
typename MetaArrowConverter< NDimensions >::MetaObjectType *
MetaArrowConverter< NDimensions>
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new ArrowMetaObjectType);
}

/** Convert a metaArrow into an arrow SpatialObject  */
template< unsigned int NDimensions >
typename MetaArrowConverter< NDimensions >::SpatialObjectPointer
MetaArrowConverter< NDimensions >
::MetaObjectToSpatialObject( const MetaObjectType *mo )
{
  const ArrowMetaObjectType *metaArrow = dynamic_cast<const MetaArrow *>(mo);
  if(metaArrow == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaArrow");
    }
  ArrowSpatialObjectPointer arrowSO = ArrowSpatialObjectType::New();

  double spacing[NDimensions];
  float  length = metaArrow->Length();

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    spacing[i] = metaArrow->ElementSpacing()[i];
    }

  // convert position and direction/orientation
  const double *metaPosition = metaArrow->Position();
  const double *metaDirection = metaArrow->Direction();
  typename SpatialObjectType::PointType position;
  typename SpatialObjectType::VectorType direction;
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    position[i] = metaPosition[i];
    direction[i] = metaDirection[i];
    }
  arrowSO->SetPosition(position);
  arrowSO->SetDirection(direction);

  // convert the other fields
  arrowSO->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  arrowSO->SetLength(length);
  arrowSO->GetProperty()->SetName( metaArrow->Name() );
  arrowSO->SetId( metaArrow->ID() );
  arrowSO->SetParentId( metaArrow->ParentID() );
  arrowSO->GetProperty()->SetRed(metaArrow->Color()[0]);
  arrowSO->GetProperty()->SetGreen(metaArrow->Color()[1]);
  arrowSO->GetProperty()->SetBlue(metaArrow->Color()[2]);
  arrowSO->GetProperty()->SetAlpha(metaArrow->Color()[3]);

  return arrowSO.GetPointer();
}

/** Convert an arrow SpatialObject into a metaArrow */
template< unsigned int NDimensions >
typename MetaArrowConverter<NDimensions>::MetaObjectType *
MetaArrowConverter< NDimensions >
::SpatialObjectToMetaObject(const SpatialObjectType *spatialObject)
{
  ArrowSpatialObjectConstPointer arrowSO =
    dynamic_cast<const ArrowSpatialObjectType *>(spatialObject);
  if(arrowSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to ArrowSpatialObject");
    }

  ArrowMetaObjectType *mo = new MetaArrow(NDimensions);

  float length = arrowSO->GetLength();

  if ( arrowSO->GetParent() )
    {
    mo->ParentID( arrowSO->GetParent()->GetId() );
    }

  // convert position and direction
  double position[NDimensions];
  double direction[NDimensions];
  typename SpatialObjectType::PointType spPosition = arrowSO->GetPosition();
  typename SpatialObjectType::VectorType spDirection = arrowSO->GetDirection();
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    position[i] = spPosition[i];
    direction[i] = spDirection[i];
    }
  mo->Position(position);
  mo->Direction(direction);

  // convert the rest of the parameters
  mo->Length(length);
  mo->ID( arrowSO->GetId() );

  mo->Color( arrowSO->GetProperty()->GetRed(),
                arrowSO->GetProperty()->GetGreen(),
                arrowSO->GetProperty()->GetBlue(),
                arrowSO->GetProperty()->GetAlpha() );

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    mo->ElementSpacing(i, arrowSO->GetIndexToObjectTransform()->GetScaleComponent()[i]);
    }

  return mo;
}

} // end namespace itk

#endif
