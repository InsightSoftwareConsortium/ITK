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
#ifndef itkMetaArrowConverter_hxx
#define itkMetaArrowConverter_hxx

#include "itkMetaArrowConverter.h"

namespace itk
{

template <unsigned int NDimensions>
typename MetaArrowConverter<NDimensions>::MetaObjectType *
MetaArrowConverter<NDimensions>::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new ArrowMetaObjectType);
}

/** Convert a metaArrow into an arrow SpatialObject  */
template <unsigned int NDimensions>
typename MetaArrowConverter<NDimensions>::SpatialObjectPointer
MetaArrowConverter<NDimensions>::MetaObjectToSpatialObject(const MetaObjectType * mo)
{
  const auto * metaArrow = dynamic_cast<const MetaArrow *>(mo);
  if (metaArrow == nullptr)
  {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaArrow");
  }
  ArrowSpatialObjectPointer arrowSO = ArrowSpatialObjectType::New();

  float lengthInObjectSpace = metaArrow->Length();
  arrowSO->SetLengthInObjectSpace(lengthInObjectSpace);

  const double *                         metaPosition = metaArrow->Position();
  const double *                         metaDirection = metaArrow->Direction();
  typename SpatialObjectType::PointType  positionInObjectSpace;
  typename SpatialObjectType::VectorType directionInObjectSpace;
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    positionInObjectSpace[i] = metaPosition[i];
    directionInObjectSpace[i] = metaDirection[i];
  }
  arrowSO->SetPositionInObjectSpace(positionInObjectSpace);
  arrowSO->SetDirectionInObjectSpace(directionInObjectSpace);

  // convert the other fields
  arrowSO->GetProperty().SetName(metaArrow->Name());
  arrowSO->SetId(metaArrow->ID());
  arrowSO->SetParentId(metaArrow->ParentID());
  arrowSO->GetProperty().SetRed(metaArrow->Color()[0]);
  arrowSO->GetProperty().SetGreen(metaArrow->Color()[1]);
  arrowSO->GetProperty().SetBlue(metaArrow->Color()[2]);
  arrowSO->GetProperty().SetAlpha(metaArrow->Color()[3]);
  arrowSO->Update();

  return arrowSO.GetPointer();
}

/** Convert an arrow SpatialObject into a metaArrow */
template <unsigned int NDimensions>
typename MetaArrowConverter<NDimensions>::MetaObjectType *
MetaArrowConverter<NDimensions>::SpatialObjectToMetaObject(const SpatialObjectType * spatialObject)
{
  ArrowSpatialObjectConstPointer arrowSO = dynamic_cast<const ArrowSpatialObjectType *>(spatialObject);
  if (arrowSO.IsNull())
  {
    itkExceptionMacro(<< "Can't downcast SpatialObject to ArrowSpatialObject");
  }

  auto * mo = new MetaArrow(NDimensions);

  float metaLength = arrowSO->GetLengthInObjectSpace();

  if (arrowSO->GetParent())
  {
    mo->ParentID(arrowSO->GetParent()->GetId());
  }

  // convert position and direction
  double                                 metaPosition[NDimensions];
  double                                 metaDirection[NDimensions];
  typename SpatialObjectType::PointType  spPositionInObjectSpace = arrowSO->GetPositionInObjectSpace();
  typename SpatialObjectType::VectorType spDirectionInObjectSpace = arrowSO->GetDirectionInObjectSpace();
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    metaPosition[i] = spPositionInObjectSpace[i];
    metaDirection[i] = spDirectionInObjectSpace[i];
  }
  mo->Position(metaPosition);
  mo->Direction(metaDirection);

  // convert the rest of the parameters
  mo->Length(metaLength);
  mo->ID(arrowSO->GetId());

  mo->Color(arrowSO->GetProperty().GetRed(),
            arrowSO->GetProperty().GetGreen(),
            arrowSO->GetProperty().GetBlue(),
            arrowSO->GetProperty().GetAlpha());

  return mo;
}

} // end namespace itk

#endif
