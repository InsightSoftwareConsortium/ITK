/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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


namespace itk
{

template <unsigned int VDimension>
auto
MetaArrowConverter<VDimension>::CreateMetaObject() -> MetaObjectType *
{
  return dynamic_cast<MetaObjectType *>(new ArrowMetaObjectType);
}

/** Convert a metaArrow into an arrow SpatialObject  */
template <unsigned int VDimension>
auto
MetaArrowConverter<VDimension>::MetaObjectToSpatialObject(const MetaObjectType * mo) -> SpatialObjectPointer
{
  const auto * metaArrow = dynamic_cast<const MetaArrow *>(mo);
  if (metaArrow == nullptr)
  {
    itkExceptionMacro("Can't convert MetaObject to MetaArrow");
  }
  ArrowSpatialObjectPointer arrowSO = ArrowSpatialObjectType::New();

  this->MetaObjectToSpatialObjectBase(mo, arrowSO);

  float lengthInObjectSpace = metaArrow->Length();
  arrowSO->SetLengthInObjectSpace(lengthInObjectSpace);

  const double *                         metaPosition = metaArrow->Position();
  const double *                         metaDirection = metaArrow->Direction();
  typename SpatialObjectType::PointType  positionInObjectSpace;
  typename SpatialObjectType::VectorType directionInObjectSpace;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    positionInObjectSpace[i] = metaPosition[i];
    directionInObjectSpace[i] = metaDirection[i];
  }
  if (mo->APIVersion() == 1)
  {
    // if not using new API, then the position is already stored as Offset.
    // Really not certain how to handle the old case since it produced a bug...with
    //    position being stored in the Offset and the Position!  But then the WorldPosition
    //    would compose offset with position, which would be wrong...   It really never
    //    worked correctly...
    arrowSO->SetPositionInObjectSpace(positionInObjectSpace);
  }

  arrowSO->SetDirectionInObjectSpace(directionInObjectSpace);

  arrowSO->Update();

  return arrowSO.GetPointer();
}

/** Convert an arrow SpatialObject into a metaArrow */
template <unsigned int VDimension>
auto
MetaArrowConverter<VDimension>::SpatialObjectToMetaObject(const SpatialObjectType * spatialObject) -> MetaObjectType *
{
  ArrowSpatialObjectConstPointer arrowSO = dynamic_cast<const ArrowSpatialObjectType *>(spatialObject);
  if (arrowSO.IsNull())
  {
    itkExceptionMacro("Can't downcast SpatialObject to ArrowSpatialObject");
  }

  auto * mo = new MetaArrow(VDimension);
  mo->APIVersion(1);
  mo->FileFormatVersion(1);

  this->SpatialObjectToMetaObjectBase(spatialObject, mo);

  float metaLength = arrowSO->GetLengthInObjectSpace();
  mo->Length(metaLength);

  // convert position and direction
  double                                 metaPosition[VDimension];
  double                                 metaDirection[VDimension];
  typename SpatialObjectType::PointType  spPositionInObjectSpace = arrowSO->GetPositionInObjectSpace();
  typename SpatialObjectType::VectorType spDirectionInObjectSpace = arrowSO->GetDirectionInObjectSpace();
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    metaPosition[i] = spPositionInObjectSpace[i];
    metaDirection[i] = spDirectionInObjectSpace[i];
  }
  mo->Position(metaPosition);
  mo->Direction(metaDirection);

  return mo;
}

} // end namespace itk

#endif
