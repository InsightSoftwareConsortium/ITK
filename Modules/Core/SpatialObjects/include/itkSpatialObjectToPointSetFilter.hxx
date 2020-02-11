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
#ifndef itkSpatialObjectToPointSetFilter_hxx
#define itkSpatialObjectToPointSetFilter_hxx

#include "itkSpatialObjectToPointSetFilter.h"

namespace itk
{

template <typename TPointBasedSpatialObject, typename TOutputPointSet>
SpatialObjectToPointSetFilter<TPointBasedSpatialObject, TOutputPointSet>::SpatialObjectToPointSetFilter()

{
  this->SetNumberOfRequiredInputs(1);
}

template <typename TPointBasedSpatialObject, typename TOutputPointSet>
void
SpatialObjectToPointSetFilter<TPointBasedSpatialObject, TOutputPointSet>::SetInput(const SpatialObjectType * input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0, const_cast<SpatialObjectType *>(input));
}

template <typename TPointBasedSpatialObject, typename TOutputPointSet>
void
SpatialObjectToPointSetFilter<TPointBasedSpatialObject, TOutputPointSet>::SetInput(const DataObjectIdentifierType & key,
                                                                                   const SpatialObjectType * object)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(key, const_cast<SpatialObjectType *>(object));
}

template <typename TPointBasedSpatialObject, typename TOutputPointSet>
const typename SpatialObjectToPointSetFilter<TPointBasedSpatialObject, TOutputPointSet>::SpatialObjectType *
SpatialObjectToPointSetFilter<TPointBasedSpatialObject, TOutputPointSet>::GetInput()
{
  return static_cast<const SpatialObjectType *>(this->GetPrimaryInput());
}

template <typename TPointBasedSpatialObject, typename TOutputPointSet>
const typename SpatialObjectToPointSetFilter<TPointBasedSpatialObject, TOutputPointSet>::SpatialObjectType *
SpatialObjectToPointSetFilter<TPointBasedSpatialObject, TOutputPointSet>::GetInput(unsigned int idx)
{
  return static_cast<const SpatialObjectType *>(this->ProcessObject::GetInput(idx));
}

template <typename TPointBasedSpatialObject, typename TOutputPointSet>
void
SpatialObjectToPointSetFilter<TPointBasedSpatialObject, TOutputPointSet>::GenerateData()
{
  // Get the input and output pointers
  const SpatialObjectType *            inputObject = this->GetInput();
  typename OutputPointSetType::Pointer outputPointSet = this->GetOutput();

  using PointIdentifier = typename OutputPointSetType::PointIdentifier;

  // Look for the number of points to allocate
  PointIdentifier numberOfPoints = 0;
  const auto *    inputSO = dynamic_cast<const TPointBasedSpatialObject *>(inputObject);
  if (inputSO)
  {
    numberOfPoints = inputSO->GetNumberOfPoints() / m_SamplingFactor;
  }

  ChildrenListType *                        children = inputObject->GetChildren(m_ChildrenDepth);
  typename ChildrenListType::const_iterator it = children->begin();

  while (it != children->end())
  {
    const auto * pointSO = dynamic_cast<const TPointBasedSpatialObject *>(it->GetPointer());
    if (pointSO)
    {
      numberOfPoints += pointSO->GetNumberOfPoints() / m_SamplingFactor;
    }
    ++it;
  }

  using DataContainer = typename OutputPointSetType::PointDataContainer;
  outputPointSet->SetPointData(DataContainer::New());

  outputPointSet->GetPoints()->Reserve(numberOfPoints);
  outputPointSet->GetPointData()->Reserve(numberOfPoints);

  PointIdentifier                        pointId = 0;
  typename OutputPointSetType::PointType point;

  // add the object it itself
  PointIdentifier n;
  if (inputSO)
  {
    n = inputSO->GetNumberOfPoints();
    for (unsigned int i = 0; i < n; i += m_SamplingFactor)
    {
      typename PointBasedSpatialObjectType::PointType transformedPoint =
        inputSO->GetPoint(i)->GetPositionInWorldSpace();

      for (unsigned int j = 0; j < Self::ObjectDimension; j++)
      {
        point[j] = transformedPoint[j];
      }
      outputPointSet->SetPoint(pointId++, point);
    }
  }

  // then add children points
  it = children->begin();
  while (it != children->end())
  {
    const auto * pointSO = dynamic_cast<const TPointBasedSpatialObject *>(it->GetPointer());
    if (pointSO)
    {
      n = pointSO->GetNumberOfPoints();
      for (unsigned int i = 0; i < n; i += m_SamplingFactor)
      {
        typename PointBasedSpatialObjectType::PointType transformedPoint =
          pointSO->GetPoint(i)->GetPositionInWorldSpace();

        for (unsigned int j = 0; j < Self::ObjectDimension; j++)
        {
          point[j] = transformedPoint[j];
        }
        outputPointSet->SetPoint(pointId++, point);
      }
    }
    ++it;
  }

  delete children;
}

template <typename TPointBasedSpatialObject, typename TOutputPointSet>
void
SpatialObjectToPointSetFilter<TPointBasedSpatialObject, TOutputPointSet>::PrintSelf(std::ostream & os,
                                                                                    Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Children depth : " << m_ChildrenDepth << std::endl;
  os << indent << "Sampling Factor : " << m_SamplingFactor << std::endl;
}
} // end namespace itk

#endif
