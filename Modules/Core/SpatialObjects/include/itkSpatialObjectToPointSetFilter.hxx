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
#ifndef itkSpatialObjectToPointSetFilter_hxx
#define itkSpatialObjectToPointSetFilter_hxx

#include "itkSpatialObjectToPointSetFilter.h"

namespace itk
{

template< typename TInputSpatialObject, typename TOutputPointSet >
SpatialObjectToPointSetFilter< TInputSpatialObject, TOutputPointSet >
::SpatialObjectToPointSetFilter()

{
  this->SetNumberOfRequiredInputs(1);
}

template< typename TInputSpatialObject, typename TOutputPointSet >
SpatialObjectToPointSetFilter< TInputSpatialObject, TOutputPointSet >
::~SpatialObjectToPointSetFilter() = default;

template< typename TInputSpatialObject, typename TOutputPointSet >
void
SpatialObjectToPointSetFilter< TInputSpatialObject, TOutputPointSet >
::SetInput(const InputSpatialObjectType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
    const_cast< InputSpatialObjectType * >( input ) );
}

template< typename TInputSpatialObject, typename TOutputPointSet >
void
SpatialObjectToPointSetFilter< TInputSpatialObject, TOutputPointSet >
::SetInput(unsigned int index, const TInputSpatialObject *object)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( index,
    const_cast< TInputSpatialObject * >( object ) );
}

template< typename TInputSpatialObject, typename TOutputPointSet >
const typename SpatialObjectToPointSetFilter< TInputSpatialObject, TOutputPointSet >::InputSpatialObjectType *
SpatialObjectToPointSetFilter< TInputSpatialObject, TOutputPointSet >
::GetInput()
{
  return static_cast< const TInputSpatialObject * >( this->GetPrimaryInput() );
}

template< typename TInputSpatialObject, typename TOutputPointSet >
const typename SpatialObjectToPointSetFilter< TInputSpatialObject, TOutputPointSet >::InputSpatialObjectType *
SpatialObjectToPointSetFilter< TInputSpatialObject, TOutputPointSet >
::GetInput(unsigned int idx)
{
  return static_cast< const TInputSpatialObject * >(
    this->ProcessObject::GetInput(idx) );
}

template< typename TInputSpatialObject, typename TOutputPointSet >
void
SpatialObjectToPointSetFilter< TInputSpatialObject, TOutputPointSet >
::GenerateData()
{
  // Get the input and output pointers
  const InputSpatialObjectType *inputObject  = this->GetInput();
  typename OutputPointSetType::Pointer outputPointSet = this->GetOutput();

  using PointIdentifier = typename OutputPointSetType::PointIdentifier;

  const auto * inputPointSO = dynamic_cast<
    const PointBasedSpatialObjectType * >( inputObject );

  // Look for the number of points to allocate
  PointIdentifier numberOfPoints = 0;
  if ( inputPointSO )
    {
    numberOfPoints = inputPointSO->GetNumberOfPoints() / m_SamplingFactor;
    }

  ChildrenListType *children = inputObject->GetChildren(m_ChildrenDepth);
  typename ChildrenListType::const_iterator it = children->begin();

  for (; it != children->end(); it++ )
    {
    const auto * pointSO = dynamic_cast< const PointBasedSpatialObjectType * >(
      it->GetPointer() );
    if ( pointSO )
      {
      numberOfPoints += pointSO->GetNumberOfPoints() / m_SamplingFactor;
      }
    }

  using DataContainer = typename OutputPointSetType::PointDataContainer;
  outputPointSet->SetPointData( DataContainer::New() );

  outputPointSet->GetPoints()->Reserve(numberOfPoints);
  outputPointSet->GetPointData()->Reserve(numberOfPoints);

  PointIdentifier pointId = 0;
  typename OutputPointSetType::PointType point;

  // add the object it itself
  PointIdentifier n;
  if ( inputPointSO )
    {
    n = inputPointSO->GetNumberOfPoints();
    for ( unsigned int i = 0; i < n; i += m_SamplingFactor )
      {
      typename InputSpatialObjectType::PointType transformedPoint =
        inputPointSO->GetPoint(i)->GetPositionInWorldSpace();

      for ( unsigned int j = 0; j < Self::ObjectDimension; j++ )
        {
        point[j] = transformedPoint[j];
        }
      outputPointSet->SetPoint(pointId++, point);
      }
    }

  // then add children points
  it = children->begin();

  for (; it != children->end(); it++ )
    {
    const auto * pointSO = dynamic_cast< const PointBasedSpatialObjectType * >(
      it->GetPointer() );
    if ( pointSO )
      {
      n = pointSO->GetNumberOfPoints();
      for ( unsigned int i = 0; i < n; i += m_SamplingFactor )
        {
        typename InputSpatialObjectType::PointType transformedPoint =
          pointSO->GetPoint(i)->GetPositionInWorldSpace();

        for ( unsigned int j = 0; j < Self::ObjectDimension; j++ )
          {
          point[j] = transformedPoint[j];
          }
        outputPointSet->SetPoint(pointId++, point);
        }
      }
    }

  delete children;
}

template< typename TInputSpatialObject, typename TOutputPointSet >
void
SpatialObjectToPointSetFilter< TInputSpatialObject, TOutputPointSet >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Children depth : " << m_ChildrenDepth << std::endl;
  os << indent << "Sampling Factor : " << m_SamplingFactor << std::endl;
}
} // end namespace itk

#endif
