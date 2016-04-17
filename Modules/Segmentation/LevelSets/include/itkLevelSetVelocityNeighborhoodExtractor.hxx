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
#ifndef itkLevelSetVelocityNeighborhoodExtractor_hxx
#define itkLevelSetVelocityNeighborhoodExtractor_hxx

#include "itkLevelSetVelocityNeighborhoodExtractor.h"
#include "itkMath.h"

namespace itk
{
/**
 *
 */
template< typename TLevelSet, typename TAuxValue,
          unsigned int VAuxDimension >
LevelSetVelocityNeighborhoodExtractor< TLevelSet, TAuxValue, VAuxDimension >
::LevelSetVelocityNeighborhoodExtractor()
{
  m_AuxInsideValues = ITK_NULLPTR;
  m_AuxOutsideValues = ITK_NULLPTR;
  for ( unsigned int i = 0; i < VAuxDimension; ++i )
    {
    m_AuxImage[i] = ITK_NULLPTR;
    }
}

/*
 *
 */
template< typename TLevelSet, typename TAuxValue,
          unsigned int VAuxDimension >
void
LevelSetVelocityNeighborhoodExtractor< TLevelSet, TAuxValue, VAuxDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Input aux image: [";
  unsigned int j;
  for ( j = 0; j + 1 < VAuxDimension; j++ )
    {
    os << m_AuxImage[j].GetPointer() << ", ";
    }
  os << m_AuxImage[j].GetPointer() << "]" << std::endl;
  os << indent << "AuxInsideValues: "
     << m_AuxInsideValues.GetPointer() << std::endl;
  os << indent << "AuxOutsideValues: "
     << m_AuxOutsideValues.GetPointer() << std::endl;
}

/*
 *
 */
template< typename TLevelSet, typename TAuxValue,
          unsigned int VAuxDimension >
void
LevelSetVelocityNeighborhoodExtractor< TLevelSet, TAuxValue, VAuxDimension >
::Initialize()
{
  this->Superclass::Initialize();

  // create new empty auxiliary variable containers
  m_AuxInsideValues = AuxValueContainer::New();
  m_AuxOutsideValues = AuxValueContainer::New();
}

/*
 *
 */
template< typename TLevelSet, typename TAuxValue,
          unsigned int VAuxDimension >
double
LevelSetVelocityNeighborhoodExtractor< TLevelSet, TAuxValue, VAuxDimension >
::CalculateDistance(
  Index & index)
{
  double distance = this->Superclass::CalculateDistance(index);

  if ( distance >= this->GetLargeValue() )
    {
    return distance;
    }

  double             centerValue[VAuxDimension];
  AuxValueType       auxPixel;
  AuxValueVectorType auxVector;

  for ( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    auxPixel = m_AuxImage[k]->GetPixel(index);
    centerValue[k] = (double)auxPixel;
    }

  // if distance is zero, insert point in inside container
  if ( this->GetLastPointIsInside() )
    {
    for ( unsigned int k = 0; k < VAuxDimension; k++ )
      {
      auxVector[k] = centerValue[k];
      }

    m_AuxInsideValues->InsertElement(m_AuxInsideValues->Size(), auxVector);

    return distance;
    }

  double denom = 0.0;
  double numer[VAuxDimension];
  typename Superclass::NodeType neighNode;

  for ( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    numer[k] = 0.0;
    }

  // The extend velcoity value is a weighted value of
  // the speed values at point used in the computation
  // of the distance by the superclass.
  //
  // The weights is proportional to one over the square
  // of distance along the grid line to the zero set
  // crossing.

  for ( unsigned int j = 0; j < SetDimension; j++ )
    {
    neighNode = this->GetNodeUsedInCalculation(j);
    if ( neighNode.GetValue() >= this->GetLargeValue() )
      {
      break;
      }

    denom += 1.0 / itk::Math::sqr( (double)neighNode.GetValue() );
    for ( unsigned int k = 0; k < VAuxDimension; k++ )
      {
      auxPixel = m_AuxImage[k]->GetPixel( neighNode.GetIndex() );
      numer[k] += (double)( auxPixel ) / itk::Math::sqr( (double)neighNode.GetValue() );
      }
    }

  for ( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    numer[k] /= denom;
    auxVector[k] = numer[k];
    }

  if ( this->GetLastPointIsInside() )
    {
    m_AuxInsideValues->InsertElement(
      m_AuxInsideValues->Size(), auxVector);
    }
  else
    {
    m_AuxOutsideValues->InsertElement(
      m_AuxOutsideValues->Size(), auxVector);
    }

  return distance;
}
} // namespace itk

#endif
