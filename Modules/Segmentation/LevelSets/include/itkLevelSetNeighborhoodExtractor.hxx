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
#ifndef itkLevelSetNeighborhoodExtractor_hxx
#define itkLevelSetNeighborhoodExtractor_hxx

#include "itkLevelSetNeighborhoodExtractor.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkMath.h"

#include <algorithm>

namespace itk
{
/**
 *
 */
template< typename TLevelSet >
LevelSetNeighborhoodExtractor< TLevelSet >
::LevelSetNeighborhoodExtractor() :
  m_LevelSetValue(0.0),
  m_InsidePoints(ITK_NULLPTR),
  m_OutsidePoints(ITK_NULLPTR),
  m_InputLevelSet(ITK_NULLPTR),
  m_NarrowBanding(false),
  m_NarrowBandwidth(12.0),
  m_InputNarrowBand(ITK_NULLPTR),
  m_LargeValue(NumericTraits< PixelType >::max()),
  m_LastPointIsInside(false)
{
  m_NodesUsed.resize(SetDimension);
}

/*
 *
 */
template< typename TLevelSet >
void
LevelSetNeighborhoodExtractor< TLevelSet >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Input level set: " << m_InputLevelSet.GetPointer();
  os << std::endl;
  os << indent << "Level set value: " << m_LevelSetValue << std::endl;
  os << indent << "Narrow bandwidth: " << m_NarrowBandwidth << std::endl;
  os << indent << "Narrowbanding: " << m_NarrowBanding << std::endl;
  os << indent << "Input narrow band: ";
  os << m_InputNarrowBand.GetPointer() << std::endl;
}

/*
 *
 */
template< typename TLevelSet >
void
LevelSetNeighborhoodExtractor< TLevelSet >
::SetInputNarrowBand(
  NodeContainer *ptr)
{
  if ( m_InputNarrowBand != ptr )
    {
    m_InputNarrowBand = ptr;
    this->Modified();
    }
}

/**
 *
 */
template< typename TLevelSet >
void
LevelSetNeighborhoodExtractor< TLevelSet >
::Locate()
{
  this->GenerateData();
}

/**
 *
 */
template< typename TLevelSet >
void
LevelSetNeighborhoodExtractor< TLevelSet >
::Initialize()
{
  // create new emtpy points containers
  m_InsidePoints = NodeContainer::New();
  m_OutsidePoints = NodeContainer::New();

  m_ImageRegion = this->m_InputLevelSet->GetBufferedRegion();
}

/*
 *
 */
template< typename TLevelSet >
void
LevelSetNeighborhoodExtractor< TLevelSet >
::GenerateData()
{
  if ( !m_InputLevelSet )
    {
    itkExceptionMacro(<< "Input level set is ITK_NULLPTR");
    }

  this->Initialize();

  if ( m_NarrowBanding )
    {
    this->GenerateDataNarrowBand();
    }
  else
    {
    this->GenerateDataFull();
    }

  itkDebugMacro( << "No. inside points: " << m_InsidePoints->Size() );
  itkDebugMacro( << "No. outside points: " << m_OutsidePoints->Size() );
}

/*
 *
 */
template< typename TLevelSet >
void
LevelSetNeighborhoodExtractor< TLevelSet >
::GenerateDataFull()
{
  typedef ImageRegionConstIterator< LevelSetImageType > InputConstIterator;

  InputConstIterator inIt ( m_InputLevelSet,
                            m_InputLevelSet->GetBufferedRegion() );

  IndexType inputIndex;

  SizeValueType totalPixels  =
    m_InputLevelSet->GetBufferedRegion().GetNumberOfPixels();
  SizeValueType updateVisits = totalPixels / 10;
  if ( updateVisits < 1 ) { updateVisits = 1; }

  SizeValueType i;
  for ( i = 0; !inIt.IsAtEnd(); ++inIt, ++i )
    {
    // update progress
    if ( !( i % updateVisits ) )
      {
      this->UpdateProgress( (float)i / (float)totalPixels );
      }

    inputIndex = inIt.GetIndex();
    this->CalculateDistance(inputIndex);
    }
}

/**
 *
 */
template< typename TLevelSet >
void
LevelSetNeighborhoodExtractor< TLevelSet >
::GenerateDataNarrowBand()
{
  if ( !m_InputNarrowBand )
    {
    itkExceptionMacro(<< "InputNarrowBand has not been set");
    }

  typename NodeContainer::ConstIterator pointsIter;
  typename NodeContainer::ConstIterator pointsEnd;

  pointsIter = m_InputNarrowBand->Begin();
  pointsEnd = m_InputNarrowBand->End();
  NodeType node;
  double   maxValue = m_NarrowBandwidth / 2.0;

  SizeValueType totalPixels  = m_InputNarrowBand->Size();
  SizeValueType updateVisits = totalPixels / 10;
  if ( updateVisits < 1 ) { updateVisits = 1; }

  unsigned int i;
  for ( i = 0; pointsIter != pointsEnd; ++pointsIter, ++i )
    {
    // update progress
    if ( !( i % updateVisits ) )
      {
      this->UpdateProgress( (float)i / (float)totalPixels );
      }

    node = pointsIter.Value();
    if ( itk::Math::abs( node.GetValue() ) <= maxValue )
      {
      this->CalculateDistance( node.GetIndex() );
      }
    }
}

/**
 *
 */
template< typename TLevelSet >
double
LevelSetNeighborhoodExtractor< TLevelSet >
::CalculateDistance(
  IndexType & index)
{
  m_LastPointIsInside = false;

  typename LevelSetImageType::PixelType centerValue;
  PixelType inputPixel;

  inputPixel = m_InputLevelSet->GetPixel(index);
  centerValue = (double)inputPixel;
  centerValue -= m_LevelSetValue;

  NodeType centerNode;
  centerNode.SetIndex(index);

  if ( centerValue == 0.0 )
    {
    centerNode.SetValue(0.0);
    m_InsidePoints->InsertElement(m_InsidePoints->Size(), centerNode);
    m_LastPointIsInside = true;
    return 0.0;
    }

  bool inside = ( centerValue <= 0.0 );

  IndexType neighIndex = index;
  typename LevelSetImageType::PixelType neighValue;
  NodeType neighNode;
  SpacePrecisionType distance;
  SpacePrecisionType spacing;

  // In each dimension, find the distance to the zero set
  // by linear interpolating along the grid line.
  for ( unsigned int j = 0; j < SetDimension; j++ )
    {
    neighNode.SetValue(m_LargeValue);
    spacing = m_InputLevelSet->GetSpacing()[j];

    for ( int s = -1; s < 2; s = s + 2 )
      {
      neighIndex[j] = index[j] + s;

      if ( ! this->m_ImageRegion.IsInside( neighIndex ) )
        {
        continue;
        }

      inputPixel = m_InputLevelSet->GetPixel(neighIndex);
      neighValue = inputPixel;
      neighValue -= m_LevelSetValue;

      if ( ( neighValue > 0 && inside )
           || ( neighValue < 0 && !inside ) )
        {
        distance = centerValue / ( centerValue - neighValue ) * spacing;

        if ( neighNode.GetValue() > distance )
          {
          neighNode.SetValue(distance);
          neighNode.SetIndex(neighIndex);
          }
        }
      } // end one dim loop

    // put the minimum distance neighbor onto the heap
    m_NodesUsed[j] = neighNode;

    // reset neighIndex
    neighIndex[j] = index[j];
    } // end dimension loop

  // sort the neighbors according to distance
  std::sort( m_NodesUsed.begin(), m_NodesUsed.end() );

  // The final distance is given by the minimum distance to the plane
  // crossing formed by the zero set crossing points.
  distance = 0.0;
  for ( unsigned int j = 0; j < SetDimension; j++ )
    {
    neighNode = m_NodesUsed[j];

    if ( neighNode.GetValue() >= m_LargeValue )
      {
      break;
      }

    distance += 1.0 / itk::Math::sqr( (double)neighNode.GetValue() );
    }

  if ( distance == 0.0 )
    {
    return m_LargeValue;
    }

  distance = std::sqrt(1.0 / distance);
  centerNode.SetValue(distance);

  if ( inside )
    {
    m_InsidePoints->InsertElement(m_InsidePoints->Size(), centerNode);
    m_LastPointIsInside = true;
    }
  else
    {
    m_OutsidePoints->InsertElement(m_OutsidePoints->Size(), centerNode);
    m_LastPointIsInside = false;
    }

  return distance;
}
} // namespace itk

#endif
