/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLocateLevelSet.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkPixelTraits.h"
#include "vnl/vnl_math.h"

#include <algorithm>

namespace itk
{

/**
 *
 */
template <class TLevelSet>
LocateLevelSet<TLevelSet>
::LocateLevelSet()
{
  m_InputLevelSet = NULL;  
  m_LevelSetValue = 0.0;

  typedef typename LevelSetImageType::ScalarValueType ScalarValueType;
  m_LargeValue = NumericTraits<ScalarValueType>::max();
  m_NodesUsed.resize( SetDimension );

  m_NarrowBanding = false;
  m_NarrowBandwidth = 12.0;
  m_InputNarrowBand = NULL;

  m_DebugOn = true;

}

/**
 *
 */
template <class TLevelSet>
void
LocateLevelSet<TLevelSet>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Locate level set " << m_LevelSetValue << std::endl;
}

/**
 *
 */
template <class TLevelSet>
void
LocateLevelSet<TLevelSet>
::SetInputNarrowBand(
NodeContainer * ptr )
{
  if( m_InputNarrowBand != ptr )
    {
    m_InputNarrowBand = ptr;
    this->Modified();
    }
}

/**
 *
 */
template <class TLevelSet>
void
LocateLevelSet<TLevelSet>
::SetInput(TLevelSet *ptr)
{
  if( m_InputLevelSet != ptr )
    {
    m_InputLevelSet = ptr;
    this->Modified();
    }
}

/**
 *
 */
template <class TLevelSet>
LocateLevelSet<TLevelSet>::LevelSetPointer
LocateLevelSet<TLevelSet>
::GetInput()
{
  return m_InputLevelSet;
}


/**
 *
 */
template <class TLevelSet>
void
LocateLevelSet<TLevelSet>
::Locate()
{
  this->GenerateData();
}


/**
 *
 */
template <class TLevelSet>
void
LocateLevelSet<TLevelSet>
::Initialize()
{
  // create new emtpy points containers
  m_InsidePoints = NodeContainer::New();
  m_OutsidePoints = NodeContainer::New();

  m_ImageSize = 
    m_InputLevelSet->GetLargestPossibleRegion().GetSize();

}

/**
 *
 */
template <class TLevelSet>
void
LocateLevelSet<TLevelSet>
::GenerateData()
{
  if( !m_InputLevelSet ) 
    { 
    throw ExceptionObject();
    }

  this->Initialize();


  if( m_NarrowBanding )
    {
    this->GenerateDataNarrowBand();
    }
  else
    {
    this->GenerateDataFull();
    }

  if( m_DebugOn )
    {
    std::cout << "No. inside points: " << m_InsidePoints->Size() << std::endl;
    std::cout << "No. outside points: " << m_OutsidePoints->Size() << std::endl;
    }

}


/**
 *
 */
template <class TLevelSet>
void
LocateLevelSet<TLevelSet>
::GenerateDataFull()
{

  typedef ImageRegionIterator<PixelType,SetDimension>
    InputIterator;

  InputIterator inIt ( m_InputLevelSet,
                       m_InputLevelSet->GetBufferedRegion() );

  IndexType inputIndex;

  for( inIt = inIt.Begin(); !inIt.IsAtEnd(); ++inIt )
    {
    inputIndex = inIt.GetIndex();
    this->CalculateDistance( inputIndex );
    }

}

/**
 *
 */
template <class TLevelSet>
void
LocateLevelSet<TLevelSet>
::GenerateDataNarrowBand()
{
  if( !m_InputNarrowBand )
    {
    throw ExceptionObject();
    }

  typename NodeContainer::ConstIterator pointsIter;
  typename NodeContainer::ConstIterator pointsEnd;

  pointsIter = m_InputNarrowBand->Begin();
  pointsEnd = m_InputNarrowBand->End();
  NodeType node;
  double maxValue = m_NarrowBandwidth / 2.0;

  for( ; pointsIter != pointsEnd; ++pointsIter )
    {
    node = pointsIter.Value();
    if( vnl_math_abs( node.value ) <= maxValue )
      {
      this->CalculateDistance( node.index );
      }
    }

}

/**
 *
 */
template <class TLevelSet>
double
LocateLevelSet<TLevelSet>
::CalculateDistance(
IndexType& index)
{
  typename LevelSetImageType::ScalarValueType centerValue;
  PixelType inputPixel;

  inputPixel = m_InputLevelSet->GetPixel( index );
  centerValue = (double) ScalarTraits<PixelType>::GetScalar( inputPixel );
  centerValue -= m_LevelSetValue;

  NodeType centerNode;
  centerNode.index = index;

  if( centerValue == 0.0 )
    { 
    centerNode.value = 0.0;
    m_InsidePoints->InsertElement( m_InsidePoints->Size(), centerNode );
    return 0.0;
    }

  bool inside = ( centerValue <= 0.0 );

  IndexType neighIndex = index;
  typename LevelSetImageType::ScalarValueType neighValue;
  NodeType  neighNode;
  double distance;

  // In each dimension, find the distance to the zero set
  // by linear interpolating along the grid line.
  for( int j = 0; j < SetDimension; j++ )
    {
    neighNode.value = m_LargeValue;

    for( int s = -1; s < 2; s = s + 2 )
      {
      neighIndex[j] = index[j] + s;
      
      if( neighIndex[j] < 0 || neighIndex[j] > m_ImageSize[j] - 1 )
        {
        continue;
        }
          
      inputPixel = m_InputLevelSet->GetPixel( neighIndex );
      neighValue = ScalarTraits<PixelType>::GetScalar( inputPixel );
      neighValue -= m_LevelSetValue;

      if( ( neighValue > 0 && inside ) ||
          ( neighValue < 0 && !inside ) )
        {
         distance = centerValue / ( centerValue - neighValue );

         if( neighNode.value > distance )
          {
            neighNode.value = distance;
            neighNode.index = neighIndex;
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
  for( int j = 0; j < SetDimension; j++ )
    {
    neighNode = m_NodesUsed[j];

    if( neighNode.value >= m_LargeValue )
      { 
      break;
      }

    distance += 1.0 / vnl_math_sqr( neighNode.value );
    }

  if( distance == 0.0 )
    {
    return m_LargeValue;
    }

  distance = vnl_math_sqrt( 1.0 / distance );
  centerNode.value = distance;

  if( inside )
    {
    m_InsidePoints->InsertElement( m_InsidePoints->Size(), centerNode );
    }
  else
    {
    m_OutsidePoints->InsertElement( m_OutsidePoints->Size(), centerNode );
    }

  return distance;

}

} // namespace itk
