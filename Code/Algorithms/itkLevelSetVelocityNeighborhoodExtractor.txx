/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetVelocityNeighborhoodExtractor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#include "vnl/vnl_math.h"

namespace itk
{

/**
 *
 */
template <class TLevelSet, class TAuxValue, 
unsigned int VAuxDimension>
LevelSetVelocityNeighborhoodExtractor<TLevelSet,TAuxValue,VAuxDimension>
::LevelSetVelocityNeighborhoodExtractor( )
{
  
}

/**
 *
 */
template <class TLevelSet, class TAuxValue,
  unsigned int VAuxDimension>
void
LevelSetVelocityNeighborhoodExtractor<TLevelSet,TAuxValue,VAuxDimension>
::PrintSelf(std::ostream &os, Indent indent)
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Locate level set with auxiliary variable extension" 
    << std::endl;
}

/**
 *
 */
template <class TLevelSet, class TAuxValue,
  unsigned int VAuxDimension>
void
LevelSetVelocityNeighborhoodExtractor<TLevelSet,TAuxValue,VAuxDimension>
::Initialize()
{
  this->Superclass::Initialize();

  // create new empty auxiliary variable containers
  m_AuxInsideValues = AuxValueContainer::New();
  m_AuxOutsideValues = AuxValueContainer::New();

}

/**
 *
 */
template <class TLevelSet, class TAuxValue,
  unsigned int VAuxDimension>
double
LevelSetVelocityNeighborhoodExtractor<TLevelSet,TAuxValue,VAuxDimension>
::CalculateDistance(
Index& index)
{
  double distance = this->Superclass::CalculateDistance( index );
  if( distance >= this->GetLargeValue() )
  {
    return distance;
  }

  // is this an inside or outside point
  typename LevelSetImageType::ScalarValueType pixelValue;
  PixelType inputPixel;

  inputPixel = (this->GetInput())->GetPixel( index );
  pixelValue = (double) ScalarTraits<PixelType>::GetScalar( inputPixel );
  pixelValue -= this->GetLevelSetValue();

  bool inside = ( pixelValue <= 0.0 );
  double centerValue[VAuxDimension];
  AuxValueType auxPixel;
  AuxValueVectorType auxVector;

  for( unsigned int k = 0; k < VAuxDimension; k++ )
  {
    auxPixel = m_AuxImage[k]->GetPixel( index );
    centerValue[k] = (double) ScalarTraits<AuxValueType>::
      GetScalar( auxPixel );
  }

  // if distance is zero, insert point in inside container
  if( distance == 0.0 )
  {
    for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
      ScalarTraits<AuxValueType>::SetScalar( auxVector[k], centerValue[k] );
    }

    m_AuxInsideValues->InsertElement( m_AuxInsideValues->Size(), auxVector );
    
    return distance;
  }

  double denom = 0.0;
  double numer[VAuxDimension];
  NodeType neighNode;

  for( unsigned int k = 0; k < VAuxDimension; k++ )
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

  for( int j = 0; j < SetDimension; j++ )
  {
    neighNode = this->GetNodeUsedInCalculation(j);
    if( neighNode.value >= this->GetLargeValue() )
    {
      break;
    }

    denom += 1.0 / vnl_math_sqr( neighNode.value );
    for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
      auxPixel = m_AuxImage[k]->GetPixel( neighNode.index );
      numer[k] += (double) ScalarTraits<AuxValueType>::
        GetScalar( auxPixel ) / vnl_math_sqr( neighNode.value );
    }

  }

  for( unsigned int k = 0; k < VAuxDimension; k++ )
  {
    numer[k] /= denom;
    ScalarTraits<AuxValueType>::SetScalar( auxVector[k], numer[k] );
  }

  if( inside )
  {
    m_AuxInsideValues->InsertElement( 
      m_AuxInsideValues->Size(), auxVector );
  }
  else
  {
    m_AuxOutsideValues->InsertElement( 
      m_AuxOutsideValues->Size(), auxVector );
  }

  return distance;

}

} // namespace itk
