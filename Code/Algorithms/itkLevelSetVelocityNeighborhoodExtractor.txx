/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetVelocityNeighborhoodExtractor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkLevelSetVelocityNeighborhoodExtractor_txx
#define _itkLevelSetVelocityNeighborhoodExtractor_txx

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

#endif
