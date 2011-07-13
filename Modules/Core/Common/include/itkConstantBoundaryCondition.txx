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
#ifndef __itkConstantBoundaryCondition_txx
#define __itkConstantBoundaryCondition_txx

#include "itkConstantBoundaryCondition.h"

namespace itk
{

template< class TImage >
ConstantBoundaryCondition< TImage >
::ConstantBoundaryCondition()
{
  PixelType p;
  m_Constant = NumericTraits< PixelType >::ZeroValue( p );
}

template< class TImage >
typename ConstantBoundaryCondition< TImage >::PixelType
ConstantBoundaryCondition< TImage >
::operator()(const OffsetType &, const OffsetType &, const NeighborhoodType *) const
{
  return m_Constant;
}

template< class TImage >
typename ConstantBoundaryCondition< TImage >::PixelType
ConstantBoundaryCondition< TImage >
::operator()(const OffsetType &, const OffsetType &, const NeighborhoodType *,
             const NeighborhoodAccessorFunctorType &) const
{
  return m_Constant;
}

template< class TImage >
void
ConstantBoundaryCondition< TImage >
::SetConstant(const PixelType & c)
{
  m_Constant = c;
}

template< class TImage >
const typename ConstantBoundaryCondition< TImage >::PixelType &
ConstantBoundaryCondition< TImage >
::GetConstant() const
{
  return m_Constant;
}

template< class TImage >
typename ConstantBoundaryCondition< TImage >::RegionType
ConstantBoundaryCondition< TImage >
:: GetInputRequestedRegion( const RegionType & inputLargestPossibleRegion,
                            const RegionType & outputRequestedRegion ) const
{
  RegionType inputRequestedRegion( inputLargestPossibleRegion );
  bool cropped = inputRequestedRegion.Crop( outputRequestedRegion );

  if ( !cropped )
    {
    IndexType index; index.Fill( 0 );
    SizeType size; size.Fill( 0 );
    inputRequestedRegion.SetIndex( index );
    inputRequestedRegion.SetSize( size );
    }

  return inputRequestedRegion;
}

template< class TImage >
typename ConstantBoundaryCondition< TImage >::PixelType
ConstantBoundaryCondition< TImage >
::GetPixel( const IndexType & index, const TImage * image ) const
{
  RegionType imageRegion = image->GetLargestPossibleRegion();
  if ( imageRegion.IsInside( index ) )
    {
    return image->GetPixel( index );
    }

  return m_Constant;
}

template< class TImage >
void
ConstantBoundaryCondition< TImage >
::Print( std::ostream & os, Indent i ) const
{
  this->Superclass::Print( os, i );

  std::cout << i.GetNextIndent() << "Constant: " << m_Constant << std::endl;
}


} // namespace itk

#endif
