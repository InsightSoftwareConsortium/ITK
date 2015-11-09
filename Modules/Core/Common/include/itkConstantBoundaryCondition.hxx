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
#ifndef itkConstantBoundaryCondition_hxx
#define itkConstantBoundaryCondition_hxx

#include "itkConstantBoundaryCondition.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
ConstantBoundaryCondition< TInputImage, TOutputImage >
::ConstantBoundaryCondition()
{
  OutputPixelType p;
  m_Constant = NumericTraits< OutputPixelType >::ZeroValue( p );
}

template< typename TInputImage, typename TOutputImage >
typename ConstantBoundaryCondition< TInputImage, TOutputImage >::OutputPixelType
ConstantBoundaryCondition< TInputImage, TOutputImage >
::operator()(const OffsetType &, const OffsetType &, const NeighborhoodType *) const
{
  return m_Constant;
}

template< typename TInputImage, typename TOutputImage >
typename ConstantBoundaryCondition< TInputImage, TOutputImage >::OutputPixelType
ConstantBoundaryCondition< TInputImage, TOutputImage >
::operator()(const OffsetType &, const OffsetType &, const NeighborhoodType *,
             const NeighborhoodAccessorFunctorType &) const
{
  return m_Constant;
}

template< typename TInputImage, typename TOutputImage >
void
ConstantBoundaryCondition< TInputImage, TOutputImage >
::SetConstant(const OutputPixelType & c)
{
  m_Constant = c;
}

template< typename TInputImage, typename TOutputImage >
const typename ConstantBoundaryCondition< TInputImage, TOutputImage >::OutputPixelType &
ConstantBoundaryCondition< TInputImage, TOutputImage >
::GetConstant() const
{
  return m_Constant;
}

template< typename TInputImage, typename TOutputImage >
typename ConstantBoundaryCondition< TInputImage, TOutputImage >::RegionType
ConstantBoundaryCondition< TInputImage, TOutputImage >
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

template< typename TInputImage, typename TOutputImage >
typename ConstantBoundaryCondition< TInputImage, TOutputImage >::OutputPixelType
ConstantBoundaryCondition< TInputImage, TOutputImage >
::GetPixel( const IndexType & index, const TInputImage * image ) const
{
  RegionType imageRegion = image->GetLargestPossibleRegion();
  if ( imageRegion.IsInside( index ) )
    {
    return static_cast< OutputPixelType >( image->GetPixel( index ) );
    }

  return m_Constant;
}

template< typename TInputImage, typename TOutputImage >
void
ConstantBoundaryCondition< TInputImage, TOutputImage >
::Print( std::ostream & os, Indent i ) const
{
  this->Superclass::Print( os, i );

  os << i.GetNextIndent() << "Constant: " << m_Constant << std::endl;
}


} // namespace itk

#endif
