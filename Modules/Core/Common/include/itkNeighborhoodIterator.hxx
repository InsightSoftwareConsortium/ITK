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
#ifndef itkNeighborhoodIterator_hxx
#define itkNeighborhoodIterator_hxx

#include "itkNeighborhoodIterator.h"

namespace itk
{

template< typename TImage, typename TBoundaryCondition >
void
NeighborhoodIterator< TImage, TBoundaryCondition >
::SetPixel(const unsigned n, const PixelType & v)
{

  if ( this->m_NeedToUseBoundaryCondition == false )
    {
    this->m_NeighborhoodAccessorFunctor.Set(this->operator[](n), v);
    }
  // Is this whole neighborhood in bounds?
  else if ( this->InBounds() )
    {
    this->m_NeighborhoodAccessorFunctor.Set(this->operator[](n), v);
    }
  else
    {
    OffsetType temp = this->ComputeInternalIndex(n);
    OffsetType OverlapLow;
    OffsetType OverlapHigh;
    OffsetType offset;

    // Calculate overlap
    for ( unsigned int ii = 0; ii < Superclass::Dimension; ++ii )
      {
      OverlapLow[ii] = this->m_InnerBoundsLow[ii] - this->m_Loop[ii];
      OverlapHigh[ii] = static_cast< OffsetValueType >( this->GetSize(ii) - ( ( this->m_Loop[ii] + 2 ) - this->m_InnerBoundsHigh[ii] ) );
      }

    bool flag = true;

    // Is this pixel in bounds?
    for ( unsigned int ii = 0; ii < Superclass::Dimension; ++ii )
      {
      if ( this->m_InBounds[ii] )
        {
        offset[ii] = 0;                        // this dimension in bounds
        }
      else  // part of this dimension spills out of bounds
        {
        if ( temp[ii] < OverlapLow[ii] )
          {
          flag = false;
          offset[ii] = OverlapLow[ii] - temp[ii];
          }
        else if ( OverlapHigh[ii] < temp[ii] )
          {
          flag = false;
          offset[ii] =  OverlapHigh[ii] - temp[ii];
          }
        else
          {
          offset[ii] = 0;
          }
        }
      }

    if ( flag )
      {
      this->m_NeighborhoodAccessorFunctor.Set(this->operator[](n), v);
      }
    else
      { // Attempt to write out of bounds
      RangeError e(__FILE__, __LINE__);
      e.SetLocation(ITK_LOCATION);
      e.SetDescription("Attempt to write out of bounds.");
      throw e;
      }
    }
}

template< typename TImage, typename TBoundaryCondition >
void
NeighborhoodIterator< TImage, TBoundaryCondition >
::SetPixel(const unsigned n, const PixelType & v, bool & status)
{
  unsigned int i;
  OffsetType   temp;

  typename OffsetType::OffsetValueType OverlapLow, OverlapHigh;

  if ( this->m_NeedToUseBoundaryCondition == false )
    {
    status = true;
    this->m_NeighborhoodAccessorFunctor.Set(this->operator[](n), v);
    }

  // Is this whole neighborhood in bounds?
  else if ( this->InBounds() )
    {
    this->m_NeighborhoodAccessorFunctor.Set(this->operator[](n), v);
    status = true;
    return;
    }
  else
    {
    temp = this->ComputeInternalIndex(n);

    // Calculate overlap.
    // Here, we are checking whether the particular pixel in the
    // neighborhood is within the bounds (when the neighborhood is not
    // completely in bounds, it is usually partly in bounds)
    for ( i = 0; i < Superclass::Dimension; i++ )
      {
      if ( !this->m_InBounds[i] ) // Part of dimension spills out of bounds
        {
        OverlapLow = this->m_InnerBoundsLow[i] - this->m_Loop[i];
        OverlapHigh =
          static_cast< OffsetValueType >( this->GetSize(i)
                                          - ( ( this->m_Loop[i] + 2 ) - this->m_InnerBoundsHigh[i] ) );
        if ( temp[i] < OverlapLow || OverlapHigh < temp[i] )
          {
          status = false;
          return;
          }
        }
      }

    this->m_NeighborhoodAccessorFunctor.Set(this->operator[](n), v);
    status = true;
    }
}

template< typename TImage, typename TBoundaryCondition >
void
NeighborhoodIterator< TImage, TBoundaryCondition >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent;
  os << "NeighborhoodIterator {this= " << this << "}" << std::endl;
  Superclass::PrintSelf( os, indent.GetNextIndent() );
}

template< typename TImage, typename TBoundaryCondition >
void
NeighborhoodIterator< TImage, TBoundaryCondition >
::SetNeighborhood(const NeighborhoodType & N)
{
  unsigned int i;
  OffsetType   OverlapLow, OverlapHigh, temp;
  bool         flag;

  const Iterator _end = this->End();
  Iterator       this_it;

  typename  NeighborhoodType::ConstIterator N_it;

  if ( this->m_NeedToUseBoundaryCondition == false )
    {
    for ( N_it = N.Begin(), this_it = this->Begin(); this_it < _end;
          this_it++, N_it++ )
      {
      this->m_NeighborhoodAccessorFunctor.Set(*this_it, *N_it);
      }
    }
  else if ( this->InBounds() )
    {
    for ( N_it = N.Begin(), this_it = this->Begin(); this_it < _end;
          this_it++, N_it++ )
      {
      this->m_NeighborhoodAccessorFunctor.Set(*this_it, *N_it);
      }
    }
  else
    {
    // Calculate overlap & initialize index
    for ( i = 0; i < Superclass::Dimension; i++ )
      {
      OverlapLow[i] = this->m_InnerBoundsLow[i] - this->m_Loop[i];
      OverlapHigh[i] =
        static_cast< OffsetValueType >( this->GetSize(i) - ( this->m_Loop[i] - this->m_InnerBoundsHigh[i] ) - 1 );
      temp[i] = 0;
      }

    // Iterate through neighborhood
    for ( N_it = N.Begin(), this_it = this->Begin();
          this_it < _end; N_it++, this_it++ )
      {
      flag = true;
      for ( i = 0; i < Superclass::Dimension; ++i )
        {
        if ( !this->m_InBounds[i] && ( ( temp[i] < OverlapLow[i] )
                                       || ( temp[i] >= OverlapHigh[i] ) ) )
          {
          flag = false;
          break;
          }
        }

      if ( flag )
        {
        this->m_NeighborhoodAccessorFunctor.Set(*this_it, *N_it);
        }

      for ( i = 0; i < Superclass::Dimension; ++i )  // Update index
        {
        temp[i]++;
        if ( (unsigned int)( temp[i] ) == this->GetSize(i) ) { temp[i] = 0; }
        else { break; }
        }
      }
    }
}
} // namespace itk

#endif
