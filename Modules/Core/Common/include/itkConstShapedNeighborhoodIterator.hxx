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
#ifndef itkConstShapedNeighborhoodIterator_hxx
#define itkConstShapedNeighborhoodIterator_hxx
#include "itkConstShapedNeighborhoodIterator.h"
namespace itk
{
template< typename TImage, typename TBoundaryCondition >
void
ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent <<  "ConstShapedNeighborhoodIterator {this = " << this;
  os << " m_ActiveIndexList = [";
  for ( IndexListConstIterator it = m_ActiveIndexList.begin();
        it != m_ActiveIndexList.end();
        ++it )
    {
    os << *it << " ";
    }
  os << "] ";
  os << " m_CenterIsActive = " << m_CenterIsActive;
  os << "}" << std::endl;
  Superclass::PrintSelf( os, indent.GetNextIndent() );
}

template< typename TImage, typename TBoundaryCondition >
void
ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition >
::ActivateIndex(NeighborIndexType n)
{
  const OffsetValueType *OffsetTable = this->m_ConstImage->GetOffsetTable();

  // Insert so that the list remains ordered.
  IndexListIterator it = m_ActiveIndexList.begin();

  if ( m_ActiveIndexList.empty() )
    {
    m_ActiveIndexList.push_front(n);
    }
  else
    {
    while ( n > *it )
      {
      it++;
      if ( it == m_ActiveIndexList.end() )
        {
        break;
        }
      }
    if ( it == m_ActiveIndexList.end() )
      {
      m_ActiveIndexList.insert(it, n);
      }
    else if ( n != *it )
      {
      m_ActiveIndexList.insert(it, n);
      }
    }

  // Adjust the begin and end iterators.
  m_ConstBeginIterator.GoToBegin();
  m_ConstEndIterator.GoToEnd();

  // Did we just activate the index at the center of the neighborhood?
  if ( n == this->GetCenterNeighborhoodIndex() )
    {
    m_CenterIsActive = true;
    }

  // Set the pointer in the neighborhood location just activated.
  this->GetElement(n) = this->GetCenterPointer();
  for ( unsigned i = 0; i < Dimension; ++i )
    {
    this->GetElement(n) += OffsetTable[i] * this->GetOffset(n)[i];
    }
}

template< typename TImage, typename TBoundaryCondition >
void
ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition >
::DeactivateIndex(NeighborIndexType n)
{
  IndexListIterator it = m_ActiveIndexList.begin();

  if ( m_ActiveIndexList.empty() )
    {
    return;
    }
  else
    {
    while ( n != *it )
      {
      it++;
      if ( it == m_ActiveIndexList.end() )
        {
        return;
        }
      }
    m_ActiveIndexList.erase(it);
    }

  // Adjust the begin and end iterators.
  m_ConstBeginIterator.GoToBegin();
  m_ConstEndIterator.GoToEnd();

  // Did we just deactivate the index at the center of the neighborhood?
  if ( n == this->GetCenterNeighborhoodIndex() )
    {
    m_CenterIsActive = false;
    }
}

template< typename TImage, typename TBoundaryCondition >
void
ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition >
::CreateActiveListFromNeighborhood(const NeighborhoodType &neighborhood)
{
  if (this->GetRadius() != neighborhood.GetRadius())
    {
    itkGenericExceptionMacro(<< "Radius of shaped iterator("
                             << this->GetRadius()
                             << ") does not equal radius of neighborhood("
                             << neighborhood.GetRadius() << ")");
    }
  typename NeighborhoodType::ConstIterator nit;
  NeighborIndexType idx = 0;
  for (nit = neighborhood.Begin(); nit != neighborhood.End(); ++nit, ++idx)
    {
    if (*nit)
      {
      this->ActivateOffset(GetOffset(idx));
      }
    else
      {
      this->DeactivateOffset(GetOffset(idx));
      }
    }
}

template< typename TImage, typename TBoundaryCondition >
ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition > &
ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition >
::operator++()
{
  // Repositioning neighborhood, previous bounds check on neighborhood
  // location is invalid.
  this->m_IsInBoundsValid = false;

  if ( this->m_BoundaryCondition->RequiresCompleteNeighborhood() )
    {
    // Some Boundary Conditions, such as ZeroFluxNeumann can return
    // values from anywhere in the neighborhood, thus we can not do
    // the shaped optimization.

    NeighborhoodIterator< TImage, TBoundaryCondition >::operator++();
    }
  else
    {
    IndexListConstIterator it;

    // Center pointer must be updated whether or not it is active.
    if ( !m_CenterIsActive )
      {
      this->GetElement( this->GetCenterNeighborhoodIndex() )++;
      }

    // Increment pointers for only the active pixels.
    for ( it = m_ActiveIndexList.begin();
          it != m_ActiveIndexList.end();
          it++ )
      {
      ( this->GetElement(*it) )++;
      }

    // Check loop bounds, wrap & add pointer offsets if needed.
    for ( unsigned int ii = 0; ii < Dimension; ++ii )
      {
      this->m_Loop[ii]++;
      if ( this->m_Loop[ii] == this->m_Bound[ii] )
        {
        this->m_Loop[ii] = this->m_BeginIndex[ii];
        if ( !m_CenterIsActive )
          {
          this->GetElement( this->GetCenterNeighborhoodIndex() ) +=
            this->m_WrapOffset[ii];
          }
        for ( it = m_ActiveIndexList.begin();
              it != m_ActiveIndexList.end();
              it++ )
          {
          ( this->GetElement(*it) ) += this->m_WrapOffset[ii];
          }
        }
      else { break; }
      }
    }
  return *this;
}

template< typename TImage, typename TBoundaryCondition >
ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition > &
ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition >
::operator--()
{
  unsigned int                  i;
  IndexListConstIterator        it;

  // Repositioning neighborhood, previous bounds check on neighborhood
  // location is invalid.
  this->m_IsInBoundsValid = false;

  if ( this->m_BoundaryCondition->RequiresCompleteNeighborhood() )
    {
    // Some Boundary Conditions, such as ZeroFluxNeumann can return
    // values from anywhere in the neighborhood, thus we can not do
    // the shaped optimization.

    NeighborhoodIterator< TImage, TBoundaryCondition >::operator--();
    }
  else
    {
    // Center pointer must be updated whether or not it is active.
    if ( !m_CenterIsActive )
      {
      this->GetElement( this->GetCenterNeighborhoodIndex() )--;
      }

    // Decrement pointers for only the active pixels.
    for ( it = m_ActiveIndexList.begin();
          it != m_ActiveIndexList.end();
          it++ )
      {
      ( this->GetElement(*it) )--;
      }

    // Check loop bounds, wrap & add pointer offsets if needed.
    for ( i = 0; i < Dimension; ++i )
      {
      if ( this->m_Loop[i] == this->m_BeginIndex[i] )
        {
        this->m_Loop[i] = this->m_Bound[i] - 1;
        if ( !m_CenterIsActive )
          {
          this->GetElement( this->GetCenterNeighborhoodIndex() ) -=
            this->m_WrapOffset[i];
          }
        for ( it = m_ActiveIndexList.begin();
              it != m_ActiveIndexList.end();
              it++ )
          {
          ( this->GetElement(*it) ) -= this->m_WrapOffset[i];
          }
        }
      else
        {
        this->m_Loop[i]--;
        break;
        }
      }
    }
  return *this;
}

template< typename TImage, typename TBoundaryCondition >
ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition > &
ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition >
::operator+=(const OffsetType & idx)
{
  unsigned int                  i;
  IndexListConstIterator        it;
  OffsetValueType               accumulator = 0;
  const OffsetValueType *       stride = this->GetImagePointer()->GetOffsetTable();

  // Repositioning neighborhood, previous bounds check on neighborhood
  // location is invalid.
  this->m_IsInBoundsValid = false;

  if ( this->m_BoundaryCondition->RequiresCompleteNeighborhood() )
    {
    // Some Boundary Conditions, such as ZeroFluxNeumann can return
    // values from anywhere in the neighborhood, thus we can not do
    // the shaped optimization.

    NeighborhoodIterator< TImage, TBoundaryCondition >::operator+=(idx);
    }
  else
    {
    // Offset from the increment in the lowest dimension
    accumulator += idx[0];

    // Offsets from the stride lengths in each dimension.
    //
    // Because the image offset table is based on its buffer size and
    // not its requested region size, we don't have to worry about
    // adding in the wrapping offsets.
    for ( i = 1; i < Dimension; ++i )
      {
      accumulator += idx[i] * stride[i];
      }

    // Center pointer is always updated even if not active.
    if ( !m_CenterIsActive )
      {
      this->GetElement( this->GetCenterNeighborhoodIndex() ) += accumulator;
      }

    // Increment pointers only for those active pixels
    for ( it = m_ActiveIndexList.begin();
          it != m_ActiveIndexList.end();
          it++ )
      {
      ( this->GetElement(*it) ) += accumulator;
      }

    // Update loop counter values
    this->m_Loop += idx;
    }
  return *this;
}

template< typename TImage, typename TBoundaryCondition >
ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition > &
ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition >
::operator-=(const OffsetType & idx)
{
  unsigned int                  i;
  IndexListConstIterator        it;
  OffsetValueType               accumulator = 0;
  const OffsetValueType *       stride = this->GetImagePointer()->GetOffsetTable();

  // Repositioning neighborhood, previous bounds check on neighborhood
  // location is invalid.
  this->m_IsInBoundsValid = false;

  if ( this->m_BoundaryCondition->RequiresCompleteNeighborhood() )
    {
    // Some Boundary Conditions, such as ZeroFluxNeumann can return
    // values from anywhere in the neighborhood, thus we can not do
    // the shaped optimization.

    NeighborhoodIterator< TImage, TBoundaryCondition >::operator-=(idx);
    }
  else
    {
    // Offset from the increment in the lowest dimension
    accumulator += idx[0];

    // Offsets from the stride lengths in each dimension.
    //
    // Because the image offset table is based on its buffer size and
    // not its requested region size, we don't have to worry about
    // adding in the wrapping offsets.
    for ( i = 1; i < Dimension; ++i )
      {
      accumulator += idx[i] * stride[i];
      }

    // Center pointer is always updated even if not active.
    if ( !m_CenterIsActive )
      {
      this->GetElement( this->GetCenterNeighborhoodIndex() ) -= accumulator;
      }

    // Increment pointers only for those active pixels
    for ( it = m_ActiveIndexList.begin();
          it != m_ActiveIndexList.end();
          it++ )
      {
      ( this->GetElement(*it) ) -= accumulator;
      }

    // Update loop counter values
    this->m_Loop -= idx;
    }
  return *this;
}
} // namespace itk

#endif
