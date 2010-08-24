/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionConstIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageRegionConstIteratorWithIndex_txx
#define __itkImageRegionConstIteratorWithIndex_txx

#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template< class TImage >
ImageRegionConstIteratorWithIndex< TImage > &
ImageRegionConstIteratorWithIndex< TImage >
::operator++()
{
  this->m_Remaining = false;
  for ( unsigned int in = 0; in < TImage::ImageDimension; in++ )
    {
    this->m_PositionIndex[in]++;
    if ( this->m_PositionIndex[in] < this->m_EndIndex[in] )
      {
      this->m_Position += this->m_OffsetTable[in];
      this->m_Remaining = true;
      break;
      }
    else
      {
      this->m_Position -= this->m_OffsetTable[in]
                          * ( static_cast< long >( this->m_Region.GetSize()[in] ) - 1 );
      this->m_PositionIndex[in] = this->m_BeginIndex[in];
      }
    }

  if ( !this->m_Remaining ) // It will not advance here otherwise
    {
    this->m_Position = this->m_End;
    }

  return *this;
}

//----------------------------------------------------------------------
//  Advance along the line in reverse direction
//----------------------------------------------------------------------
template< class TImage >
ImageRegionConstIteratorWithIndex< TImage > &
ImageRegionConstIteratorWithIndex< TImage >
::operator--()
{
  this->m_Remaining = false;
  for ( unsigned int in = 0; in < TImage::ImageDimension; in++ )
    {
    if ( this->m_PositionIndex[in] > this->m_BeginIndex[in] )
      {
      this->m_PositionIndex[in]--;
      this->m_Position -= this->m_OffsetTable[in];
      this->m_Remaining = true;
      break;
      }
    else
      {
      this->m_Position += this->m_OffsetTable[in]
                          * ( static_cast< long >( this->m_Region.GetSize()[in] ) - 1 );
      this->m_PositionIndex[in] = this->m_EndIndex[in] - 1;
      }
    }

  if ( !this->m_Remaining ) // It will not advance here otherwise
    {
    this->m_Position = this->m_End;
    }

  return *this;
}
} // end namespace itk

#endif
