/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SliceFiller.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __SliceFiller_txx
#define __SliceFiller_txx

#include "SliceFiller.h"

#include "itkNumericTraits.h"
#include "itkImageSliceConstIteratorWithIndex.h"
#include "itkImageSliceIteratorWithIndex.h"

template< class TImage >
SliceFiller< TImage >
::SliceFiller()
{
  m_StartingSliceNumber = -1 ;
  m_DesiredSize.Fill(0) ;
  m_SliceSize = 0 ;
  m_BackgroundPixelValue = 
    itk::NumericTraits< typename TImage::PixelType >::min() ;
}

template< class TImage >
SliceFiller< TImage >
::~SliceFiller()
{
}

template< class TImage >
void
SliceFiller< TImage >
::SetStartingSliceNumber(int sliceNumber)
{
  if ( m_StartingSliceNumber != sliceNumber )
    {
      m_StartingSliceNumber = sliceNumber ;
      this->Modified() ;
    }
}

template< class TImage >
void
SliceFiller< TImage >
::SetDesiredSize(typename TImage::SizeType size)
{
  if ( m_DesiredSize != size )
    {
      m_DesiredSize = size ;
      m_SliceSize = 1 ;
      for ( int i = 0 ; i < (TImage::ImageDimension - 1) ; i++ )
        {
          m_SliceSize *= m_DesiredSize[i] ;
        } 
      this->Modified() ;
    }
}

template< class TImage >
void
SliceFiller< TImage >
::SetBackgroundPixelValue(typename TImage::PixelType value)
{
  if ( m_BackgroundPixelValue != value )
    {
      m_BackgroundPixelValue = value ;
      this->Modified() ;
    }
}
  
template< class TImage >
void
SliceFiller< TImage >
::GenerateData()
{
  typename TImage::IndexType index ;
  typename TImage::RegionType region ;
  index.Fill(0) ;
  region.SetSize(m_DesiredSize) ;
  region.SetIndex(index) ;
  TImage* outputPtr = this->GetOutput() ;
  outputPtr->SetLargestPossibleRegion(region) ;
  outputPtr->SetRequestedRegion(region) ;
  outputPtr->SetBufferedRegion(region) ;
  outputPtr->Allocate() ;
  itk::ImageSliceIteratorWithIndex< TImage > o_iter(outputPtr, region) ;
  o_iter.GoToBegin() ;
  o_iter.SetFirstDirection( 0 );  // 0=x, 1=y, 2=z
  o_iter.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z

  int endingSliceNumber ;
  const TImage* inputPtr = this->GetInput() ;
  itk::ImageSliceConstIteratorWithIndex< TImage > 
    i_iter(inputPtr, inputPtr->GetLargestPossibleRegion()) ;
  
  i_iter.GoToBegin() ;
  i_iter.SetFirstDirection( 0 );  // 0=x, 1=y, 2=z
  i_iter.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z
  if ( m_StartingSliceNumber < 0 )
    {
      int i = 0 ;
      // ignore the slices that have negative slice numbers.
      while( i < -m_StartingSliceNumber )
        {
          while( !i_iter.IsAtEndOfSlice() )
            {
              while( !i_iter.IsAtEndOfLine() )
                {
                  ++i_iter ;
                }
              i_iter.NextLine() ;
            }
          i_iter.NextSlice() ;
          ++i ;
        }
    }
  endingSliceNumber = 
    inputPtr->GetLargestPossibleRegion().GetSize()[TImage::ImageDimension - 1] + 
    m_StartingSliceNumber - 1 ;
  int sliceNumber = 0 ;

  while ( !o_iter.IsAtEnd() )
    {
      if ( sliceNumber < m_StartingSliceNumber || 
           sliceNumber > endingSliceNumber )
        {
          while ( !o_iter.IsAtEndOfSlice() )
            {
              while( !o_iter.IsAtEndOfLine() )
                {
                  o_iter.Set(m_BackgroundPixelValue) ;
                  ++o_iter ;
                }
              o_iter.NextLine() ;
            }
        }
      else
        {
          while ( !o_iter.IsAtEndOfSlice() )
            {
              while( !o_iter.IsAtEndOfLine() )
                {
                  o_iter.Set(i_iter.Get()) ;
                  ++i_iter ;
                  ++o_iter ;
                }
              i_iter.NextLine() ;
              o_iter.NextLine() ;
            }
          i_iter.NextSlice() ;
        }
      ++sliceNumber ;
      o_iter.NextSlice() ;
    }
}

#endif
