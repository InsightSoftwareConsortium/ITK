/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPathConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPathConstIterator_txx
#define _itkPathConstIterator_txx

#include "itkPathConstIterator.h"
#include "itkOffset.h"        // for operator++

namespace itk
{

template<class TImage, class TPath>
PathConstIterator<TImage, TPath>
::PathConstIterator(const ImageType *imagePtr, const PathType  *pathPtr)
{
  m_ZeroOffset.Fill(0); 
  
  m_Image = imagePtr;
  m_Path  = pathPtr;
  
  m_ImageOrigin   = m_Image->GetOrigin();
  m_ImageSpacing  = m_Image->GetSpacing();
  m_Region        = m_Image->GetLargestPossibleRegion();
  m_ImageSize     = m_Region.GetSize().m_Size;
  
  m_VisitStartIndexAsLastIndexIfClosed=true;
  
  GoToBegin();
}


template<class TImage, class TPath>
PathConstIterator<TImage, TPath> &
PathConstIterator<TImage, TPath>
::operator=(const Self & it)
{
  m_Image  = it.m_Image;  // copy the smart pointer
  m_Path   = it.m_Path;   // copy the smart pointer
  m_Region = it.m_Region;
  m_ImageOrigin  = it.m_ImageOrigin;
  m_ImageSpacing = it.m_ImageSpacing;
  m_ImageSize    = it.m_ImageSize;
  m_CurrentPathPosition = it.m_CurrentPathPosition;
  m_CurrentImageIndex   = it.m_CurrentImageIndex;
  m_VisitStartIndexAsLastIndexIfClosed=it.m_VisitStartIndexAsLastIndexIfClosed;
}


template<class TImage, class TPath>
void
PathConstIterator<TImage, TPath>
::GoToBegin()
{
  // Go the the beginning
  m_CurrentPathPosition = m_Path->StartOfInput();
  
  // But don't visit the first index twice for closed paths (unless told to)
  if( m_VisitStartIndexAsLastIndexIfClosed )
    {
    // Are the first and last indices coincident?
    if(  m_Path->EvaluateToIndex(m_Path->EndOfInput())  ==
         m_Path->EvaluateToIndex(m_Path->StartOfInput())  )
      {
      // Skip the starting index; we will visit it later.
      m_Path->IncrementInput(m_CurrentPathPosition);
      }
    }
  // Update the other member data
  m_CurrentImageIndex   = m_Path->EvaluateToIndex( m_CurrentPathPosition );
  m_IsAtEnd = false;
}


template<class TImage, class TPath>
void
PathConstIterator<TImage, TPath>
::operator++()
{
  // We need to modify m_CurrentPathPosition, m_CurrentImageIndex, m_IsAtEnd
  OffsetType offset;

  offset = m_Path->IncrementInput(m_CurrentPathPosition);

  if( m_ZeroOffset == offset )
    {
    // We tried to go past the end (and we are still there)
    m_IsAtEnd = true;
    }
  else if( ! m_Region.IsInside( m_CurrentImageIndex ) )
    {
    // The new index is outside the acceptable region.  We can iterate no
    // farther, call this the end.  NOTE THAT INPUT IS STILL INCREMENTED.
    m_IsAtEnd = true;
    itkWarningMacro(<<"Path left region; unable to finish tracing it");
    }
  else
    {
    m_CurrentImageIndex += offset;
    }
}

} // end namespace itk

#endif
