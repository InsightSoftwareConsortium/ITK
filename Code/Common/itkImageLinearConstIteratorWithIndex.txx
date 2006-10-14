/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageLinearConstIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageLinearConstIteratorWithIndex_txx
#define _itkImageLinearConstIteratorWithIndex_txx

#include "itkImageLinearConstIteratorWithIndex.h"


namespace itk
{



//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template<class TImage>
ImageLinearConstIteratorWithIndex<TImage>
::ImageLinearConstIteratorWithIndex(
                          const ImageType *ptr,
                          const RegionType& region )
    : ImageConstIteratorWithIndex<TImage>( ptr, region ) 
{
  this->SetDirection( 0 );
}


//----------------------------------------------------------------------
//  Go to the last pixel of the current line
//----------------------------------------------------------------------
template<class TImage>
void 
ImageLinearConstIteratorWithIndex<TImage>
::GoToReverseBeginOfLine(void)
{
  long distanceToEnd = 
    this->m_EndIndex[ m_Direction ] - this->m_PositionIndex[ m_Direction ] - 1;
  this->m_Position += m_Jump * distanceToEnd; 
  this->m_PositionIndex[m_Direction] = this->m_EndIndex[m_Direction];
}



//----------------------------------------------------------------------
//  Go to the first pixel of the current line
//----------------------------------------------------------------------
template<class TImage>
void 
ImageLinearConstIteratorWithIndex<TImage>
::GoToBeginOfLine(void)
{

  long distanceToBegin = 
    this->m_PositionIndex[ m_Direction ] - this->m_BeginIndex[ m_Direction ];

  this->m_Position -= m_Jump * distanceToBegin; 

  this->m_PositionIndex[m_Direction] = this->m_BeginIndex[m_Direction];   
  
}




//----------------------------------------------------------------------
//  Pass to the past last pixel of the current line
//----------------------------------------------------------------------
template<class TImage>
void 
ImageLinearConstIteratorWithIndex<TImage>
::GoToEndOfLine(void)
{

  long distanceToEnd = 
    this->m_EndIndex[ m_Direction ] - this->m_PositionIndex[ m_Direction ];


  this->m_Position += m_Jump * distanceToEnd; 

  this->m_PositionIndex[m_Direction] = this->m_EndIndex[m_Direction];
  
}


} // end namespace itk

#endif
