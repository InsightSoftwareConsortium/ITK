/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConditionalConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkConditionalConstIterator_txx
#define _itkConditionalConstIterator_txx

#include "itkConditionalConstIterator.h"

namespace itk
{

template <typename TImageType>
ConditionalConstIterator<TImageType>
::ConditionalConstIterator()
{
}


template <typename TImageType>
ConditionalConstIterator<TImageType>
::~ConditionalConstIterator()
{
}




} // end namespace itk

#endif
