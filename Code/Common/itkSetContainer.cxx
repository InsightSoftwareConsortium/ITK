/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSetContainer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkSetContainer.h"


/**
 *
 */
template <typename TElement>
itkSetContainer<TElement>::Pointer
itkSetContainer<TElement>
::New(void)
{
  return new Self;
}
