/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIdentifierTraits.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkIdentifierTraits.h"

namespace itk
{

const bool IdentifierTraits<char>::IsIntegralType = true;

const bool IdentifierTraits<unsigned char>::IsIntegralType = true;

const bool IdentifierTraits<short>::IsIntegralType = true;

const bool IdentifierTraits<unsigned short>::IsIntegralType = true;

const bool IdentifierTraits<int>::IsIntegralType = true;

const bool IdentifierTraits<unsigned int>::IsIntegralType = true;

const bool IdentifierTraits<long>::IsIntegralType = true;

const bool IdentifierTraits<unsigned long>::IsIntegralType = true;
  
}
