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

const bool itkIdentifierTraits<char>::IsIntegralType = true;

const bool itkIdentifierTraits<unsigned char>::IsIntegralType = true;

const bool itkIdentifierTraits<short>::IsIntegralType = true;

const bool itkIdentifierTraits<unsigned short>::IsIntegralType = true;

const bool itkIdentifierTraits<int>::IsIntegralType = true;

const bool itkIdentifierTraits<unsigned int>::IsIntegralType = true;

const bool itkIdentifierTraits<long>::IsIntegralType = true;

const bool itkIdentifierTraits<unsigned long>::IsIntegralType = true;
