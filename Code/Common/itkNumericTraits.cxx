/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraits.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkNumericTraits.h"

const bool itkNumericTraits<bool>::Zero = false;
const bool itkNumericTraits<bool>::One = true;

const unsigned char itkNumericTraits<unsigned char>::Zero = 0;
const unsigned char itkNumericTraits<unsigned char>::One = 1;

const char itkNumericTraits<char>::Zero = 0;
const char itkNumericTraits<char>::One = 1;

const unsigned short itkNumericTraits<unsigned short>::Zero = 0;
const unsigned short itkNumericTraits<unsigned short>::One = 1;

const short itkNumericTraits<short>::Zero = 0;
const short itkNumericTraits<short>::One = 1;

const unsigned int itkNumericTraits<unsigned int>::Zero = 0;
const unsigned int itkNumericTraits<unsigned int>::One = 1;

const int itkNumericTraits<int>::Zero = 0;
const int itkNumericTraits<int>::One = 1;

const unsigned long itkNumericTraits<unsigned long>::Zero = 0;
const unsigned long itkNumericTraits<unsigned long>::One = 1;

const long itkNumericTraits<long>::Zero = 0UL;
const long itkNumericTraits<long>::One = 1UL;

const float itkNumericTraits<float>::Zero = 0.0F;
const float itkNumericTraits<float>::One = 1.0F;

const double itkNumericTraits<double>::Zero = 0.0;
const double itkNumericTraits<double>::One = 1.0;

const long double itkNumericTraits<long double>::Zero = 0.0;
const long double itkNumericTraits<long double>::One = 1.0;

