/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPixelTraits.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkPixelTraits.h"

const bool itkPixelTraits<bool>::Zero = false;
const bool itkPixelTraits<bool>::One = true;
const bool itkPixelTraits<bool>::Min = false;
const bool itkPixelTraits<bool>::Max = true;

const unsigned char itkPixelTraits<unsigned char>::Zero = 0;
const unsigned char itkPixelTraits<unsigned char>::One = 1;
const unsigned char itkPixelTraits<unsigned char>::Min = 0;
const unsigned char itkPixelTraits<unsigned char>::Max = 255;

const signed char itkPixelTraits<signed char>::Zero = 0;
const signed char itkPixelTraits<signed char>::One = 1;
const signed char itkPixelTraits<signed char>::Min = -128;
const signed char itkPixelTraits<signed char>::Max = 127;

const unsigned short itkPixelTraits<unsigned short>::Zero = 0;
const unsigned short itkPixelTraits<unsigned short>::One = 1;
const unsigned short itkPixelTraits<unsigned short>::Min = 0;
const unsigned short itkPixelTraits<unsigned short>::Max = 65535;

const signed short itkPixelTraits<signed short>::Zero = 0;
const signed short itkPixelTraits<signed short>::One = 1;
const signed short itkPixelTraits<signed short>::Min = -32768;
const signed short itkPixelTraits<signed short>::Max = 32767;

const unsigned int itkPixelTraits<unsigned int>::Zero = 0;
const unsigned int itkPixelTraits<unsigned int>::One = 1;
const unsigned int itkPixelTraits<unsigned int>::Min = 0;
const unsigned int itkPixelTraits<unsigned int>::Max = 4294967295UL;

const signed int itkPixelTraits<signed int>::Zero = 0;
const signed int itkPixelTraits<signed int>::One = 1;
const signed int itkPixelTraits<signed int>::Min = -2147483648;
const signed int itkPixelTraits<signed int>::Max = 2147483647;

const unsigned long itkPixelTraits<unsigned long>::Zero = 0;
const unsigned long itkPixelTraits<unsigned long>::One = 1;
const unsigned long itkPixelTraits<unsigned long>::Min = 0;
const unsigned long itkPixelTraits<unsigned long>::Max = 4294967295UL;

const signed long itkPixelTraits<signed long>::Zero = 0UL;
const signed long itkPixelTraits<signed long>::One = 1UL;
const signed long itkPixelTraits<signed long>::Min = -4294967295L;
const signed long itkPixelTraits<signed long>::Max = 4294967295L;

const float itkPixelTraits<float>::Zero = 0.0F;
const float itkPixelTraits<float>::One = 1.0F;
const float itkPixelTraits<float>::Min = -1.0e+38F;
const float itkPixelTraits<float>::Max = 1.0e+38F;

const double itkPixelTraits<double>::Zero = 0.0;
const double itkPixelTraits<double>::One = 1.0;
const double itkPixelTraits<double>::Min = -1.0e+38;
const double itkPixelTraits<double>::Max = 1.0e+38;

