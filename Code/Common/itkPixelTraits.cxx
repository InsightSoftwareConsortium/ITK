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
const char* itkPixelTraits<bool>::Name = "bool";

const unsigned char itkPixelTraits<unsigned char>::Zero = false;
const unsigned char itkPixelTraits<unsigned char>::One = true;
const unsigned char itkPixelTraits<unsigned char>::Min = false;
const unsigned char itkPixelTraits<unsigned char>::Max = true;
const char* itkPixelTraits<unsigned char>::Name = "unsigned char";

const signed char itkPixelTraits<signed char>::Zero = false;
const signed char itkPixelTraits<signed char>::One = true;
const signed char itkPixelTraits<signed char>::Min = false;
const signed char itkPixelTraits<signed char>::Max = true;
const char* itkPixelTraits<signed char>::Name = "signed char";

const unsigned short itkPixelTraits<unsigned short>::Zero = false;
const unsigned short itkPixelTraits<unsigned short>::One = true;
const unsigned short itkPixelTraits<unsigned short>::Min = false;
const unsigned short itkPixelTraits<unsigned short>::Max = true;
const char* itkPixelTraits<unsigned short>::Name = "unsigned short";

const signed short itkPixelTraits<signed short>::Zero = false;
const signed short itkPixelTraits<signed short>::One = true;
const signed short itkPixelTraits<signed short>::Min = false;
const signed short itkPixelTraits<signed short>::Max = true;
const char* itkPixelTraits<signed short>::Name = "signed short";

const unsigned int itkPixelTraits<unsigned int>::Zero = false;
const unsigned int itkPixelTraits<unsigned int>::One = true;
const unsigned int itkPixelTraits<unsigned int>::Min = false;
const unsigned int itkPixelTraits<unsigned int>::Max = true;
const char* itkPixelTraits<unsigned int>::Name = "unsigned int";

const signed int itkPixelTraits<signed int>::Zero = false;
const signed int itkPixelTraits<signed int>::One = true;
const signed int itkPixelTraits<signed int>::Min = false;
const signed int itkPixelTraits<signed int>::Max = true;
const char* itkPixelTraits<signed int>::Name = "signed int";

const unsigned long itkPixelTraits<unsigned long>::Zero = false;
const unsigned long itkPixelTraits<unsigned long>::One = true;
const unsigned long itkPixelTraits<unsigned long>::Min = false;
const unsigned long itkPixelTraits<unsigned long>::Max = true;
const char* itkPixelTraits<unsigned long>::Name = "unsigned long";

const signed long itkPixelTraits<signed long>::Zero = false;
const signed long itkPixelTraits<signed long>::One = true;
const signed long itkPixelTraits<signed long>::Min = false;
const signed long itkPixelTraits<signed long>::Max = true;
const char* itkPixelTraits<signed long>::Name = "signed long";

const float itkPixelTraits<float>::Zero = false;
const float itkPixelTraits<float>::One = true;
const float itkPixelTraits<float>::Min = false;
const float itkPixelTraits<float>::Max = true;
const char* itkPixelTraits<float>::Name = "float";

const double itkPixelTraits<double>::Zero = false;
const double itkPixelTraits<double>::One = true;
const double itkPixelTraits<double>::Min = false;
const double itkPixelTraits<double>::Max = true;
const char* itkPixelTraits<double>::Name = "double";

