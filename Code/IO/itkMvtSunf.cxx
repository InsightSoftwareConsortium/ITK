/*=========================================================================
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMvtSunf.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2003 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  This software is distributed WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the above copyright notices for more information.

  =========================================================================*/
#include "itkByteSwapper.h"
#include "itkImageIOBase.h"
#include <string>
/*
  convert mv floating point numbers to sun float
*/
namespace itk
{

float
MvtSunf (int numb)
{
  float x;
  int dg_exp, dg_sign, dg_mantissa;
  int sun_exp, sun_num;
#define signbit 020000000000
#define dmantissa 077777777
#define dexponent 0177
#define dmantlen 24
#define smantissa 037777777
#define sexponent 0377
#define smantlen 23
  ByteSwapper<int>::SwapFromSystemToBigEndian(&numb);
  dg_exp = (numb >> 24) & dexponent;
  dg_sign = numb & signbit;
  dg_mantissa = (numb & dmantissa) << 8;
  sun_exp = 4 * (dg_exp - 64);
  while ((dg_mantissa & signbit) == 0 && dg_mantissa != 0)
    {
    sun_exp--;
    dg_mantissa = dg_mantissa << 1;
    }
  sun_num = 0;
  sun_exp += 126;
  if (sun_exp < 0)
    {
    sun_exp = 0;
    }
  else if (sun_exp > 255)
    {
    sun_exp = 255;
    }
  dg_mantissa = dg_mantissa << 1;
  sun_num = dg_sign | (sun_exp << smantlen) | ((dg_mantissa >> 9) & smantissa);
  std::memcpy ((void *) &x, (void *) &sun_num, sizeof(x));
  return (x);
}
}
