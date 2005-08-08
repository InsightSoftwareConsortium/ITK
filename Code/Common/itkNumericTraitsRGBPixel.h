/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsRGBPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsRGBPixel_h
#define __itkNumericTraitsRGBPixel_h

#include "itkNumericTraits.h"
#include "itkRGBPixel.h"

namespace itk
{

/** \class NumericTraits<RGBPixel<unsigned char> >
 * \brief Define traits for type RGBPixel<unsigned char>.
 * \ingroup DataRepresentation
 */
template <>
class NumericTraits<RGBPixel<unsigned char> > {
public:
  typedef RGBPixel<unsigned char> ValueType;
  typedef RGBPixel<unsigned char> PrintType;
  typedef RGBPixel<unsigned char> AbsType;
  typedef RGBPixel<unsigned short> AccumulateType;
  typedef RGBPixel<double> RealType;
  typedef double           ScalarRealType;
  typedef RGBPixel<float>  FloatType;
  static const RGBPixel<unsigned char> ITKCommon_EXPORT Zero;
  static const RGBPixel<unsigned char> ITKCommon_EXPORT One;
  static ValueType ZeroValue() { return Zero; }
  
};

} // end namespace itk

#endif // __itkNumericTraitsRGBPixel_h
