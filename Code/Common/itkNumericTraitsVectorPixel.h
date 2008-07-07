/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsVectorPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsVectorPixel_h
#define __itkNumericTraitsVectorPixel_h

#include "itkNumericTraits.h"
#include "itkVector.h"

// This file defines numeric traits for vector pixels types in itk
// TODO: Add doxygen tags..

namespace itk
{
template <> class NumericTraits<Vector<unsigned char, 1 > > {
public:
  typedef unsigned char ValueType;
  typedef Vector<unsigned char, 1> PrintType;
  typedef Vector<unsigned char, 1> AbsType;
  typedef Vector<unsigned short, 1> AccumulateType;
  typedef Vector<double, 1> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 1> FloatType;
  static const Vector<unsigned char, 1> ITKCommon_EXPORT Zero;
  static const Vector<unsigned char, 1> ITKCommon_EXPORT One;
  static Vector<unsigned char,1>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned char,1>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<signed char, 1> > {
public:
  typedef signed char ValueType;
  typedef Vector<signed char, 1> PrintType;
  typedef Vector<unsigned char, 1> AbsType;
  typedef Vector<short, 1> AccumulateType;
  typedef Vector<double, 1> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 1> FloatType;
  static const Vector<signed char, 1> ITKCommon_EXPORT Zero;
  static const Vector<signed char, 1> ITKCommon_EXPORT One;
  static Vector<signed char,1>  ITKCommon_EXPORT ZeroValue();
  static Vector<signed char,1>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<char, 1> > {
public:
  typedef char ValueType;
  typedef Vector<char, 1> PrintType;
  typedef Vector<unsigned char, 1> AbsType;
  typedef Vector<short, 1> AccumulateType;
  typedef Vector<double, 1> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 1> FloatType;
  static const Vector<char, 1> ITKCommon_EXPORT Zero;
  static const Vector<char, 1> ITKCommon_EXPORT One;
  static Vector<char,1>  ITKCommon_EXPORT ZeroValue();
  static Vector<char,1>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<short, 1> > {
public:
  typedef short ValueType;
  typedef Vector<short, 1> PrintType;
  typedef Vector<unsigned short, 1> AbsType;
  typedef Vector<int, 1> AccumulateType;
  typedef Vector<double, 1> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 1> FloatType;
  static const Vector<short, 1> ITKCommon_EXPORT Zero;
  static const Vector<short, 1> ITKCommon_EXPORT One;
  static Vector<short,1>  ITKCommon_EXPORT ZeroValue();
  static Vector<short,1>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned short, 1> > {
public:
  typedef unsigned short ValueType;
  typedef Vector<unsigned short, 1> PrintType;
  typedef Vector<unsigned short, 1> AbsType;
  typedef Vector<unsigned int, 1> AccumulateType;
  typedef Vector<double, 1> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 1> FloatType;
  static const Vector<unsigned short, 1> ITKCommon_EXPORT Zero;
  static const Vector<unsigned short, 1> ITKCommon_EXPORT One;
  static Vector<unsigned short,1>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned short,1>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<int, 1> > {
public:
  typedef int ValueType;
  typedef Vector<int, 1> PrintType;
  typedef Vector<unsigned int, 1> AbsType;
  typedef Vector<long, 1> AccumulateType;
  typedef Vector<double, 1> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 1> FloatType;
  static const Vector<int, 1> ITKCommon_EXPORT Zero;
  static const Vector<int, 1> ITKCommon_EXPORT One;
  static Vector<int,1>  ITKCommon_EXPORT ZeroValue();
  static Vector<int,1>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned int, 1> > {
public:
  typedef unsigned int ValueType;
  typedef Vector<unsigned int, 1> PrintType;
  typedef Vector<unsigned int, 1> AbsType;
  typedef Vector<unsigned long, 1> AccumulateType;
  typedef Vector<double, 1> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 1> FloatType;
  static const Vector<unsigned int, 1> ITKCommon_EXPORT Zero;
  static const Vector<unsigned int, 1> ITKCommon_EXPORT One;
  static Vector<unsigned int,1>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned int,1>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long, 1> > {
public:
  typedef long ValueType;
  typedef Vector<long, 1> PrintType;
  typedef Vector<unsigned long, 1> AbsType;
  typedef Vector<long, 1> AccumulateType;
  typedef Vector<double, 1> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 1> FloatType;
  static const Vector<long, 1> ITKCommon_EXPORT Zero;
  static const Vector<long, 1> ITKCommon_EXPORT One;
  static Vector<long,1>  ITKCommon_EXPORT ZeroValue();
  static Vector<long,1>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned long, 1> > {
public:
  typedef unsigned long ValueType;
  typedef Vector<unsigned long, 1> PrintType;
  typedef Vector<unsigned long, 1> AbsType;
  typedef Vector<unsigned long, 1> AccumulateType;
  typedef Vector<double, 1> RealType;
  typedef Vector<float, 1> FloatType;
  static const Vector<unsigned long, 1> ITKCommon_EXPORT Zero;
  static const Vector<unsigned long, 1> ITKCommon_EXPORT One;
  static Vector<unsigned long,1>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned long,1>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<float, 1> > {
public:
  typedef float ValueType;
  typedef Vector<float, 1> PrintType;
  typedef Vector<float, 1> AbsType;
  typedef Vector<double, 1> AccumulateType;
  typedef Vector<double, 1> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 1> FloatType;
  static const Vector<float, 1> ITKCommon_EXPORT Zero;
  static const Vector<float, 1> ITKCommon_EXPORT One;
  static Vector<float, 1>  ITKCommon_EXPORT ZeroValue();
  static Vector<float, 1>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<double, 1> > {
public:
  typedef double ValueType;
  typedef Vector<double, 1> PrintType;
  typedef Vector<double, 1> AbsType;
  typedef Vector<long double, 1> AccumulateType;
  typedef Vector<double, 1> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 1> FloatType;
  static const Vector<double, 1> ITKCommon_EXPORT Zero;
  static const Vector<double, 1> ITKCommon_EXPORT One;
  static Vector<double, 1>  ITKCommon_EXPORT ZeroValue();
  static Vector<double, 1>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long double, 1> > {
public:
  typedef long double ValueType;
  typedef Vector<long double, 1> PrintType;
  typedef Vector<long double, 1> AbsType;
  typedef Vector<long double, 1> AccumulateType;
  typedef Vector<long double, 1> RealType;
  typedef long double ScalarRealType;
  typedef Vector<float, 1> FloatType;
  static const Vector<long double, 1> ITKCommon_EXPORT Zero;
  static const Vector<long double, 1> ITKCommon_EXPORT One;
  static Vector<long double, 1>  ITKCommon_EXPORT ZeroValue();
  static Vector<long double, 1>  ITKCommon_EXPORT OneValue();
};

template <> class NumericTraits<Vector<unsigned char, 2 > > {
public:
  typedef unsigned char ValueType;
  typedef Vector<unsigned char, 2> PrintType;
  typedef Vector<unsigned char, 2> AbsType;
  typedef Vector<unsigned short, 2> AccumulateType;
  typedef Vector<double, 2> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 2> FloatType;
  static const Vector<unsigned char, 2> ITKCommon_EXPORT Zero;
  static const Vector<unsigned char, 2> ITKCommon_EXPORT One;
  static Vector<unsigned char,2>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned char,2>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<signed char, 2> > {
public:
  typedef signed char ValueType;
  typedef Vector<signed char, 2> PrintType;
  typedef Vector<unsigned char, 2> AbsType;
  typedef Vector<short, 2> AccumulateType;
  typedef Vector<double, 2> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 2> FloatType;
  static const Vector<signed char, 2> ITKCommon_EXPORT Zero;
  static const Vector<signed char, 2> ITKCommon_EXPORT One;
  static Vector<signed char,2>  ITKCommon_EXPORT ZeroValue();
  static Vector<signed char,2>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<char, 2> > {
public:
  typedef char ValueType;
  typedef Vector<char, 2> PrintType;
  typedef Vector<unsigned char, 2> AbsType;
  typedef Vector<short, 2> AccumulateType;
  typedef Vector<double, 2> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 2> FloatType;
  static const Vector<char, 2> ITKCommon_EXPORT Zero;
  static const Vector<char, 2> ITKCommon_EXPORT One;
  static Vector<char,2>  ITKCommon_EXPORT ZeroValue();
  static Vector<char,2>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<short, 2> > {
public:
  typedef short ValueType;
  typedef Vector<short, 2> PrintType;
  typedef Vector<unsigned short, 2> AbsType;
  typedef Vector<int, 2> AccumulateType;
  typedef Vector<double, 2> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 2> FloatType;
  static const Vector<short, 2> ITKCommon_EXPORT Zero;
  static const Vector<short, 2> ITKCommon_EXPORT One;
  static Vector<short,2>  ITKCommon_EXPORT ZeroValue();
  static Vector<short,2>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned short, 2> > {
public:
  typedef unsigned short ValueType;
  typedef Vector<unsigned short, 2> PrintType;
  typedef Vector<unsigned short, 2> AbsType;
  typedef Vector<unsigned int, 2> AccumulateType;
  typedef Vector<double, 2> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 2> FloatType;
  static const Vector<unsigned short, 2> ITKCommon_EXPORT Zero;
  static const Vector<unsigned short, 2> ITKCommon_EXPORT One;
  static Vector<unsigned short,2>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned short,2>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<int, 2> > {
public:
  typedef int ValueType;
  typedef Vector<int, 2> PrintType;
  typedef Vector<unsigned int, 2> AbsType;
  typedef Vector<long, 2> AccumulateType;
  typedef Vector<double, 2> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 2> FloatType;
  static const Vector<int, 2> ITKCommon_EXPORT Zero;
  static const Vector<int, 2> ITKCommon_EXPORT One;
  static Vector<int,2>  ITKCommon_EXPORT ZeroValue();
  static Vector<int,2>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned int, 2> > {
public:
  typedef unsigned int ValueType;
  typedef Vector<unsigned int, 2> PrintType;
  typedef Vector<unsigned int, 2> AbsType;
  typedef Vector<unsigned long, 2> AccumulateType;
  typedef Vector<double, 2> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 2> FloatType;
  static const Vector<unsigned int, 2> ITKCommon_EXPORT Zero;
  static const Vector<unsigned int, 2> ITKCommon_EXPORT One;
  static Vector<unsigned int,2>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned int,2>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long, 2> > {
public:
  typedef long ValueType;
  typedef Vector<long, 2> PrintType;
  typedef Vector<unsigned long, 2> AbsType;
  typedef Vector<long, 2> AccumulateType;
  typedef Vector<double, 2> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 2> FloatType;
  static const Vector<long, 2> ITKCommon_EXPORT Zero;
  static const Vector<long, 2> ITKCommon_EXPORT One;
  static Vector<long,2>  ITKCommon_EXPORT ZeroValue();
  static Vector<long,2>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned long, 2> > {
public:
  typedef unsigned long ValueType;
  typedef Vector<unsigned long, 2> PrintType;
  typedef Vector<unsigned long, 2> AbsType;
  typedef Vector<unsigned long, 2> AccumulateType;
  typedef Vector<double, 2> RealType;
  typedef Vector<float, 2> FloatType;
  static const Vector<unsigned long, 2> ITKCommon_EXPORT Zero;
  static const Vector<unsigned long, 2> ITKCommon_EXPORT One;
  static Vector<unsigned long,2>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned long,2>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<float, 2> > {
public:
  typedef float ValueType;
  typedef Vector<float, 2> PrintType;
  typedef Vector<float, 2> AbsType;
  typedef Vector<double, 2> AccumulateType;
  typedef Vector<double, 2> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 2> FloatType;
  static const Vector<float, 2> ITKCommon_EXPORT Zero;
  static const Vector<float, 2> ITKCommon_EXPORT One;
  static Vector<float, 2>  ITKCommon_EXPORT ZeroValue();
  static Vector<float, 2>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<double, 2> > {
public:
  typedef double ValueType;
  typedef Vector<double, 2> PrintType;
  typedef Vector<double, 2> AbsType;
  typedef Vector<long double, 2> AccumulateType;
  typedef Vector<double, 2> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 2> FloatType;
  static const Vector<double, 2> ITKCommon_EXPORT Zero;
  static const Vector<double, 2> ITKCommon_EXPORT One;
  static Vector<double, 2>  ITKCommon_EXPORT ZeroValue();
  static Vector<double, 2>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long double, 2> > {
public:
  typedef long double ValueType;
  typedef Vector<long double, 2> PrintType;
  typedef Vector<long double, 2> AbsType;
  typedef Vector<long double, 2> AccumulateType;
  typedef Vector<long double, 2> RealType;
  typedef long double ScalarRealType;
  typedef Vector<float, 2> FloatType;
  static const Vector<long double, 2> ITKCommon_EXPORT Zero;
  static const Vector<long double, 2> ITKCommon_EXPORT One;
  static Vector<long double, 2>  ITKCommon_EXPORT ZeroValue();
  static Vector<long double, 2>  ITKCommon_EXPORT OneValue();
};

template <> class NumericTraits<Vector<unsigned char, 3 > > {
public:
  typedef unsigned char ValueType;
  typedef Vector<unsigned char, 3> PrintType;
  typedef Vector<unsigned char, 3> AbsType;
  typedef Vector<unsigned short, 3> AccumulateType;
  typedef Vector<double, 3> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 3> FloatType;
  static const Vector<unsigned char, 3> ITKCommon_EXPORT Zero;
  static const Vector<unsigned char, 3> ITKCommon_EXPORT One;
  static Vector<unsigned char,3>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned char,3>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<signed char, 3> > {
public:
  typedef signed char ValueType;
  typedef Vector<signed char, 3> PrintType;
  typedef Vector<unsigned char, 3> AbsType;
  typedef Vector<short, 3> AccumulateType;
  typedef Vector<double, 3> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 3> FloatType;
  static const Vector<signed char, 3> ITKCommon_EXPORT Zero;
  static const Vector<signed char, 3> ITKCommon_EXPORT One;
  static Vector<signed char,3>  ITKCommon_EXPORT ZeroValue();
  static Vector<signed char,3>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<char, 3> > {
public:
  typedef char ValueType;
  typedef Vector<char, 3> PrintType;
  typedef Vector<unsigned char, 3> AbsType;
  typedef Vector<short, 3> AccumulateType;
  typedef Vector<double, 3> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 3> FloatType;
  static const Vector<char, 3> ITKCommon_EXPORT Zero;
  static const Vector<char, 3> ITKCommon_EXPORT One;
  static Vector<char,3>  ITKCommon_EXPORT ZeroValue();
  static Vector<char,3>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<short, 3> > {
public:
  typedef short ValueType;
  typedef Vector<short, 3> PrintType;
  typedef Vector<unsigned short, 3> AbsType;
  typedef Vector<int, 3> AccumulateType;
  typedef Vector<double, 3> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 3> FloatType;
  static const Vector<short, 3> ITKCommon_EXPORT Zero;
  static const Vector<short, 3> ITKCommon_EXPORT One;
  static Vector<short,3>  ITKCommon_EXPORT ZeroValue();
  static Vector<short,3>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned short, 3> > {
public:
  typedef unsigned short ValueType;
  typedef Vector<unsigned short, 3> PrintType;
  typedef Vector<unsigned short, 3> AbsType;
  typedef Vector<unsigned int, 3> AccumulateType;
  typedef Vector<double, 3> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 3> FloatType;
  static const Vector<unsigned short, 3> ITKCommon_EXPORT Zero;
  static const Vector<unsigned short, 3> ITKCommon_EXPORT One;
  static Vector<unsigned short,3>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned short,3>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<int, 3> > {
public:
  typedef int ValueType;
  typedef Vector<int, 3> PrintType;
  typedef Vector<unsigned int, 3> AbsType;
  typedef Vector<long, 3> AccumulateType;
  typedef Vector<double, 3> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 3> FloatType;
  static const Vector<int, 3> ITKCommon_EXPORT Zero;
  static const Vector<int, 3> ITKCommon_EXPORT One;
  static Vector<int,3>  ITKCommon_EXPORT ZeroValue();
  static Vector<int,3>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned int, 3> > {
public:
  typedef unsigned int ValueType;
  typedef Vector<unsigned int, 3> PrintType;
  typedef Vector<unsigned int, 3> AbsType;
  typedef Vector<unsigned long, 3> AccumulateType;
  typedef Vector<double, 3> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 3> FloatType;
  static const Vector<unsigned int, 3> ITKCommon_EXPORT Zero;
  static const Vector<unsigned int, 3> ITKCommon_EXPORT One;
  static Vector<unsigned int,3>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned int,3>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long, 3> > {
public:
  typedef long ValueType;
  typedef Vector<long, 3> PrintType;
  typedef Vector<unsigned long, 3> AbsType;
  typedef Vector<long, 3> AccumulateType;
  typedef Vector<double, 3> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 3> FloatType;
  static const Vector<long, 3> ITKCommon_EXPORT Zero;
  static const Vector<long, 3> ITKCommon_EXPORT One;
  static Vector<long,3>  ITKCommon_EXPORT ZeroValue();
  static Vector<long,3>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned long, 3> > {
public:
  typedef unsigned long ValueType;
  typedef Vector<unsigned long, 3> PrintType;
  typedef Vector<unsigned long, 3> AbsType;
  typedef Vector<unsigned long, 3> AccumulateType;
  typedef Vector<double, 3> RealType;
  typedef Vector<float, 3> FloatType;
  static const Vector<unsigned long, 3> ITKCommon_EXPORT Zero;
  static const Vector<unsigned long, 3> ITKCommon_EXPORT One;
  static Vector<unsigned long,3>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned long,3>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<float, 3> > {
public:
  typedef float ValueType;
  typedef Vector<float, 3> PrintType;
  typedef Vector<float, 3> AbsType;
  typedef Vector<double, 3> AccumulateType;
  typedef Vector<double, 3> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 3> FloatType;
  static const Vector<float, 3> ITKCommon_EXPORT Zero;
  static const Vector<float, 3> ITKCommon_EXPORT One;
  static Vector<float, 3>  ITKCommon_EXPORT ZeroValue();
  static Vector<float, 3>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<double, 3> > {
public:
  typedef double ValueType;
  typedef Vector<double, 3> PrintType;
  typedef Vector<double, 3> AbsType;
  typedef Vector<long double, 3> AccumulateType;
  typedef Vector<double, 3> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 3> FloatType;
  static const Vector<double, 3> ITKCommon_EXPORT Zero;
  static const Vector<double, 3> ITKCommon_EXPORT One;
  static Vector<double, 3>  ITKCommon_EXPORT ZeroValue();
  static Vector<double, 3>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long double, 3> > {
public:
  typedef long double ValueType;
  typedef Vector<long double, 3> PrintType;
  typedef Vector<long double, 3> AbsType;
  typedef Vector<long double, 3> AccumulateType;
  typedef Vector<long double, 3> RealType;
  typedef long double ScalarRealType;
  typedef Vector<float, 3> FloatType;
  static const Vector<long double, 3> ITKCommon_EXPORT Zero;
  static const Vector<long double, 3> ITKCommon_EXPORT One;
  static Vector<long double, 3>  ITKCommon_EXPORT ZeroValue();
  static Vector<long double, 3>  ITKCommon_EXPORT OneValue();
};

template <> class NumericTraits<Vector<unsigned char, 4 > > {
public:
  typedef unsigned char ValueType;
  typedef Vector<unsigned char, 4> PrintType;
  typedef Vector<unsigned char, 4> AbsType;
  typedef Vector<unsigned short, 4> AccumulateType;
  typedef Vector<double, 4> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 4> FloatType;
  static const Vector<unsigned char, 4> ITKCommon_EXPORT Zero;
  static const Vector<unsigned char, 4> ITKCommon_EXPORT One;
  static Vector<unsigned char,4>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned char,4>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<signed char, 4> > {
public:
  typedef signed char ValueType;
  typedef Vector<signed char, 4> PrintType;
  typedef Vector<unsigned char, 4> AbsType;
  typedef Vector<short, 4> AccumulateType;
  typedef Vector<double, 4> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 4> FloatType;
  static const Vector<signed char, 4> ITKCommon_EXPORT Zero;
  static const Vector<signed char, 4> ITKCommon_EXPORT One;
  static Vector<signed char,4>  ITKCommon_EXPORT ZeroValue();
  static Vector<signed char,4>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<char, 4> > {
public:
  typedef char ValueType;
  typedef Vector<char, 4> PrintType;
  typedef Vector<unsigned char, 4> AbsType;
  typedef Vector<short, 4> AccumulateType;
  typedef Vector<double, 4> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 4> FloatType;
  static const Vector<char, 4> ITKCommon_EXPORT Zero;
  static const Vector<char, 4> ITKCommon_EXPORT One;
  static Vector<char,4>  ITKCommon_EXPORT ZeroValue();
  static Vector<char,4>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<short, 4> > {
public:
  typedef short ValueType;
  typedef Vector<short, 4> PrintType;
  typedef Vector<unsigned short, 4> AbsType;
  typedef Vector<int, 4> AccumulateType;
  typedef Vector<double, 4> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 4> FloatType;
  static const Vector<short, 4> ITKCommon_EXPORT Zero;
  static const Vector<short, 4> ITKCommon_EXPORT One;
  static Vector<short,4>  ITKCommon_EXPORT ZeroValue();
  static Vector<short,4>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned short, 4> > {
public:
  typedef unsigned short ValueType;
  typedef Vector<unsigned short, 4> PrintType;
  typedef Vector<unsigned short, 4> AbsType;
  typedef Vector<unsigned int, 4> AccumulateType;
  typedef Vector<double, 4> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 4> FloatType;
  static const Vector<unsigned short, 4> ITKCommon_EXPORT Zero;
  static const Vector<unsigned short, 4> ITKCommon_EXPORT One;
  static Vector<unsigned short,4>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned short,4>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<int, 4> > {
public:
  typedef int ValueType;
  typedef Vector<int, 4> PrintType;
  typedef Vector<unsigned int, 4> AbsType;
  typedef Vector<long, 4> AccumulateType;
  typedef Vector<double, 4> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 4> FloatType;
  static const Vector<int, 4> ITKCommon_EXPORT Zero;
  static const Vector<int, 4> ITKCommon_EXPORT One;
  static Vector<int,4>  ITKCommon_EXPORT ZeroValue();
  static Vector<int,4>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned int, 4> > {
public:
  typedef unsigned int ValueType;
  typedef Vector<unsigned int, 4> PrintType;
  typedef Vector<unsigned int, 4> AbsType;
  typedef Vector<unsigned long, 4> AccumulateType;
  typedef Vector<double, 4> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 4> FloatType;
  static const Vector<unsigned int, 4> ITKCommon_EXPORT Zero;
  static const Vector<unsigned int, 4> ITKCommon_EXPORT One;
  static Vector<unsigned int,4>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned int,4>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long, 4> > {
public:
  typedef long ValueType;
  typedef Vector<long, 4> PrintType;
  typedef Vector<unsigned long, 4> AbsType;
  typedef Vector<long, 4> AccumulateType;
  typedef Vector<double, 4> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 4> FloatType;
  static const Vector<long, 4> ITKCommon_EXPORT Zero;
  static const Vector<long, 4> ITKCommon_EXPORT One;
  static Vector<long,4>  ITKCommon_EXPORT ZeroValue();
  static Vector<long,4>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned long, 4> > {
public:
  typedef unsigned long ValueType;
  typedef Vector<unsigned long, 4> PrintType;
  typedef Vector<unsigned long, 4> AbsType;
  typedef Vector<unsigned long, 4> AccumulateType;
  typedef Vector<double, 4> RealType;
  typedef Vector<float, 4> FloatType;
  static const Vector<unsigned long, 4> ITKCommon_EXPORT Zero;
  static const Vector<unsigned long, 4> ITKCommon_EXPORT One;
  static Vector<unsigned long,4>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned long,4>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<float, 4> > {
public:
  typedef float ValueType;
  typedef Vector<float, 4> PrintType;
  typedef Vector<float, 4> AbsType;
  typedef Vector<double, 4> AccumulateType;
  typedef Vector<double, 4> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 4> FloatType;
  static const Vector<float, 4> ITKCommon_EXPORT Zero;
  static const Vector<float, 4> ITKCommon_EXPORT One;
  static Vector<float, 4>  ITKCommon_EXPORT ZeroValue();
  static Vector<float, 4>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<double, 4> > {
public:
  typedef double ValueType;
  typedef Vector<double, 4> PrintType;
  typedef Vector<double, 4> AbsType;
  typedef Vector<long double, 4> AccumulateType;
  typedef Vector<double, 4> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 4> FloatType;
  static const Vector<double, 4> ITKCommon_EXPORT Zero;
  static const Vector<double, 4> ITKCommon_EXPORT One;
  static Vector<double, 4>  ITKCommon_EXPORT ZeroValue();
  static Vector<double, 4>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long double, 4> > {
public:
  typedef long double ValueType;
  typedef Vector<long double, 4> PrintType;
  typedef Vector<long double, 4> AbsType;
  typedef Vector<long double, 4> AccumulateType;
  typedef Vector<long double, 4> RealType;
  typedef long double ScalarRealType;
  typedef Vector<float, 4> FloatType;
  static const Vector<long double, 4> ITKCommon_EXPORT Zero;
  static const Vector<long double, 4> ITKCommon_EXPORT One;
  static Vector<long double, 4>  ITKCommon_EXPORT ZeroValue();
  static Vector<long double, 4>  ITKCommon_EXPORT OneValue();
};

template <> class NumericTraits<Vector<unsigned char, 5 > > {
public:
  typedef unsigned char ValueType;
  typedef Vector<unsigned char, 5> PrintType;
  typedef Vector<unsigned char, 5> AbsType;
  typedef Vector<unsigned short, 5> AccumulateType;
  typedef Vector<double, 5> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 5> FloatType;
  static const Vector<unsigned char, 5> ITKCommon_EXPORT Zero;
  static const Vector<unsigned char, 5> ITKCommon_EXPORT One;
  static Vector<unsigned char,5>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned char,5>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<signed char, 5> > {
public:
  typedef signed char ValueType;
  typedef Vector<signed char, 5> PrintType;
  typedef Vector<unsigned char, 5> AbsType;
  typedef Vector<short, 5> AccumulateType;
  typedef Vector<double, 5> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 5> FloatType;
  static const Vector<signed char, 5> ITKCommon_EXPORT Zero;
  static const Vector<signed char, 5> ITKCommon_EXPORT One;
  static Vector<signed char,5>  ITKCommon_EXPORT ZeroValue();
  static Vector<signed char,5>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<char, 5> > {
public:
  typedef char ValueType;
  typedef Vector<char, 5> PrintType;
  typedef Vector<unsigned char, 5> AbsType;
  typedef Vector<short, 5> AccumulateType;
  typedef Vector<double, 5> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 5> FloatType;
  static const Vector<char, 5> ITKCommon_EXPORT Zero;
  static const Vector<char, 5> ITKCommon_EXPORT One;
  static Vector<char,5>  ITKCommon_EXPORT ZeroValue();
  static Vector<char,5>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<short, 5> > {
public:
  typedef short ValueType;
  typedef Vector<short, 5> PrintType;
  typedef Vector<unsigned short, 5> AbsType;
  typedef Vector<int, 5> AccumulateType;
  typedef Vector<double, 5> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 5> FloatType;
  static const Vector<short, 5> ITKCommon_EXPORT Zero;
  static const Vector<short, 5> ITKCommon_EXPORT One;
  static Vector<short,5>  ITKCommon_EXPORT ZeroValue();
  static Vector<short,5>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned short, 5> > {
public:
  typedef unsigned short ValueType;
  typedef Vector<unsigned short, 5> PrintType;
  typedef Vector<unsigned short, 5> AbsType;
  typedef Vector<unsigned int, 5> AccumulateType;
  typedef Vector<double, 5> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 5> FloatType;
  static const Vector<unsigned short, 5> ITKCommon_EXPORT Zero;
  static const Vector<unsigned short, 5> ITKCommon_EXPORT One;
  static Vector<unsigned short,5>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned short,5>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<int, 5> > {
public:
  typedef int ValueType;
  typedef Vector<int, 5> PrintType;
  typedef Vector<unsigned int, 5> AbsType;
  typedef Vector<long, 5> AccumulateType;
  typedef Vector<double, 5> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 5> FloatType;
  static const Vector<int, 5> ITKCommon_EXPORT Zero;
  static const Vector<int, 5> ITKCommon_EXPORT One;
  static Vector<int,5>  ITKCommon_EXPORT ZeroValue();
  static Vector<int,5>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned int, 5> > {
public:
  typedef unsigned int ValueType;
  typedef Vector<unsigned int, 5> PrintType;
  typedef Vector<unsigned int, 5> AbsType;
  typedef Vector<unsigned long, 5> AccumulateType;
  typedef Vector<double, 5> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 5> FloatType;
  static const Vector<unsigned int, 5> ITKCommon_EXPORT Zero;
  static const Vector<unsigned int, 5> ITKCommon_EXPORT One;
  static Vector<unsigned int,5>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned int,5>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long, 5> > {
public:
  typedef long ValueType;
  typedef Vector<long, 5> PrintType;
  typedef Vector<unsigned long, 5> AbsType;
  typedef Vector<long, 5> AccumulateType;
  typedef Vector<double, 5> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 5> FloatType;
  static const Vector<long, 5> ITKCommon_EXPORT Zero;
  static const Vector<long, 5> ITKCommon_EXPORT One;
  static Vector<long,5>  ITKCommon_EXPORT ZeroValue();
  static Vector<long,5>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned long, 5> > {
public:
  typedef unsigned long ValueType;
  typedef Vector<unsigned long, 5> PrintType;
  typedef Vector<unsigned long, 5> AbsType;
  typedef Vector<unsigned long, 5> AccumulateType;
  typedef Vector<double, 5> RealType;
  typedef Vector<float, 5> FloatType;
  static const Vector<unsigned long, 5> ITKCommon_EXPORT Zero;
  static const Vector<unsigned long, 5> ITKCommon_EXPORT One;
  static Vector<unsigned long,5>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned long,5>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<float, 5> > {
public:
  typedef float ValueType;
  typedef Vector<float, 5> PrintType;
  typedef Vector<float, 5> AbsType;
  typedef Vector<double, 5> AccumulateType;
  typedef Vector<double, 5> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 5> FloatType;
  static const Vector<float, 5> ITKCommon_EXPORT Zero;
  static const Vector<float, 5> ITKCommon_EXPORT One;
  static Vector<float, 5>  ITKCommon_EXPORT ZeroValue();
  static Vector<float, 5>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<double, 5> > {
public:
  typedef double ValueType;
  typedef Vector<double, 5> PrintType;
  typedef Vector<double, 5> AbsType;
  typedef Vector<long double, 5> AccumulateType;
  typedef Vector<double, 5> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 5> FloatType;
  static const Vector<double, 5> ITKCommon_EXPORT Zero;
  static const Vector<double, 5> ITKCommon_EXPORT One;
  static Vector<double, 5>  ITKCommon_EXPORT ZeroValue();
  static Vector<double, 5>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long double, 5> > {
public:
  typedef long double ValueType;
  typedef Vector<long double, 5> PrintType;
  typedef Vector<long double, 5> AbsType;
  typedef Vector<long double, 5> AccumulateType;
  typedef Vector<long double, 5> RealType;
  typedef long double ScalarRealType;
  typedef Vector<float, 5> FloatType;
  static const Vector<long double, 5> ITKCommon_EXPORT Zero;
  static const Vector<long double, 5> ITKCommon_EXPORT One;
  static Vector<long double, 5>  ITKCommon_EXPORT ZeroValue();
  static Vector<long double, 5>  ITKCommon_EXPORT OneValue();
};

template <> class NumericTraits<Vector<unsigned char, 6 > > {
public:
  typedef unsigned char ValueType;
  typedef Vector<unsigned char, 6> PrintType;
  typedef Vector<unsigned char, 6> AbsType;
  typedef Vector<unsigned short, 6> AccumulateType;
  typedef Vector<double, 6> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 6> FloatType;
  static const Vector<unsigned char, 6> ITKCommon_EXPORT Zero;
  static const Vector<unsigned char, 6> ITKCommon_EXPORT One;
  static Vector<unsigned char,6>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned char,6>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<signed char, 6> > {
public:
  typedef signed char ValueType;
  typedef Vector<signed char, 6> PrintType;
  typedef Vector<unsigned char, 6> AbsType;
  typedef Vector<short, 6> AccumulateType;
  typedef Vector<double, 6> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 6> FloatType;
  static const Vector<signed char, 6> ITKCommon_EXPORT Zero;
  static const Vector<signed char, 6> ITKCommon_EXPORT One;
  static Vector<signed char,6>  ITKCommon_EXPORT ZeroValue();
  static Vector<signed char,6>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<char, 6> > {
public:
  typedef char ValueType;
  typedef Vector<char, 6> PrintType;
  typedef Vector<unsigned char, 6> AbsType;
  typedef Vector<short, 6> AccumulateType;
  typedef Vector<double, 6> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 6> FloatType;
  static const Vector<char, 6> ITKCommon_EXPORT Zero;
  static const Vector<char, 6> ITKCommon_EXPORT One;
  static Vector<char,6>  ITKCommon_EXPORT ZeroValue();
  static Vector<char,6>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<short, 6> > {
public:
  typedef short ValueType;
  typedef Vector<short, 6> PrintType;
  typedef Vector<unsigned short, 6> AbsType;
  typedef Vector<int, 6> AccumulateType;
  typedef Vector<double, 6> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 6> FloatType;
  static const Vector<short, 6> ITKCommon_EXPORT Zero;
  static const Vector<short, 6> ITKCommon_EXPORT One;
  static Vector<short,6>  ITKCommon_EXPORT ZeroValue();
  static Vector<short,6>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned short, 6> > {
public:
  typedef unsigned short ValueType;
  typedef Vector<unsigned short, 6> PrintType;
  typedef Vector<unsigned short, 6> AbsType;
  typedef Vector<unsigned int, 6> AccumulateType;
  typedef Vector<double, 6> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 6> FloatType;
  static const Vector<unsigned short, 6> ITKCommon_EXPORT Zero;
  static const Vector<unsigned short, 6> ITKCommon_EXPORT One;
  static Vector<unsigned short,6>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned short,6>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<int, 6> > {
public:
  typedef int ValueType;
  typedef Vector<int, 6> PrintType;
  typedef Vector<unsigned int, 6> AbsType;
  typedef Vector<long, 6> AccumulateType;
  typedef Vector<double, 6> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 6> FloatType;
  static const Vector<int, 6> ITKCommon_EXPORT Zero;
  static const Vector<int, 6> ITKCommon_EXPORT One;
  static Vector<int,6>  ITKCommon_EXPORT ZeroValue();
  static Vector<int,6>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned int, 6> > {
public:
  typedef unsigned int ValueType;
  typedef Vector<unsigned int, 6> PrintType;
  typedef Vector<unsigned int, 6> AbsType;
  typedef Vector<unsigned long, 6> AccumulateType;
  typedef Vector<double, 6> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 6> FloatType;
  static const Vector<unsigned int, 6> ITKCommon_EXPORT Zero;
  static const Vector<unsigned int, 6> ITKCommon_EXPORT One;
  static Vector<unsigned int,6>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned int,6>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long, 6> > {
public:
  typedef long ValueType;
  typedef Vector<long, 6> PrintType;
  typedef Vector<unsigned long, 6> AbsType;
  typedef Vector<long, 6> AccumulateType;
  typedef Vector<double, 6> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 6> FloatType;
  static const Vector<long, 6> ITKCommon_EXPORT Zero;
  static const Vector<long, 6> ITKCommon_EXPORT One;
  static Vector<long,6>  ITKCommon_EXPORT ZeroValue();
  static Vector<long,6>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned long, 6> > {
public:
  typedef unsigned long ValueType;
  typedef Vector<unsigned long, 6> PrintType;
  typedef Vector<unsigned long, 6> AbsType;
  typedef Vector<unsigned long, 6> AccumulateType;
  typedef Vector<double, 6> RealType;
  typedef Vector<float, 6> FloatType;
  static const Vector<unsigned long, 6> ITKCommon_EXPORT Zero;
  static const Vector<unsigned long, 6> ITKCommon_EXPORT One;
  static Vector<unsigned long,6>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned long,6>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<float, 6> > {
public:
  typedef float ValueType;
  typedef Vector<float, 6> PrintType;
  typedef Vector<float, 6> AbsType;
  typedef Vector<double, 6> AccumulateType;
  typedef Vector<double, 6> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 6> FloatType;
  static const Vector<float, 6> ITKCommon_EXPORT Zero;
  static const Vector<float, 6> ITKCommon_EXPORT One;
  static Vector<float, 6>  ITKCommon_EXPORT ZeroValue();
  static Vector<float, 6>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<double, 6> > {
public:
  typedef double ValueType;
  typedef Vector<double, 6> PrintType;
  typedef Vector<double, 6> AbsType;
  typedef Vector<long double, 6> AccumulateType;
  typedef Vector<double, 6> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 6> FloatType;
  static const Vector<double, 6> ITKCommon_EXPORT Zero;
  static const Vector<double, 6> ITKCommon_EXPORT One;
  static Vector<double, 6>  ITKCommon_EXPORT ZeroValue();
  static Vector<double, 6>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long double, 6> > {
public:
  typedef long double ValueType;
  typedef Vector<long double, 6> PrintType;
  typedef Vector<long double, 6> AbsType;
  typedef Vector<long double, 6> AccumulateType;
  typedef Vector<long double, 6> RealType;
  typedef long double ScalarRealType;
  typedef Vector<float, 6> FloatType;
  static const Vector<long double, 6> ITKCommon_EXPORT Zero;
  static const Vector<long double, 6> ITKCommon_EXPORT One;
  static Vector<long double, 6>  ITKCommon_EXPORT ZeroValue();
  static Vector<long double, 6>  ITKCommon_EXPORT OneValue();
};

template <> class NumericTraits<Vector<unsigned char, 7 > > {
public:
  typedef unsigned char ValueType;
  typedef Vector<unsigned char, 7> PrintType;
  typedef Vector<unsigned char, 7> AbsType;
  typedef Vector<unsigned short, 7> AccumulateType;
  typedef Vector<double, 7> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 7> FloatType;
  static const Vector<unsigned char, 7> ITKCommon_EXPORT Zero;
  static const Vector<unsigned char, 7> ITKCommon_EXPORT One;
  static Vector<unsigned char,7>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned char,7>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<signed char, 7> > {
public:
  typedef signed char ValueType;
  typedef Vector<signed char, 7> PrintType;
  typedef Vector<unsigned char, 7> AbsType;
  typedef Vector<short, 7> AccumulateType;
  typedef Vector<double, 7> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 7> FloatType;
  static const Vector<signed char, 7> ITKCommon_EXPORT Zero;
  static const Vector<signed char, 7> ITKCommon_EXPORT One;
  static Vector<signed char,7>  ITKCommon_EXPORT ZeroValue();
  static Vector<signed char,7>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<char, 7> > {
public:
  typedef char ValueType;
  typedef Vector<char, 7> PrintType;
  typedef Vector<unsigned char, 7> AbsType;
  typedef Vector<short, 7> AccumulateType;
  typedef Vector<double, 7> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 7> FloatType;
  static const Vector<char, 7> ITKCommon_EXPORT Zero;
  static const Vector<char, 7> ITKCommon_EXPORT One;
  static Vector<char,7>  ITKCommon_EXPORT ZeroValue();
  static Vector<char,7>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<short, 7> > {
public:
  typedef short ValueType;
  typedef Vector<short, 7> PrintType;
  typedef Vector<unsigned short, 7> AbsType;
  typedef Vector<int, 7> AccumulateType;
  typedef Vector<double, 7> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 7> FloatType;
  static const Vector<short, 7> ITKCommon_EXPORT Zero;
  static const Vector<short, 7> ITKCommon_EXPORT One;
  static Vector<short,7>  ITKCommon_EXPORT ZeroValue();
  static Vector<short,7>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned short, 7> > {
public:
  typedef unsigned short ValueType;
  typedef Vector<unsigned short, 7> PrintType;
  typedef Vector<unsigned short, 7> AbsType;
  typedef Vector<unsigned int, 7> AccumulateType;
  typedef Vector<double, 7> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 7> FloatType;
  static const Vector<unsigned short, 7> ITKCommon_EXPORT Zero;
  static const Vector<unsigned short, 7> ITKCommon_EXPORT One;
  static Vector<unsigned short,7>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned short,7>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<int, 7> > {
public:
  typedef int ValueType;
  typedef Vector<int, 7> PrintType;
  typedef Vector<unsigned int, 7> AbsType;
  typedef Vector<long, 7> AccumulateType;
  typedef Vector<double, 7> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 7> FloatType;
  static const Vector<int, 7> ITKCommon_EXPORT Zero;
  static const Vector<int, 7> ITKCommon_EXPORT One;
  static Vector<int,7>  ITKCommon_EXPORT ZeroValue();
  static Vector<int,7>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned int, 7> > {
public:
  typedef unsigned int ValueType;
  typedef Vector<unsigned int, 7> PrintType;
  typedef Vector<unsigned int, 7> AbsType;
  typedef Vector<unsigned long, 7> AccumulateType;
  typedef Vector<double, 7> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 7> FloatType;
  static const Vector<unsigned int, 7> ITKCommon_EXPORT Zero;
  static const Vector<unsigned int, 7> ITKCommon_EXPORT One;
  static Vector<unsigned int,7>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned int,7>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long, 7> > {
public:
  typedef long ValueType;
  typedef Vector<long, 7> PrintType;
  typedef Vector<unsigned long, 7> AbsType;
  typedef Vector<long, 7> AccumulateType;
  typedef Vector<double, 7> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 7> FloatType;
  static const Vector<long, 7> ITKCommon_EXPORT Zero;
  static const Vector<long, 7> ITKCommon_EXPORT One;
  static Vector<long,7>  ITKCommon_EXPORT ZeroValue();
  static Vector<long,7>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned long, 7> > {
public:
  typedef unsigned long ValueType;
  typedef Vector<unsigned long, 7> PrintType;
  typedef Vector<unsigned long, 7> AbsType;
  typedef Vector<unsigned long, 7> AccumulateType;
  typedef Vector<double, 7> RealType;
  typedef Vector<float, 7> FloatType;
  static const Vector<unsigned long, 7> ITKCommon_EXPORT Zero;
  static const Vector<unsigned long, 7> ITKCommon_EXPORT One;
  static Vector<unsigned long,7>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned long,7>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<float, 7> > {
public:
  typedef float ValueType;
  typedef Vector<float, 7> PrintType;
  typedef Vector<float, 7> AbsType;
  typedef Vector<double, 7> AccumulateType;
  typedef Vector<double, 7> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 7> FloatType;
  static const Vector<float, 7> ITKCommon_EXPORT Zero;
  static const Vector<float, 7> ITKCommon_EXPORT One;
  static Vector<float, 7>  ITKCommon_EXPORT ZeroValue();
  static Vector<float, 7>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<double, 7> > {
public:
  typedef double ValueType;
  typedef Vector<double, 7> PrintType;
  typedef Vector<double, 7> AbsType;
  typedef Vector<long double, 7> AccumulateType;
  typedef Vector<double, 7> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 7> FloatType;
  static const Vector<double, 7> ITKCommon_EXPORT Zero;
  static const Vector<double, 7> ITKCommon_EXPORT One;
  static Vector<double, 7>  ITKCommon_EXPORT ZeroValue();
  static Vector<double, 7>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long double, 7> > {
public:
  typedef long double ValueType;
  typedef Vector<long double, 7> PrintType;
  typedef Vector<long double, 7> AbsType;
  typedef Vector<long double, 7> AccumulateType;
  typedef Vector<long double, 7> RealType;
  typedef long double ScalarRealType;
  typedef Vector<float, 7> FloatType;
  static const Vector<long double, 7> ITKCommon_EXPORT Zero;
  static const Vector<long double, 7> ITKCommon_EXPORT One;
  static Vector<long double, 7>  ITKCommon_EXPORT ZeroValue();
  static Vector<long double, 7>  ITKCommon_EXPORT OneValue();
};

template <> class NumericTraits<Vector<unsigned char, 8 > > {
public:
  typedef unsigned char ValueType;
  typedef Vector<unsigned char, 8> PrintType;
  typedef Vector<unsigned char, 8> AbsType;
  typedef Vector<unsigned short, 8> AccumulateType;
  typedef Vector<double, 8> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 8> FloatType;
  static const Vector<unsigned char, 8> ITKCommon_EXPORT Zero;
  static const Vector<unsigned char, 8> ITKCommon_EXPORT One;
  static Vector<unsigned char,8>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned char,8>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<signed char, 8> > {
public:
  typedef signed char ValueType;
  typedef Vector<signed char, 8> PrintType;
  typedef Vector<unsigned char, 8> AbsType;
  typedef Vector<short, 8> AccumulateType;
  typedef Vector<double, 8> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 8> FloatType;
  static const Vector<signed char, 8> ITKCommon_EXPORT Zero;
  static const Vector<signed char, 8> ITKCommon_EXPORT One;
  static Vector<signed char,8>  ITKCommon_EXPORT ZeroValue();
  static Vector<signed char,8>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<char, 8> > {
public:
  typedef char ValueType;
  typedef Vector<char, 8> PrintType;
  typedef Vector<unsigned char, 8> AbsType;
  typedef Vector<short, 8> AccumulateType;
  typedef Vector<double, 8> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 8> FloatType;
  static const Vector<char, 8> ITKCommon_EXPORT Zero;
  static const Vector<char, 8> ITKCommon_EXPORT One;
  static Vector<char,8>  ITKCommon_EXPORT ZeroValue();
  static Vector<char,8>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<short, 8> > {
public:
  typedef short ValueType;
  typedef Vector<short, 8> PrintType;
  typedef Vector<unsigned short, 8> AbsType;
  typedef Vector<int, 8> AccumulateType;
  typedef Vector<double, 8> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 8> FloatType;
  static const Vector<short, 8> ITKCommon_EXPORT Zero;
  static const Vector<short, 8> ITKCommon_EXPORT One;
  static Vector<short,8>  ITKCommon_EXPORT ZeroValue();
  static Vector<short,8>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned short, 8> > {
public:
  typedef unsigned short ValueType;
  typedef Vector<unsigned short, 8> PrintType;
  typedef Vector<unsigned short, 8> AbsType;
  typedef Vector<unsigned int, 8> AccumulateType;
  typedef Vector<double, 8> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 8> FloatType;
  static const Vector<unsigned short, 8> ITKCommon_EXPORT Zero;
  static const Vector<unsigned short, 8> ITKCommon_EXPORT One;
  static Vector<unsigned short,8>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned short,8>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<int, 8> > {
public:
  typedef int ValueType;
  typedef Vector<int, 8> PrintType;
  typedef Vector<unsigned int, 8> AbsType;
  typedef Vector<long, 8> AccumulateType;
  typedef Vector<double, 8> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 8> FloatType;
  static const Vector<int, 8> ITKCommon_EXPORT Zero;
  static const Vector<int, 8> ITKCommon_EXPORT One;
  static Vector<int,8>  ITKCommon_EXPORT ZeroValue();
  static Vector<int,8>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned int, 8> > {
public:
  typedef unsigned int ValueType;
  typedef Vector<unsigned int, 8> PrintType;
  typedef Vector<unsigned int, 8> AbsType;
  typedef Vector<unsigned long, 8> AccumulateType;
  typedef Vector<double, 8> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 8> FloatType;
  static const Vector<unsigned int, 8> ITKCommon_EXPORT Zero;
  static const Vector<unsigned int, 8> ITKCommon_EXPORT One;
  static Vector<unsigned int,8>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned int,8>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long, 8> > {
public:
  typedef long ValueType;
  typedef Vector<long, 8> PrintType;
  typedef Vector<unsigned long, 8> AbsType;
  typedef Vector<long, 8> AccumulateType;
  typedef Vector<double, 8> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 8> FloatType;
  static const Vector<long, 8> ITKCommon_EXPORT Zero;
  static const Vector<long, 8> ITKCommon_EXPORT One;
  static Vector<long,8>  ITKCommon_EXPORT ZeroValue();
  static Vector<long,8>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned long, 8> > {
public:
  typedef unsigned long ValueType;
  typedef Vector<unsigned long, 8> PrintType;
  typedef Vector<unsigned long, 8> AbsType;
  typedef Vector<unsigned long, 8> AccumulateType;
  typedef Vector<double, 8> RealType;
  typedef Vector<float, 8> FloatType;
  static const Vector<unsigned long, 8> ITKCommon_EXPORT Zero;
  static const Vector<unsigned long, 8> ITKCommon_EXPORT One;
  static Vector<unsigned long,8>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned long,8>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<float, 8> > {
public:
  typedef float ValueType;
  typedef Vector<float, 8> PrintType;
  typedef Vector<float, 8> AbsType;
  typedef Vector<double, 8> AccumulateType;
  typedef Vector<double, 8> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 8> FloatType;
  static const Vector<float, 8> ITKCommon_EXPORT Zero;
  static const Vector<float, 8> ITKCommon_EXPORT One;
  static Vector<float, 8>  ITKCommon_EXPORT ZeroValue();
  static Vector<float, 8>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<double, 8> > {
public:
  typedef double ValueType;
  typedef Vector<double, 8> PrintType;
  typedef Vector<double, 8> AbsType;
  typedef Vector<long double, 8> AccumulateType;
  typedef Vector<double, 8> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 8> FloatType;
  static const Vector<double, 8> ITKCommon_EXPORT Zero;
  static const Vector<double, 8> ITKCommon_EXPORT One;
  static Vector<double, 8>  ITKCommon_EXPORT ZeroValue();
  static Vector<double, 8>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long double, 8> > {
public:
  typedef long double ValueType;
  typedef Vector<long double, 8> PrintType;
  typedef Vector<long double, 8> AbsType;
  typedef Vector<long double, 8> AccumulateType;
  typedef Vector<long double, 8> RealType;
  typedef long double ScalarRealType;
  typedef Vector<float, 8> FloatType;
  static const Vector<long double, 8> ITKCommon_EXPORT Zero;
  static const Vector<long double, 8> ITKCommon_EXPORT One;
  static Vector<long double, 8>  ITKCommon_EXPORT ZeroValue();
  static Vector<long double, 8>  ITKCommon_EXPORT OneValue();
};

template <> class NumericTraits<Vector<unsigned char, 9 > > {
public:
  typedef unsigned char ValueType;
  typedef Vector<unsigned char, 9> PrintType;
  typedef Vector<unsigned char, 9> AbsType;
  typedef Vector<unsigned short, 9> AccumulateType;
  typedef Vector<double, 9> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 9> FloatType;
  static const Vector<unsigned char, 9> ITKCommon_EXPORT Zero;
  static const Vector<unsigned char, 9> ITKCommon_EXPORT One;
  static Vector<unsigned char,9>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned char,9>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<signed char, 9> > {
public:
  typedef signed char ValueType;
  typedef Vector<signed char, 9> PrintType;
  typedef Vector<unsigned char, 9> AbsType;
  typedef Vector<short, 9> AccumulateType;
  typedef Vector<double, 9> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 9> FloatType;
  static const Vector<signed char, 9> ITKCommon_EXPORT Zero;
  static const Vector<signed char, 9> ITKCommon_EXPORT One;
  static Vector<signed char,9>  ITKCommon_EXPORT ZeroValue();
  static Vector<signed char,9>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<char, 9> > {
public:
  typedef char ValueType;
  typedef Vector<char, 9> PrintType;
  typedef Vector<unsigned char, 9> AbsType;
  typedef Vector<short, 9> AccumulateType;
  typedef Vector<double, 9> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 9> FloatType;
  static const Vector<char, 9> ITKCommon_EXPORT Zero;
  static const Vector<char, 9> ITKCommon_EXPORT One;
  static Vector<char,9>  ITKCommon_EXPORT ZeroValue();
  static Vector<char,9>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<short, 9> > {
public:
  typedef short ValueType;
  typedef Vector<short, 9> PrintType;
  typedef Vector<unsigned short, 9> AbsType;
  typedef Vector<int, 9> AccumulateType;
  typedef Vector<double, 9> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 9> FloatType;
  static const Vector<short, 9> ITKCommon_EXPORT Zero;
  static const Vector<short, 9> ITKCommon_EXPORT One;
  static Vector<short,9>  ITKCommon_EXPORT ZeroValue();
  static Vector<short,9>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned short, 9> > {
public:
  typedef unsigned short ValueType;
  typedef Vector<unsigned short, 9> PrintType;
  typedef Vector<unsigned short, 9> AbsType;
  typedef Vector<unsigned int, 9> AccumulateType;
  typedef Vector<double, 9> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 9> FloatType;
  static const Vector<unsigned short, 9> ITKCommon_EXPORT Zero;
  static const Vector<unsigned short, 9> ITKCommon_EXPORT One;
  static Vector<unsigned short,9>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned short,9>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<int, 9> > {
public:
  typedef int ValueType;
  typedef Vector<int, 9> PrintType;
  typedef Vector<unsigned int, 9> AbsType;
  typedef Vector<long, 9> AccumulateType;
  typedef Vector<double, 9> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 9> FloatType;
  static const Vector<int, 9> ITKCommon_EXPORT Zero;
  static const Vector<int, 9> ITKCommon_EXPORT One;
  static Vector<int,9>  ITKCommon_EXPORT ZeroValue();
  static Vector<int,9>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned int, 9> > {
public:
  typedef unsigned int ValueType;
  typedef Vector<unsigned int, 9> PrintType;
  typedef Vector<unsigned int, 9> AbsType;
  typedef Vector<unsigned long, 9> AccumulateType;
  typedef Vector<double, 9> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 9> FloatType;
  static const Vector<unsigned int, 9> ITKCommon_EXPORT Zero;
  static const Vector<unsigned int, 9> ITKCommon_EXPORT One;
  static Vector<unsigned int,9>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned int,9>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long, 9> > {
public:
  typedef long ValueType;
  typedef Vector<long, 9> PrintType;
  typedef Vector<unsigned long, 9> AbsType;
  typedef Vector<long, 9> AccumulateType;
  typedef Vector<double, 9> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 9> FloatType;
  static const Vector<long, 9> ITKCommon_EXPORT Zero;
  static const Vector<long, 9> ITKCommon_EXPORT One;
  static Vector<long,9>  ITKCommon_EXPORT ZeroValue();
  static Vector<long,9>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<unsigned long, 9> > {
public:
  typedef unsigned long ValueType;
  typedef Vector<unsigned long, 9> PrintType;
  typedef Vector<unsigned long, 9> AbsType;
  typedef Vector<unsigned long, 9> AccumulateType;
  typedef Vector<double, 9> RealType;
  typedef Vector<float, 9> FloatType;
  static const Vector<unsigned long, 9> ITKCommon_EXPORT Zero;
  static const Vector<unsigned long, 9> ITKCommon_EXPORT One;
  static Vector<unsigned long,9>  ITKCommon_EXPORT ZeroValue();
  static Vector<unsigned long,9>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<float, 9> > {
public:
  typedef float ValueType;
  typedef Vector<float, 9> PrintType;
  typedef Vector<float, 9> AbsType;
  typedef Vector<double, 9> AccumulateType;
  typedef Vector<double, 9> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 9> FloatType;
  static const Vector<float, 9> ITKCommon_EXPORT Zero;
  static const Vector<float, 9> ITKCommon_EXPORT One;
  static Vector<float, 9>  ITKCommon_EXPORT ZeroValue();
  static Vector<float, 9>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<double, 9> > {
public:
  typedef double ValueType;
  typedef Vector<double, 9> PrintType;
  typedef Vector<double, 9> AbsType;
  typedef Vector<long double, 9> AccumulateType;
  typedef Vector<double, 9> RealType;
  typedef double ScalarRealType;
  typedef Vector<float, 9> FloatType;
  static const Vector<double, 9> ITKCommon_EXPORT Zero;
  static const Vector<double, 9> ITKCommon_EXPORT One;
  static Vector<double, 9>  ITKCommon_EXPORT ZeroValue();
  static Vector<double, 9>  ITKCommon_EXPORT OneValue();
};
template <> class NumericTraits<Vector<long double, 9> > {
public:
  typedef long double ValueType;
  typedef Vector<long double, 9> PrintType;
  typedef Vector<long double, 9> AbsType;
  typedef Vector<long double, 9> AccumulateType;
  typedef Vector<long double, 9> RealType;
  typedef long double ScalarRealType;
  typedef Vector<float, 9> FloatType;
  static const Vector<long double, 9> ITKCommon_EXPORT Zero;
  static const Vector<long double, 9> ITKCommon_EXPORT One;
  static Vector<long double, 9>  ITKCommon_EXPORT ZeroValue();
  static Vector<long double, 9>  ITKCommon_EXPORT OneValue();
};

} // end namespace itk

#endif // __itkNumericTraitsVectorPixel_h  

