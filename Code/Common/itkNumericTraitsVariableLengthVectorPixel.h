/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsVariableLengthVectorPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsVariableLengthVector_h
#define __itkNumericTraitsVariableLengthVector_h

#include "itkNumericTraits.h"
#include "itkVariableLengthVector.h"

// This file is defines numeric traits for VariableLengthVector< T > as pixel type
// Note that the Zero and the One methods here take references to a pixel as input.
// This is due to the fact that the length of the VariableLengthVector is not
// known until run-time. Since the most common use of Zero and One is for 
// comparison purposes or initialization of sums etc, this might just as easily
// be re-written with a pixel passed in as a reference and the length is inferred
// from this pixel.
// 
// This work is part of the National Alliance for Medical Image Computing 
// (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
// for Medical Research, Grant U54 EB005149.
 
namespace itk
{
template <> class NumericTraits<VariableLengthVector<unsigned char > > {
public:
  typedef VariableLengthVector<unsigned char> ValueType;
  typedef VariableLengthVector<unsigned char> PrintType;
  typedef VariableLengthVector<unsigned char> AbsType;
  typedef VariableLengthVector<unsigned short> AccumulateType;
  typedef VariableLengthVector<double> RealType;
  typedef double ScalarRealType;
  typedef VariableLengthVector<float> FloatType;
  static const VariableLengthVector<unsigned char> ITKCommon_EXPORT 
                        Zero( const VariableLengthVector< unsigned char > & );
  static const VariableLengthVector<unsigned char> ITKCommon_EXPORT 
                        One(const VariableLengthVector< unsigned char >&);
};
template <> class NumericTraits<VariableLengthVector<signed char> > {
public:
  typedef VariableLengthVector<signed char> ValueType;
  typedef VariableLengthVector<signed char> PrintType;
  typedef VariableLengthVector<unsigned char> AbsType;
  typedef VariableLengthVector<short> AccumulateType;
  typedef VariableLengthVector<double> RealType;
  typedef double ScalarRealType;
  typedef VariableLengthVector<float> FloatType;
  static const VariableLengthVector<signed char> ITKCommon_EXPORT 
                        Zero( const VariableLengthVector< signed char > & );
  static const VariableLengthVector<signed char> ITKCommon_EXPORT 
                        One(const VariableLengthVector< signed char >&);
};
template <> class NumericTraits<VariableLengthVector<char> > {
public:
  typedef VariableLengthVector<char> ValueType;
  typedef VariableLengthVector<char> PrintType;
  typedef VariableLengthVector<unsigned char> AbsType;
  typedef VariableLengthVector<short> AccumulateType;
  typedef VariableLengthVector<double> RealType;
  typedef double ScalarRealType;
  typedef VariableLengthVector<float> FloatType;
  static const VariableLengthVector<char> ITKCommon_EXPORT 
                        Zero( const VariableLengthVector< char > & );
  static const VariableLengthVector<char> ITKCommon_EXPORT 
                        One(const VariableLengthVector< char >&);
};
template <> class NumericTraits<VariableLengthVector<short> > {
public:
  typedef VariableLengthVector<short> ValueType;
  typedef VariableLengthVector<short> PrintType;
  typedef VariableLengthVector<unsigned short> AbsType;
  typedef VariableLengthVector<int> AccumulateType;
  typedef VariableLengthVector<double> RealType;
  typedef double ScalarRealType;
  typedef VariableLengthVector<float> FloatType;
  static const VariableLengthVector<short> ITKCommon_EXPORT 
                        Zero( const VariableLengthVector< short > & );
  static const VariableLengthVector<short> ITKCommon_EXPORT 
                        One(const VariableLengthVector< short >&);
};
template <> class NumericTraits<VariableLengthVector<unsigned short> > {
public:
  typedef VariableLengthVector<unsigned short> ValueType;
  typedef VariableLengthVector<unsigned short> PrintType;
  typedef VariableLengthVector<unsigned short> AbsType;
  typedef VariableLengthVector<unsigned int> AccumulateType;
  typedef VariableLengthVector<double> RealType;
  typedef double ScalarRealType;
  typedef VariableLengthVector<float> FloatType;
  static const VariableLengthVector<unsigned short> ITKCommon_EXPORT 
                        Zero( const VariableLengthVector< unsigned short > & );
  static const VariableLengthVector<unsigned short> ITKCommon_EXPORT 
                        One(const VariableLengthVector< unsigned short >&);
};
template <> class NumericTraits<VariableLengthVector<int> > {
public:
  typedef VariableLengthVector<int> ValueType;
  typedef VariableLengthVector<int> PrintType;
  typedef VariableLengthVector<unsigned int> AbsType;
  typedef VariableLengthVector<long> AccumulateType;
  typedef VariableLengthVector<double> RealType;
  typedef double ScalarRealType;
  typedef VariableLengthVector<float> FloatType;
  static const VariableLengthVector<int> ITKCommon_EXPORT 
                        Zero( const VariableLengthVector< int > & );
  static const VariableLengthVector<int> ITKCommon_EXPORT 
                        One(const VariableLengthVector< int >&);
};
template <> class NumericTraits<VariableLengthVector<unsigned int> > {
public:
  typedef VariableLengthVector<unsigned int> ValueType;
  typedef VariableLengthVector<unsigned int> PrintType;
  typedef VariableLengthVector<unsigned int> AbsType;
  typedef VariableLengthVector<unsigned long> AccumulateType;
  typedef VariableLengthVector<double> RealType;
  typedef double ScalarRealType;
  typedef VariableLengthVector<float> FloatType;
  static const VariableLengthVector<unsigned int> ITKCommon_EXPORT 
                        Zero( const VariableLengthVector< unsigned int > & );
  static const VariableLengthVector<unsigned int> ITKCommon_EXPORT 
                        One(const VariableLengthVector< unsigned int >&);
};
template <> class NumericTraits<VariableLengthVector<long> > {
public:
  typedef VariableLengthVector<long> ValueType;
  typedef VariableLengthVector<long> PrintType;
  typedef VariableLengthVector<unsigned long> AbsType;
  typedef VariableLengthVector<long> AccumulateType;
  typedef VariableLengthVector<double> RealType;
  typedef double ScalarRealType;
  typedef VariableLengthVector<float> FloatType;
  static const VariableLengthVector<long> ITKCommon_EXPORT 
                        Zero( const VariableLengthVector< long > & );
  static const VariableLengthVector<long> ITKCommon_EXPORT 
                        One(const VariableLengthVector< long >&);
};
template <> class NumericTraits<VariableLengthVector<unsigned long> > {
public:
  typedef VariableLengthVector<unsigned long> ValueType;
  typedef VariableLengthVector<unsigned long> PrlongType;
  typedef VariableLengthVector<unsigned long> AbsType;
  typedef VariableLengthVector<unsigned long> AccumulateType;
  typedef VariableLengthVector<double> RealType;
  typedef VariableLengthVector<float> FloatType;
  static const VariableLengthVector<unsigned long> ITKCommon_EXPORT 
                        Zero( const VariableLengthVector< unsigned long > & );
  static const VariableLengthVector<unsigned long> ITKCommon_EXPORT 
                        One(const VariableLengthVector< unsigned long >&);
};
template <> class NumericTraits<VariableLengthVector<float> > {
public:
  typedef VariableLengthVector<float> ValueType;
  typedef VariableLengthVector<float> PrintType;
  typedef VariableLengthVector<float> AbsType;
  typedef VariableLengthVector<double> AccumulateType;
  typedef VariableLengthVector<double> RealType;
  typedef double ScalarRealType;
  typedef VariableLengthVector<float> FloatType;
  static const VariableLengthVector<float> ITKCommon_EXPORT 
                        Zero( const VariableLengthVector< float > & );
  static const VariableLengthVector<float> ITKCommon_EXPORT 
                        One(const VariableLengthVector< float >&);
};
template <> class NumericTraits<VariableLengthVector<double> > {
public:
  typedef VariableLengthVector<double> ValueType;
  typedef VariableLengthVector<double> PrintType;
  typedef VariableLengthVector<double> AbsType;
  typedef VariableLengthVector<long double> AccumulateType;
  typedef VariableLengthVector<double> RealType;
  typedef VariableLengthVector<float> FloatType;
  typedef double ScalarRealType;
  static const VariableLengthVector<double> ITKCommon_EXPORT 
                        Zero( const VariableLengthVector< double > & );
  static const VariableLengthVector<double> ITKCommon_EXPORT 
                        One(const VariableLengthVector< double >&);
};
template <> class NumericTraits<VariableLengthVector<long double> > {
public:
  typedef VariableLengthVector<long double> ValueType;
  typedef VariableLengthVector<long double> PrintType;
  typedef VariableLengthVector<long double> AbsType;
  typedef VariableLengthVector<long double> AccumulateType;
  typedef VariableLengthVector<long double> RealType;
  typedef long double ScalarRealType;
  typedef VariableLengthVector<float> FloatType;
  static const VariableLengthVector<long double> ITKCommon_EXPORT 
                        Zero( const VariableLengthVector< long double > & );
  static const VariableLengthVector<long double> ITKCommon_EXPORT 
                        One(const VariableLengthVector< long double >&);
};

} // end namespace itk

#endif // __itkNumericTraitsVariableLengthVector_h  

