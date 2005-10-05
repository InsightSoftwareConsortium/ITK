#ifndef __itkNumericTraitsCovariantVectorPixel_h
#define __itkNumericTraitsCovariantVectorPixel_h

#include "itkNumericTraits.h"
#include "itkCovariantVector.h"

// This file defines numeric traits for vector pixels types in itk
// TODO: Add doxygen tags..

namespace itk
{
template <> class NumericTraits<CovariantVector<unsigned char, 2 > > {
public:
  typedef CovariantVector<unsigned char, 2> ValueType;
  typedef CovariantVector<unsigned char, 2> PrintType;
  typedef CovariantVector<unsigned char, 2> AbsType;
  typedef CovariantVector<unsigned short, 2> AccumulateType;
  typedef CovariantVector<double, 2> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned char, 2> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned char, 2> ITKCommon_EXPORT One;
  static CovariantVector<unsigned char,2>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<signed char, 2> > {
public:
  typedef CovariantVector<signed char, 2> ValueType;
  typedef CovariantVector<signed char, 2> PrintType;
  typedef CovariantVector<unsigned char, 2> AbsType;
  typedef CovariantVector<short, 2> AccumulateType;
  typedef CovariantVector<double, 2> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<signed char, 2> ITKCommon_EXPORT Zero;
  static const CovariantVector<signed char, 2> ITKCommon_EXPORT One;
  static CovariantVector<signed char,2>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<char, 2> > {
public:
  typedef CovariantVector<char, 2> ValueType;
  typedef CovariantVector<char, 2> PrintType;
  typedef CovariantVector<unsigned char, 2> AbsType;
  typedef CovariantVector<short, 2> AccumulateType;
  typedef CovariantVector<double, 2> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<char, 2> ITKCommon_EXPORT Zero;
  static const CovariantVector<char, 2> ITKCommon_EXPORT One;
  static CovariantVector<char,2>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<short, 2> > {
public:
  typedef CovariantVector<short, 2> ValueType;
  typedef CovariantVector<short, 2> PrintType;
  typedef CovariantVector<unsigned short, 2> AbsType;
  typedef CovariantVector<int, 2> AccumulateType;
  typedef CovariantVector<double, 2> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<short, 2> ITKCommon_EXPORT Zero;
  static const CovariantVector<short, 2> ITKCommon_EXPORT One;
  static CovariantVector<short,2>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned short, 2> > {
public:
  typedef CovariantVector<unsigned short, 2> ValueType;
  typedef CovariantVector<unsigned short, 2> PrintType;
  typedef CovariantVector<unsigned short, 2> AbsType;
  typedef CovariantVector<unsigned int, 2> AccumulateType;
  typedef CovariantVector<double, 2> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned short, 2> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned short, 2> ITKCommon_EXPORT One;
  static CovariantVector<unsigned short,2>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<int, 2> > {
public:
  typedef CovariantVector<int, 2> ValueType;
  typedef CovariantVector<int, 2> PrintType;
  typedef CovariantVector<unsigned int, 2> AbsType;
  typedef CovariantVector<long, 2> AccumulateType;
  typedef CovariantVector<double, 2> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<int, 2> ITKCommon_EXPORT Zero;
  static const CovariantVector<int, 2> ITKCommon_EXPORT One;
  static CovariantVector<int,2>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned int, 2> > {
public:
  typedef CovariantVector<unsigned int, 2> ValueType;
  typedef CovariantVector<unsigned int, 2> PrintType;
  typedef CovariantVector<unsigned int, 2> AbsType;
  typedef CovariantVector<unsigned long, 2> AccumulateType;
  typedef CovariantVector<double, 2> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned int, 2> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned int, 2> ITKCommon_EXPORT One;
  static CovariantVector<unsigned int,2>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long, 2> > {
public:
  typedef CovariantVector<long, 2> ValueType;
  typedef CovariantVector<long, 2> PrintType;
  typedef CovariantVector<unsigned long, 2> AbsType;
  typedef CovariantVector<long, 2> AccumulateType;
  typedef CovariantVector<double, 2> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<long, 2> ITKCommon_EXPORT Zero;
  static const CovariantVector<long, 2> ITKCommon_EXPORT One;
  static CovariantVector<long,2>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned long, 2> > {
public:
  typedef CovariantVector<unsigned long, 2> ValueType;
  typedef CovariantVector<unsigned long, 2> PrlongType;
  typedef CovariantVector<unsigned long, 2> AbsType;
  typedef CovariantVector<unsigned long, 2> AccumulateType;
  typedef CovariantVector<double, 2> RealType;
  static const CovariantVector<unsigned long, 2> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned long, 2> ITKCommon_EXPORT One;
  static CovariantVector<unsigned long,2>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<float, 2> > {
public:
  typedef CovariantVector<float, 2> ValueType;
  typedef CovariantVector<float, 2> PrintType;
  typedef CovariantVector<float, 2> AbsType;
  typedef CovariantVector<double, 2> AccumulateType;
  typedef CovariantVector<double, 2> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<float, 2> ITKCommon_EXPORT Zero;
  static const CovariantVector<float, 2> ITKCommon_EXPORT One;
  static CovariantVector<float, 2>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<double, 2> > {
public:
  typedef CovariantVector<double, 2> ValueType;
  typedef CovariantVector<double, 2> PrintType;
  typedef CovariantVector<double, 2> AbsType;
  typedef CovariantVector<long double, 2> AccumulateType;
  typedef CovariantVector<double, 2> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<double, 2> ITKCommon_EXPORT Zero;
  static const CovariantVector<double, 2> ITKCommon_EXPORT One;
  static CovariantVector<double, 2>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long double, 2> > {
public:
  typedef CovariantVector<long double, 2> ValueType;
  typedef CovariantVector<long double, 2> PrintType;
  typedef CovariantVector<long double, 2> AbsType;
  typedef CovariantVector<long double, 2> AccumulateType;
  typedef CovariantVector<long double, 2> RealType;
  typedef long double ScalarRealType;
  static const CovariantVector<long double, 2> ITKCommon_EXPORT Zero;
  static const CovariantVector<long double, 2> ITKCommon_EXPORT One;
  static CovariantVector<long double, 2>  ZeroValue() { return Zero; }
};

template <> class NumericTraits<CovariantVector<unsigned char, 3 > > {
public:
  typedef CovariantVector<unsigned char, 3> ValueType;
  typedef CovariantVector<unsigned char, 3> PrintType;
  typedef CovariantVector<unsigned char, 3> AbsType;
  typedef CovariantVector<unsigned short, 3> AccumulateType;
  typedef CovariantVector<double, 3> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned char, 3> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned char, 3> ITKCommon_EXPORT One;
  static CovariantVector<unsigned char, 3>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<signed char, 3> > {
public:
  typedef CovariantVector<signed char, 3> ValueType;
  typedef CovariantVector<signed char, 3> PrintType;
  typedef CovariantVector<unsigned char, 3> AbsType;
  typedef CovariantVector<short, 3> AccumulateType;
  typedef CovariantVector<double, 3> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<signed char, 3> ITKCommon_EXPORT Zero;
  static const CovariantVector<signed char, 3> ITKCommon_EXPORT One;
  static CovariantVector<signed char, 3>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<char, 3> > {
public:
  typedef CovariantVector<char, 3> ValueType;
  typedef CovariantVector<char, 3> PrintType;
  typedef CovariantVector<unsigned char, 3> AbsType;
  typedef CovariantVector<short, 3> AccumulateType;
  typedef CovariantVector<double, 3> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<char, 3> ITKCommon_EXPORT Zero;
  static const CovariantVector<char, 3> ITKCommon_EXPORT One;
  static CovariantVector<char, 3>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<short, 3> > {
public:
  typedef CovariantVector<short, 3> ValueType;
  typedef CovariantVector<short, 3> PrintType;
  typedef CovariantVector<unsigned short, 3> AbsType;
  typedef CovariantVector<int, 3> AccumulateType;
  typedef CovariantVector<double, 3> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<short, 3> ITKCommon_EXPORT Zero;
  static const CovariantVector<short, 3> ITKCommon_EXPORT One;
  static CovariantVector<short, 3>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned short, 3> > {
public:
  typedef CovariantVector<unsigned short, 3> ValueType;
  typedef CovariantVector<unsigned short, 3> PrintType;
  typedef CovariantVector<unsigned short, 3> AbsType;
  typedef CovariantVector<unsigned int, 3> AccumulateType;
  typedef CovariantVector<double, 3> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned short, 3> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned short, 3> ITKCommon_EXPORT One;
  static CovariantVector<unsigned short, 3>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<int, 3> > {
public:
  typedef CovariantVector<int, 3> ValueType;
  typedef CovariantVector<int, 3> PrintType;
  typedef CovariantVector<unsigned int, 3> AbsType;
  typedef CovariantVector<long, 3> AccumulateType;
  typedef CovariantVector<double, 3> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<int, 3> ITKCommon_EXPORT Zero;
  static const CovariantVector<int, 3> ITKCommon_EXPORT One;
  static CovariantVector<int, 3>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned int, 3> > {
public:
  typedef CovariantVector<unsigned int, 3> ValueType;
  typedef CovariantVector<unsigned int, 3> PrintType;
  typedef CovariantVector<unsigned int, 3> AbsType;
  typedef CovariantVector<unsigned long, 3> AccumulateType;
  typedef CovariantVector<double, 3> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned int, 3> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned int, 3> ITKCommon_EXPORT One;
  static CovariantVector<unsigned int, 3>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long, 3> > {
public:
  typedef CovariantVector<long, 3> ValueType;
  typedef CovariantVector<long, 3> PrintType;
  typedef CovariantVector<unsigned long, 3> AbsType;
  typedef CovariantVector<long, 3> AccumulateType;
  typedef CovariantVector<double, 3> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<long, 3> ITKCommon_EXPORT Zero;
  static const CovariantVector<long, 3> ITKCommon_EXPORT One;
  static CovariantVector<long, 3>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned long, 3> > {
public:
  typedef CovariantVector<unsigned long, 3> ValueType;
  typedef CovariantVector<unsigned long, 3> PrlongType;
  typedef CovariantVector<unsigned long, 3> AbsType;
  typedef CovariantVector<unsigned long, 3> AccumulateType;
  typedef CovariantVector<double, 3> RealType;
  static const CovariantVector<unsigned long, 3> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned long, 3> ITKCommon_EXPORT One;
  static CovariantVector<unsigned long, 3>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<float, 3> > {
public:
  typedef CovariantVector<float, 3> ValueType;
  typedef CovariantVector<float, 3> PrintType;
  typedef CovariantVector<float, 3> AbsType;
  typedef CovariantVector<double, 3> AccumulateType;
  typedef CovariantVector<double, 3> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<float, 3> ITKCommon_EXPORT Zero;
  static const CovariantVector<float, 3> ITKCommon_EXPORT One;
  static CovariantVector<float, 3>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<double, 3> > {
public:
  typedef CovariantVector<double, 3> ValueType;
  typedef CovariantVector<double, 3> PrintType;
  typedef CovariantVector<double, 3> AbsType;
  typedef CovariantVector<long double, 3> AccumulateType;
  typedef CovariantVector<double, 3> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<double, 3> ITKCommon_EXPORT Zero;
  static const CovariantVector<double, 3> ITKCommon_EXPORT One;
  static CovariantVector<double, 3>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long double, 3> > {
public:
  typedef CovariantVector<long double, 3> ValueType;
  typedef CovariantVector<long double, 3> PrintType;
  typedef CovariantVector<long double, 3> AbsType;
  typedef CovariantVector<long double, 3> AccumulateType;
  typedef CovariantVector<long double, 3> RealType;
  typedef long double ScalarRealType;
  static const CovariantVector<long double, 3> ITKCommon_EXPORT Zero;
  static const CovariantVector<long double, 3> ITKCommon_EXPORT One;
  static CovariantVector<long double, 3>  ZeroValue() { return Zero; }
};

template <> class NumericTraits<CovariantVector<unsigned char, 4 > > {
public:
  typedef CovariantVector<unsigned char, 4> ValueType;
  typedef CovariantVector<unsigned char, 4> PrintType;
  typedef CovariantVector<unsigned char, 4> AbsType;
  typedef CovariantVector<unsigned short, 4> AccumulateType;
  typedef CovariantVector<double, 4> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned char, 4> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned char, 4> ITKCommon_EXPORT One;
  static CovariantVector<unsigned char, 4>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<signed char, 4> > {
public:
  typedef CovariantVector<signed char, 4> ValueType;
  typedef CovariantVector<signed char, 4> PrintType;
  typedef CovariantVector<unsigned char, 4> AbsType;
  typedef CovariantVector<short, 4> AccumulateType;
  typedef CovariantVector<double, 4> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<signed char, 4> ITKCommon_EXPORT Zero;
  static const CovariantVector<signed char, 4> ITKCommon_EXPORT One;
  static CovariantVector<signed char, 4>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<char, 4> > {
public:
  typedef CovariantVector<char, 4> ValueType;
  typedef CovariantVector<char, 4> PrintType;
  typedef CovariantVector<unsigned char, 4> AbsType;
  typedef CovariantVector<short, 4> AccumulateType;
  typedef CovariantVector<double, 4> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<char, 4> ITKCommon_EXPORT Zero;
  static const CovariantVector<char, 4> ITKCommon_EXPORT One;
  static CovariantVector<char, 4>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<short, 4> > {
public:
  typedef CovariantVector<short, 4> ValueType;
  typedef CovariantVector<short, 4> PrintType;
  typedef CovariantVector<unsigned short, 4> AbsType;
  typedef CovariantVector<int, 4> AccumulateType;
  typedef CovariantVector<double, 4> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<short, 4> ITKCommon_EXPORT Zero;
  static const CovariantVector<short, 4> ITKCommon_EXPORT One;
  static CovariantVector<short, 4>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned short, 4> > {
public:
  typedef CovariantVector<unsigned short, 4> ValueType;
  typedef CovariantVector<unsigned short, 4> PrintType;
  typedef CovariantVector<unsigned short, 4> AbsType;
  typedef CovariantVector<unsigned int, 4> AccumulateType;
  typedef CovariantVector<double, 4> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned short, 4> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned short, 4> ITKCommon_EXPORT One;
  static CovariantVector<unsigned short, 4>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<int, 4> > {
public:
  typedef CovariantVector<int, 4> ValueType;
  typedef CovariantVector<int, 4> PrintType;
  typedef CovariantVector<unsigned int, 4> AbsType;
  typedef CovariantVector<long, 4> AccumulateType;
  typedef CovariantVector<double, 4> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<int, 4> ITKCommon_EXPORT Zero;
  static const CovariantVector<int, 4> ITKCommon_EXPORT One;
  static CovariantVector<int, 4>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned int, 4> > {
public:
  typedef CovariantVector<unsigned int, 4> ValueType;
  typedef CovariantVector<unsigned int, 4> PrintType;
  typedef CovariantVector<unsigned int, 4> AbsType;
  typedef CovariantVector<unsigned long, 4> AccumulateType;
  typedef CovariantVector<double, 4> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned int, 4> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned int, 4> ITKCommon_EXPORT One;
  static CovariantVector<unsigned int, 4>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long, 4> > {
public:
  typedef CovariantVector<long, 4> ValueType;
  typedef CovariantVector<long, 4> PrintType;
  typedef CovariantVector<unsigned long, 4> AbsType;
  typedef CovariantVector<long, 4> AccumulateType;
  typedef CovariantVector<double, 4> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<long, 4> ITKCommon_EXPORT Zero;
  static const CovariantVector<long, 4> ITKCommon_EXPORT One;
  static CovariantVector<long, 4>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned long, 4> > {
public:
  typedef CovariantVector<unsigned long, 4> ValueType;
  typedef CovariantVector<unsigned long, 4> PrlongType;
  typedef CovariantVector<unsigned long, 4> AbsType;
  typedef CovariantVector<unsigned long, 4> AccumulateType;
  typedef CovariantVector<double, 4> RealType;
  static const CovariantVector<unsigned long, 4> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned long, 4> ITKCommon_EXPORT One;
  static CovariantVector<unsigned long, 4>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<float, 4> > {
public:
  typedef CovariantVector<float, 4> ValueType;
  typedef CovariantVector<float, 4> PrintType;
  typedef CovariantVector<float, 4> AbsType;
  typedef CovariantVector<double, 4> AccumulateType;
  typedef CovariantVector<double, 4> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<float, 4> ITKCommon_EXPORT Zero;
  static const CovariantVector<float, 4> ITKCommon_EXPORT One;
  static CovariantVector<float, 4>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<double, 4> > {
public:
  typedef CovariantVector<double, 4> ValueType;
  typedef CovariantVector<double, 4> PrintType;
  typedef CovariantVector<double, 4> AbsType;
  typedef CovariantVector<long double, 4> AccumulateType;
  typedef CovariantVector<double, 4> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<double, 4> ITKCommon_EXPORT Zero;
  static const CovariantVector<double, 4> ITKCommon_EXPORT One;
  static CovariantVector<double, 4>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long double, 4> > {
public:
  typedef CovariantVector<long double, 4> ValueType;
  typedef CovariantVector<long double, 4> PrintType;
  typedef CovariantVector<long double, 4> AbsType;
  typedef CovariantVector<long double, 4> AccumulateType;
  typedef CovariantVector<long double, 4> RealType;
  typedef long double ScalarRealType;
  static const CovariantVector<long double, 4> ITKCommon_EXPORT Zero;
  static const CovariantVector<long double, 4> ITKCommon_EXPORT One;
  static CovariantVector<long double, 4>  ZeroValue() { return Zero; }
};

template <> class NumericTraits<CovariantVector<unsigned char, 5 > > {
public:
  typedef CovariantVector<unsigned char, 5> ValueType;
  typedef CovariantVector<unsigned char, 5> PrintType;
  typedef CovariantVector<unsigned char, 5> AbsType;
  typedef CovariantVector<unsigned short, 5> AccumulateType;
  typedef CovariantVector<double, 5> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned char, 5> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned char, 5> ITKCommon_EXPORT One;
  static CovariantVector<unsigned char, 5>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<signed char, 5> > {
public:
  typedef CovariantVector<signed char, 5> ValueType;
  typedef CovariantVector<signed char, 5> PrintType;
  typedef CovariantVector<unsigned char, 5> AbsType;
  typedef CovariantVector<short, 5> AccumulateType;
  typedef CovariantVector<double, 5> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<signed char, 5> ITKCommon_EXPORT Zero;
  static const CovariantVector<signed char, 5> ITKCommon_EXPORT One;
  static CovariantVector<signed char, 5>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<char, 5> > {
public:
  typedef CovariantVector<char, 5> ValueType;
  typedef CovariantVector<char, 5> PrintType;
  typedef CovariantVector<unsigned char, 5> AbsType;
  typedef CovariantVector<short, 5> AccumulateType;
  typedef CovariantVector<double, 5> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<char, 5> ITKCommon_EXPORT Zero;
  static const CovariantVector<char, 5> ITKCommon_EXPORT One;
  static CovariantVector<char, 5>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<short, 5> > {
public:
  typedef CovariantVector<short, 5> ValueType;
  typedef CovariantVector<short, 5> PrintType;
  typedef CovariantVector<unsigned short, 5> AbsType;
  typedef CovariantVector<int, 5> AccumulateType;
  typedef CovariantVector<double, 5> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<short, 5> ITKCommon_EXPORT Zero;
  static const CovariantVector<short, 5> ITKCommon_EXPORT One;
  static CovariantVector<short, 5>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned short, 5> > {
public:
  typedef CovariantVector<unsigned short, 5> ValueType;
  typedef CovariantVector<unsigned short, 5> PrintType;
  typedef CovariantVector<unsigned short, 5> AbsType;
  typedef CovariantVector<unsigned int, 5> AccumulateType;
  typedef CovariantVector<double, 5> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned short, 5> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned short, 5> ITKCommon_EXPORT One;
  static CovariantVector<unsigned short, 5>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<int, 5> > {
public:
  typedef CovariantVector<int, 5> ValueType;
  typedef CovariantVector<int, 5> PrintType;
  typedef CovariantVector<unsigned int, 5> AbsType;
  typedef CovariantVector<long, 5> AccumulateType;
  typedef CovariantVector<double, 5> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<int, 5> ITKCommon_EXPORT Zero;
  static const CovariantVector<int, 5> ITKCommon_EXPORT One;
  static CovariantVector<int, 5>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned int, 5> > {
public:
  typedef CovariantVector<unsigned int, 5> ValueType;
  typedef CovariantVector<unsigned int, 5> PrintType;
  typedef CovariantVector<unsigned int, 5> AbsType;
  typedef CovariantVector<unsigned long, 5> AccumulateType;
  typedef CovariantVector<double, 5> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned int, 5> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned int, 5> ITKCommon_EXPORT One;
  static CovariantVector<unsigned int, 5>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long, 5> > {
public:
  typedef CovariantVector<long, 5> ValueType;
  typedef CovariantVector<long, 5> PrintType;
  typedef CovariantVector<unsigned long, 5> AbsType;
  typedef CovariantVector<long, 5> AccumulateType;
  typedef CovariantVector<double, 5> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<long, 5> ITKCommon_EXPORT Zero;
  static const CovariantVector<long, 5> ITKCommon_EXPORT One;
  static CovariantVector<long, 5>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned long, 5> > {
public:
  typedef CovariantVector<unsigned long, 5> ValueType;
  typedef CovariantVector<unsigned long, 5> PrlongType;
  typedef CovariantVector<unsigned long, 5> AbsType;
  typedef CovariantVector<unsigned long, 5> AccumulateType;
  typedef CovariantVector<double, 5> RealType;
  static const CovariantVector<unsigned long, 5> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned long, 5> ITKCommon_EXPORT One;
  static CovariantVector<unsigned long, 5>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<float, 5> > {
public:
  typedef CovariantVector<float, 5> ValueType;
  typedef CovariantVector<float, 5> PrintType;
  typedef CovariantVector<float, 5> AbsType;
  typedef CovariantVector<double, 5> AccumulateType;
  typedef CovariantVector<double, 5> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<float, 5> ITKCommon_EXPORT Zero;
  static const CovariantVector<float, 5> ITKCommon_EXPORT One;
  static CovariantVector<float, 5>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<double, 5> > {
public:
  typedef CovariantVector<double, 5> ValueType;
  typedef CovariantVector<double, 5> PrintType;
  typedef CovariantVector<double, 5> AbsType;
  typedef CovariantVector<long double, 5> AccumulateType;
  typedef CovariantVector<double, 5> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<double, 5> ITKCommon_EXPORT Zero;
  static const CovariantVector<double, 5> ITKCommon_EXPORT One;
  static CovariantVector<double, 5>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long double, 5> > {
public:
  typedef CovariantVector<long double, 5> ValueType;
  typedef CovariantVector<long double, 5> PrintType;
  typedef CovariantVector<long double, 5> AbsType;
  typedef CovariantVector<long double, 5> AccumulateType;
  typedef CovariantVector<long double, 5> RealType;
  typedef long double ScalarRealType;
  static const CovariantVector<long double, 5> ITKCommon_EXPORT Zero;
  static const CovariantVector<long double, 5> ITKCommon_EXPORT One;
  static CovariantVector<long double, 5>  ZeroValue() { return Zero; }
};

template <> class NumericTraits<CovariantVector<unsigned char, 6 > > {
public:
  typedef CovariantVector<unsigned char, 6> ValueType;
  typedef CovariantVector<unsigned char, 6> PrintType;
  typedef CovariantVector<unsigned char, 6> AbsType;
  typedef CovariantVector<unsigned short, 6> AccumulateType;
  typedef CovariantVector<double, 6> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned char, 6> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned char, 6> ITKCommon_EXPORT One;
  static CovariantVector<unsigned char, 6>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<signed char, 6> > {
public:
  typedef CovariantVector<signed char, 6> ValueType;
  typedef CovariantVector<signed char, 6> PrintType;
  typedef CovariantVector<unsigned char, 6> AbsType;
  typedef CovariantVector<short, 6> AccumulateType;
  typedef CovariantVector<double, 6> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<signed char, 6> ITKCommon_EXPORT Zero;
  static const CovariantVector<signed char, 6> ITKCommon_EXPORT One;
  static CovariantVector<signed char, 6>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<char, 6> > {
public:
  typedef CovariantVector<char, 6> ValueType;
  typedef CovariantVector<char, 6> PrintType;
  typedef CovariantVector<unsigned char, 6> AbsType;
  typedef CovariantVector<short, 6> AccumulateType;
  typedef CovariantVector<double, 6> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<char, 6> ITKCommon_EXPORT Zero;
  static const CovariantVector<char, 6> ITKCommon_EXPORT One;
  static CovariantVector<char, 6>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<short, 6> > {
public:
  typedef CovariantVector<short, 6> ValueType;
  typedef CovariantVector<short, 6> PrintType;
  typedef CovariantVector<unsigned short, 6> AbsType;
  typedef CovariantVector<int, 6> AccumulateType;
  typedef CovariantVector<double, 6> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<short, 6> ITKCommon_EXPORT Zero;
  static const CovariantVector<short, 6> ITKCommon_EXPORT One;
  static CovariantVector<short, 6>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned short, 6> > {
public:
  typedef CovariantVector<unsigned short, 6> ValueType;
  typedef CovariantVector<unsigned short, 6> PrintType;
  typedef CovariantVector<unsigned short, 6> AbsType;
  typedef CovariantVector<unsigned int, 6> AccumulateType;
  typedef CovariantVector<double, 6> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned short, 6> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned short, 6> ITKCommon_EXPORT One;
  static CovariantVector<unsigned short, 6>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<int, 6> > {
public:
  typedef CovariantVector<int, 6> ValueType;
  typedef CovariantVector<int, 6> PrintType;
  typedef CovariantVector<unsigned int, 6> AbsType;
  typedef CovariantVector<long, 6> AccumulateType;
  typedef CovariantVector<double, 6> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<int, 6> ITKCommon_EXPORT Zero;
  static const CovariantVector<int, 6> ITKCommon_EXPORT One;
  static CovariantVector<int, 6>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned int, 6> > {
public:
  typedef CovariantVector<unsigned int, 6> ValueType;
  typedef CovariantVector<unsigned int, 6> PrintType;
  typedef CovariantVector<unsigned int, 6> AbsType;
  typedef CovariantVector<unsigned long, 6> AccumulateType;
  typedef CovariantVector<double, 6> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned int, 6> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned int, 6> ITKCommon_EXPORT One;
  static CovariantVector<unsigned int, 6>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long, 6> > {
public:
  typedef CovariantVector<long, 6> ValueType;
  typedef CovariantVector<long, 6> PrintType;
  typedef CovariantVector<unsigned long, 6> AbsType;
  typedef CovariantVector<long, 6> AccumulateType;
  typedef CovariantVector<double, 6> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<long, 6> ITKCommon_EXPORT Zero;
  static const CovariantVector<long, 6> ITKCommon_EXPORT One;
  static CovariantVector<long, 6>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned long, 6> > {
public:
  typedef CovariantVector<unsigned long, 6> ValueType;
  typedef CovariantVector<unsigned long, 6> PrlongType;
  typedef CovariantVector<unsigned long, 6> AbsType;
  typedef CovariantVector<unsigned long, 6> AccumulateType;
  typedef CovariantVector<double, 6> RealType;
  static const CovariantVector<unsigned long, 6> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned long, 6> ITKCommon_EXPORT One;
  static CovariantVector<unsigned long, 6>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<float, 6> > {
public:
  typedef CovariantVector<float, 6> ValueType;
  typedef CovariantVector<float, 6> PrintType;
  typedef CovariantVector<float, 6> AbsType;
  typedef CovariantVector<double, 6> AccumulateType;
  typedef CovariantVector<double, 6> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<float, 6> ITKCommon_EXPORT Zero;
  static const CovariantVector<float, 6> ITKCommon_EXPORT One;
  static CovariantVector<float, 6>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<double, 6> > {
public:
  typedef CovariantVector<double, 6> ValueType;
  typedef CovariantVector<double, 6> PrintType;
  typedef CovariantVector<double, 6> AbsType;
  typedef CovariantVector<long double, 6> AccumulateType;
  typedef CovariantVector<double, 6> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<double, 6> ITKCommon_EXPORT Zero;
  static const CovariantVector<double, 6> ITKCommon_EXPORT One;
  static CovariantVector<double, 6>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long double, 6> > {
public:
  typedef CovariantVector<long double, 6> ValueType;
  typedef CovariantVector<long double, 6> PrintType;
  typedef CovariantVector<long double, 6> AbsType;
  typedef CovariantVector<long double, 6> AccumulateType;
  typedef CovariantVector<long double, 6> RealType;
  typedef long double ScalarRealType;
  static const CovariantVector<long double, 6> ITKCommon_EXPORT Zero;
  static const CovariantVector<long double, 6> ITKCommon_EXPORT One;
  static CovariantVector<long double, 6>  ZeroValue() { return Zero; }
};

template <> class NumericTraits<CovariantVector<unsigned char, 7 > > {
public:
  typedef CovariantVector<unsigned char, 7> ValueType;
  typedef CovariantVector<unsigned char, 7> PrintType;
  typedef CovariantVector<unsigned char, 7> AbsType;
  typedef CovariantVector<unsigned short, 7> AccumulateType;
  typedef CovariantVector<double, 7> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned char, 7> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned char, 7> ITKCommon_EXPORT One;
  static CovariantVector<unsigned char, 7>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<signed char, 7> > {
public:
  typedef CovariantVector<signed char, 7> ValueType;
  typedef CovariantVector<signed char, 7> PrintType;
  typedef CovariantVector<unsigned char, 7> AbsType;
  typedef CovariantVector<short, 7> AccumulateType;
  typedef CovariantVector<double, 7> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<signed char, 7> ITKCommon_EXPORT Zero;
  static const CovariantVector<signed char, 7> ITKCommon_EXPORT One;
  static CovariantVector<signed char, 7>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<char, 7> > {
public:
  typedef CovariantVector<char, 7> ValueType;
  typedef CovariantVector<char, 7> PrintType;
  typedef CovariantVector<unsigned char, 7> AbsType;
  typedef CovariantVector<short, 7> AccumulateType;
  typedef CovariantVector<double, 7> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<char, 7> ITKCommon_EXPORT Zero;
  static const CovariantVector<char, 7> ITKCommon_EXPORT One;
  static CovariantVector<char, 7>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<short, 7> > {
public:
  typedef CovariantVector<short, 7> ValueType;
  typedef CovariantVector<short, 7> PrintType;
  typedef CovariantVector<unsigned short, 7> AbsType;
  typedef CovariantVector<int, 7> AccumulateType;
  typedef CovariantVector<double, 7> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<short, 7> ITKCommon_EXPORT Zero;
  static const CovariantVector<short, 7> ITKCommon_EXPORT One;
  static CovariantVector<short, 7>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned short, 7> > {
public:
  typedef CovariantVector<unsigned short, 7> ValueType;
  typedef CovariantVector<unsigned short, 7> PrintType;
  typedef CovariantVector<unsigned short, 7> AbsType;
  typedef CovariantVector<unsigned int, 7> AccumulateType;
  typedef CovariantVector<double, 7> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned short, 7> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned short, 7> ITKCommon_EXPORT One;
  static CovariantVector<unsigned short, 7>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<int, 7> > {
public:
  typedef CovariantVector<int, 7> ValueType;
  typedef CovariantVector<int, 7> PrintType;
  typedef CovariantVector<unsigned int, 7> AbsType;
  typedef CovariantVector<long, 7> AccumulateType;
  typedef CovariantVector<double, 7> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<int, 7> ITKCommon_EXPORT Zero;
  static const CovariantVector<int, 7> ITKCommon_EXPORT One;
  static CovariantVector<int, 7>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned int, 7> > {
public:
  typedef CovariantVector<unsigned int, 7> ValueType;
  typedef CovariantVector<unsigned int, 7> PrintType;
  typedef CovariantVector<unsigned int, 7> AbsType;
  typedef CovariantVector<unsigned long, 7> AccumulateType;
  typedef CovariantVector<double, 7> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned int, 7> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned int, 7> ITKCommon_EXPORT One;
  static CovariantVector<unsigned int, 7>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long, 7> > {
public:
  typedef CovariantVector<long, 7> ValueType;
  typedef CovariantVector<long, 7> PrintType;
  typedef CovariantVector<unsigned long, 7> AbsType;
  typedef CovariantVector<long, 7> AccumulateType;
  typedef CovariantVector<double, 7> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<long, 7> ITKCommon_EXPORT Zero;
  static const CovariantVector<long, 7> ITKCommon_EXPORT One;
  static CovariantVector<long, 7>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned long, 7> > {
public:
  typedef CovariantVector<unsigned long, 7> ValueType;
  typedef CovariantVector<unsigned long, 7> PrlongType;
  typedef CovariantVector<unsigned long, 7> AbsType;
  typedef CovariantVector<unsigned long, 7> AccumulateType;
  typedef CovariantVector<double, 7> RealType;
  static const CovariantVector<unsigned long, 7> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned long, 7> ITKCommon_EXPORT One;
  static CovariantVector<unsigned long, 7>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<float, 7> > {
public:
  typedef CovariantVector<float, 7> ValueType;
  typedef CovariantVector<float, 7> PrintType;
  typedef CovariantVector<float, 7> AbsType;
  typedef CovariantVector<double, 7> AccumulateType;
  typedef CovariantVector<double, 7> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<float, 7> ITKCommon_EXPORT Zero;
  static const CovariantVector<float, 7> ITKCommon_EXPORT One;
  static CovariantVector<float, 7>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<double, 7> > {
public:
  typedef CovariantVector<double, 7> ValueType;
  typedef CovariantVector<double, 7> PrintType;
  typedef CovariantVector<double, 7> AbsType;
  typedef CovariantVector<long double, 7> AccumulateType;
  typedef CovariantVector<double, 7> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<double, 7> ITKCommon_EXPORT Zero;
  static const CovariantVector<double, 7> ITKCommon_EXPORT One;
  static CovariantVector<double, 7>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long double, 7> > {
public:
  typedef CovariantVector<long double, 7> ValueType;
  typedef CovariantVector<long double, 7> PrintType;
  typedef CovariantVector<long double, 7> AbsType;
  typedef CovariantVector<long double, 7> AccumulateType;
  typedef CovariantVector<long double, 7> RealType;
  typedef long double ScalarRealType;
  static const CovariantVector<long double, 7> ITKCommon_EXPORT Zero;
  static const CovariantVector<long double, 7> ITKCommon_EXPORT One;
  static CovariantVector<long double, 7>  ZeroValue() { return Zero; }
};

template <> class NumericTraits<CovariantVector<unsigned char, 8 > > {
public:
  typedef CovariantVector<unsigned char, 8> ValueType;
  typedef CovariantVector<unsigned char, 8> PrintType;
  typedef CovariantVector<unsigned char, 8> AbsType;
  typedef CovariantVector<unsigned short, 8> AccumulateType;
  typedef CovariantVector<double, 8> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned char, 8> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned char, 8> ITKCommon_EXPORT One;
  static CovariantVector<unsigned char, 8>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<signed char, 8> > {
public:
  typedef CovariantVector<signed char, 8> ValueType;
  typedef CovariantVector<signed char, 8> PrintType;
  typedef CovariantVector<unsigned char, 8> AbsType;
  typedef CovariantVector<short, 8> AccumulateType;
  typedef CovariantVector<double, 8> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<signed char, 8> ITKCommon_EXPORT Zero;
  static const CovariantVector<signed char, 8> ITKCommon_EXPORT One;
  static CovariantVector<signed char, 8>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<char, 8> > {
public:
  typedef CovariantVector<char, 8> ValueType;
  typedef CovariantVector<char, 8> PrintType;
  typedef CovariantVector<unsigned char, 8> AbsType;
  typedef CovariantVector<short, 8> AccumulateType;
  typedef CovariantVector<double, 8> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<char, 8> ITKCommon_EXPORT Zero;
  static const CovariantVector<char, 8> ITKCommon_EXPORT One;
  static CovariantVector<char, 8>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<short, 8> > {
public:
  typedef CovariantVector<short, 8> ValueType;
  typedef CovariantVector<short, 8> PrintType;
  typedef CovariantVector<unsigned short, 8> AbsType;
  typedef CovariantVector<int, 8> AccumulateType;
  typedef CovariantVector<double, 8> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<short, 8> ITKCommon_EXPORT Zero;
  static const CovariantVector<short, 8> ITKCommon_EXPORT One;
  static CovariantVector<short, 8>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned short, 8> > {
public:
  typedef CovariantVector<unsigned short, 8> ValueType;
  typedef CovariantVector<unsigned short, 8> PrintType;
  typedef CovariantVector<unsigned short, 8> AbsType;
  typedef CovariantVector<unsigned int, 8> AccumulateType;
  typedef CovariantVector<double, 8> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned short, 8> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned short, 8> ITKCommon_EXPORT One;
  static CovariantVector<unsigned short, 8>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<int, 8> > {
public:
  typedef CovariantVector<int, 8> ValueType;
  typedef CovariantVector<int, 8> PrintType;
  typedef CovariantVector<unsigned int, 8> AbsType;
  typedef CovariantVector<long, 8> AccumulateType;
  typedef CovariantVector<double, 8> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<int, 8> ITKCommon_EXPORT Zero;
  static const CovariantVector<int, 8> ITKCommon_EXPORT One;
  static CovariantVector<int, 8>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned int, 8> > {
public:
  typedef CovariantVector<unsigned int, 8> ValueType;
  typedef CovariantVector<unsigned int, 8> PrintType;
  typedef CovariantVector<unsigned int, 8> AbsType;
  typedef CovariantVector<unsigned long, 8> AccumulateType;
  typedef CovariantVector<double, 8> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned int, 8> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned int, 8> ITKCommon_EXPORT One;
  static CovariantVector<unsigned int, 8>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long, 8> > {
public:
  typedef CovariantVector<long, 8> ValueType;
  typedef CovariantVector<long, 8> PrintType;
  typedef CovariantVector<unsigned long, 8> AbsType;
  typedef CovariantVector<long, 8> AccumulateType;
  typedef CovariantVector<double, 8> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<long, 8> ITKCommon_EXPORT Zero;
  static const CovariantVector<long, 8> ITKCommon_EXPORT One;
  static CovariantVector<long, 8>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned long, 8> > {
public:
  typedef CovariantVector<unsigned long, 8> ValueType;
  typedef CovariantVector<unsigned long, 8> PrlongType;
  typedef CovariantVector<unsigned long, 8> AbsType;
  typedef CovariantVector<unsigned long, 8> AccumulateType;
  typedef CovariantVector<double, 8> RealType;
  static const CovariantVector<unsigned long, 8> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned long, 8> ITKCommon_EXPORT One;
  static CovariantVector<unsigned long, 8>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<float, 8> > {
public:
  typedef CovariantVector<float, 8> ValueType;
  typedef CovariantVector<float, 8> PrintType;
  typedef CovariantVector<float, 8> AbsType;
  typedef CovariantVector<double, 8> AccumulateType;
  typedef CovariantVector<double, 8> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<float, 8> ITKCommon_EXPORT Zero;
  static const CovariantVector<float, 8> ITKCommon_EXPORT One;
  static CovariantVector<float, 8>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<double, 8> > {
public:
  typedef CovariantVector<double, 8> ValueType;
  typedef CovariantVector<double, 8> PrintType;
  typedef CovariantVector<double, 8> AbsType;
  typedef CovariantVector<long double, 8> AccumulateType;
  typedef CovariantVector<double, 8> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<double, 8> ITKCommon_EXPORT Zero;
  static const CovariantVector<double, 8> ITKCommon_EXPORT One;
  static CovariantVector<double, 8>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long double, 8> > {
public:
  typedef CovariantVector<long double, 8> ValueType;
  typedef CovariantVector<long double, 8> PrintType;
  typedef CovariantVector<long double, 8> AbsType;
  typedef CovariantVector<long double, 8> AccumulateType;
  typedef CovariantVector<long double, 8> RealType;
  typedef long double ScalarRealType;
  static const CovariantVector<long double, 8> ITKCommon_EXPORT Zero;
  static const CovariantVector<long double, 8> ITKCommon_EXPORT One;
  static CovariantVector<long double, 8>  ZeroValue() { return Zero; }
};

template <> class NumericTraits<CovariantVector<unsigned char, 9 > > {
public:
  typedef CovariantVector<unsigned char, 9> ValueType;
  typedef CovariantVector<unsigned char, 9> PrintType;
  typedef CovariantVector<unsigned char, 9> AbsType;
  typedef CovariantVector<unsigned short, 9> AccumulateType;
  typedef CovariantVector<double, 9> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned char, 9> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned char, 9> ITKCommon_EXPORT One;
  static CovariantVector<unsigned char, 9>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<signed char, 9> > {
public:
  typedef CovariantVector<signed char, 9> ValueType;
  typedef CovariantVector<signed char, 9> PrintType;
  typedef CovariantVector<unsigned char, 9> AbsType;
  typedef CovariantVector<short, 9> AccumulateType;
  typedef CovariantVector<double, 9> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<signed char, 9> ITKCommon_EXPORT Zero;
  static const CovariantVector<signed char, 9> ITKCommon_EXPORT One;
  static CovariantVector<signed char, 9>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<char, 9> > {
public:
  typedef CovariantVector<char, 9> ValueType;
  typedef CovariantVector<char, 9> PrintType;
  typedef CovariantVector<unsigned char, 9> AbsType;
  typedef CovariantVector<short, 9> AccumulateType;
  typedef CovariantVector<double, 9> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<char, 9> ITKCommon_EXPORT Zero;
  static const CovariantVector<char, 9> ITKCommon_EXPORT One;
  static CovariantVector<char, 9>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<short, 9> > {
public:
  typedef CovariantVector<short, 9> ValueType;
  typedef CovariantVector<short, 9> PrintType;
  typedef CovariantVector<unsigned short, 9> AbsType;
  typedef CovariantVector<int, 9> AccumulateType;
  typedef CovariantVector<double, 9> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<short, 9> ITKCommon_EXPORT Zero;
  static const CovariantVector<short, 9> ITKCommon_EXPORT One;
  static CovariantVector<short, 9>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned short, 9> > {
public:
  typedef CovariantVector<unsigned short, 9> ValueType;
  typedef CovariantVector<unsigned short, 9> PrintType;
  typedef CovariantVector<unsigned short, 9> AbsType;
  typedef CovariantVector<unsigned int, 9> AccumulateType;
  typedef CovariantVector<double, 9> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned short, 9> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned short, 9> ITKCommon_EXPORT One;
  static CovariantVector<unsigned short, 9>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<int, 9> > {
public:
  typedef CovariantVector<int, 9> ValueType;
  typedef CovariantVector<int, 9> PrintType;
  typedef CovariantVector<unsigned int, 9> AbsType;
  typedef CovariantVector<long, 9> AccumulateType;
  typedef CovariantVector<double, 9> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<int, 9> ITKCommon_EXPORT Zero;
  static const CovariantVector<int, 9> ITKCommon_EXPORT One;
  static CovariantVector<int, 9>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned int, 9> > {
public:
  typedef CovariantVector<unsigned int, 9> ValueType;
  typedef CovariantVector<unsigned int, 9> PrintType;
  typedef CovariantVector<unsigned int, 9> AbsType;
  typedef CovariantVector<unsigned long, 9> AccumulateType;
  typedef CovariantVector<double, 9> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<unsigned int, 9> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned int, 9> ITKCommon_EXPORT One;
  static CovariantVector<unsigned int, 9>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long, 9> > {
public:
  typedef CovariantVector<long, 9> ValueType;
  typedef CovariantVector<long, 9> PrintType;
  typedef CovariantVector<unsigned long, 9> AbsType;
  typedef CovariantVector<long, 9> AccumulateType;
  typedef CovariantVector<double, 9> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<long, 9> ITKCommon_EXPORT Zero;
  static const CovariantVector<long, 9> ITKCommon_EXPORT One;
  static CovariantVector<long, 9>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<unsigned long, 9> > {
public:
  typedef CovariantVector<unsigned long, 9> ValueType;
  typedef CovariantVector<unsigned long, 9> PrlongType;
  typedef CovariantVector<unsigned long, 9> AbsType;
  typedef CovariantVector<unsigned long, 9> AccumulateType;
  typedef CovariantVector<double, 9> RealType;
  static const CovariantVector<unsigned long, 9> ITKCommon_EXPORT Zero;
  static const CovariantVector<unsigned long, 9> ITKCommon_EXPORT One;
  static CovariantVector<unsigned long, 9>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<float, 9> > {
public:
  typedef CovariantVector<float, 9> ValueType;
  typedef CovariantVector<float, 9> PrintType;
  typedef CovariantVector<float, 9> AbsType;
  typedef CovariantVector<double, 9> AccumulateType;
  typedef CovariantVector<double, 9> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<float, 9> ITKCommon_EXPORT Zero;
  static const CovariantVector<float, 9> ITKCommon_EXPORT One;
  static CovariantVector<float, 9>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<double, 9> > {
public:
  typedef CovariantVector<double, 9> ValueType;
  typedef CovariantVector<double, 9> PrintType;
  typedef CovariantVector<double, 9> AbsType;
  typedef CovariantVector<long double, 9> AccumulateType;
  typedef CovariantVector<double, 9> RealType;
  typedef double ScalarRealType;
  static const CovariantVector<double, 9> ITKCommon_EXPORT Zero;
  static const CovariantVector<double, 9> ITKCommon_EXPORT One;
  static CovariantVector<double, 9>  ZeroValue() { return Zero; }
};
template <> class NumericTraits<CovariantVector<long double, 9> > {
public:
  typedef CovariantVector<long double, 9> ValueType;
  typedef CovariantVector<long double, 9> PrintType;
  typedef CovariantVector<long double, 9> AbsType;
  typedef CovariantVector<long double, 9> AccumulateType;
  typedef CovariantVector<long double, 9> RealType;
  typedef long double ScalarRealType;
  static const CovariantVector<long double, 9> ITKCommon_EXPORT Zero;
  static const CovariantVector<long double, 9> ITKCommon_EXPORT One;
  static CovariantVector<long double, 9>  ZeroValue() { return Zero; }
};

} // end namespace itk

#endif // __itkNumericTraitsCovariantVectorPixel_h  

