#ifndef __itkNumericTraitsTensorPixel_h
#define __itkNumericTraitsTensorPixel_h

#include "itkNumericTraits.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkDiffusionTensor3D.h"

// This file is meant to define numeric traits for tensor pixels types in itk

namespace itk
{
/** \class NumericTraits<SymmetricSecondRankTensor<unsigned char> >
 * \brief Define traits for type SymmetricSecondRankTensor<unsigned char>.
 * \ingroup DataRepresentation
 */
template <unsigned int NDimensions>
class NumericTraits<SymmetricSecondRankTensor<unsigned char, NDimensions > > {
public:
  typedef SymmetricSecondRankTensor<unsigned char, NDimensions> ValueType;
  typedef SymmetricSecondRankTensor<unsigned char, NDimensions> PrintType;
  typedef SymmetricSecondRankTensor<unsigned char, NDimensions> AbsType;
  typedef SymmetricSecondRankTensor<unsigned short, NDimensions> AccumulateType;
  typedef SymmetricSecondRankTensor<double, NDimensions> RealType;
  typedef double ScalarRealType;
};


/** \class NumericTraits<SymmetricSecondRankTensor<signed char> >
 * \brief Define traits for type SymmetricSecondRankTensor<signed char>.
 * \ingroup DataRepresentation
 */
template <unsigned int NDimensions>
class NumericTraits<SymmetricSecondRankTensor<signed char, NDimensions> > {
public:
  typedef SymmetricSecondRankTensor<signed char, NDimensions> ValueType;
  typedef SymmetricSecondRankTensor<signed char, NDimensions> PrintType;
  typedef SymmetricSecondRankTensor<unsigned char, NDimensions> AbsType;
  typedef SymmetricSecondRankTensor<short, NDimensions> AccumulateType;
  typedef SymmetricSecondRankTensor<double, NDimensions> RealType;
  typedef double ScalarRealType;
};


/** \class NumericTraits<SymmetricSecondRankTensor< char> >
 * \brief Define traits for type SymmetricSecondRankTensor<char>.
 * \ingroup DataRepresentation
 */
template <unsigned int NDimensions>
class NumericTraits<SymmetricSecondRankTensor<char, NDimensions> > {
public:
  typedef SymmetricSecondRankTensor<char, NDimensions> ValueType;
  typedef SymmetricSecondRankTensor<char, NDimensions> PrintType;
  typedef SymmetricSecondRankTensor<unsigned char, NDimensions> AbsType;
  typedef SymmetricSecondRankTensor<short, NDimensions> AccumulateType;
  typedef SymmetricSecondRankTensor<double, NDimensions> RealType;
  typedef double ScalarRealType;
};


/** \class NumericTraits<SymmetricSecondRankTensor< short> >
 * \brief Define traits for type SymmetricSecondRankTensor<short>.
 * \ingroup DataRepresentation
 */
template <unsigned int NDimensions>
class NumericTraits<SymmetricSecondRankTensor<short, NDimensions> > {
public:
  typedef SymmetricSecondRankTensor<short, NDimensions> ValueType;
  typedef SymmetricSecondRankTensor<short, NDimensions> PrintType;
  typedef SymmetricSecondRankTensor<unsigned short, NDimensions> AbsType;
  typedef SymmetricSecondRankTensor<int, NDimensions> AccumulateType;
  typedef SymmetricSecondRankTensor<double, NDimensions> RealType;
  typedef double ScalarRealType;
};

/** \class NumericTraits<SymmetricSecondRankTensor< unsigned short> >
 * \brief Define traits for type SymmetricSecondRankTensor<unsigned short>.
 * \ingroup DataRepresentation
 */
template <unsigned int NDimensions>
class NumericTraits<SymmetricSecondRankTensor<unsigned short, NDimensions> > {
public:
  typedef SymmetricSecondRankTensor<unsigned short, NDimensions> ValueType;
  typedef SymmetricSecondRankTensor<unsigned short, NDimensions> PrintType;
  typedef SymmetricSecondRankTensor<unsigned short, NDimensions> AbsType;
  typedef SymmetricSecondRankTensor<unsigned int, NDimensions> AccumulateType;
  typedef SymmetricSecondRankTensor<double, NDimensions> RealType;
  typedef double ScalarRealType;
};


/** \class NumericTraits<SymmetricSecondRankTensor< int> >
 * \brief Define traits for type SymmetricSecondRankTensor<int>.
 * \ingroup DataRepresentation
 */
template <unsigned int NDimensions>
class NumericTraits<SymmetricSecondRankTensor<int, NDimensions> > {
public:
  typedef SymmetricSecondRankTensor<int, NDimensions> ValueType;
  typedef SymmetricSecondRankTensor<int, NDimensions> PrintType;
  typedef SymmetricSecondRankTensor<unsigned int, NDimensions> AbsType;
  typedef SymmetricSecondRankTensor<long, NDimensions> AccumulateType;
  typedef SymmetricSecondRankTensor<double, NDimensions> RealType;
  typedef double ScalarRealType;
};

/** \class NumericTraits<SymmetricSecondRankTensor< unsigned int> >
 * \brief Define traits for type SymmetricSecondRankTensor<unsigned int>.
 * \ingroup DataRepresentation
 */
template <unsigned int NDimensions>
class NumericTraits<SymmetricSecondRankTensor<unsigned int, NDimensions> > {
public:
  typedef SymmetricSecondRankTensor<unsigned int, NDimensions> ValueType;
  typedef SymmetricSecondRankTensor<unsigned int, NDimensions> PrintType;
  typedef SymmetricSecondRankTensor<unsigned int, NDimensions> AbsType;
  typedef SymmetricSecondRankTensor<unsigned long, NDimensions> AccumulateType;
  typedef SymmetricSecondRankTensor<double, NDimensions> RealType;
  typedef double ScalarRealType;
};


/** \class NumericTraits<SymmetricSecondRankTensor< long> >
 * \brief Define traits for type SymmetricSecondRankTensor<long>.
 * \ingroup DataRepresentation
 */
template <unsigned int NDimensions>
class NumericTraits<SymmetricSecondRankTensor<long, NDimensions> > {
public:
  typedef SymmetricSecondRankTensor<long, NDimensions> ValueType;
  typedef SymmetricSecondRankTensor<long, NDimensions> PrintType;
  typedef SymmetricSecondRankTensor<unsigned long, NDimensions> AbsType;
  typedef SymmetricSecondRankTensor<long, NDimensions> AccumulateType;
  typedef SymmetricSecondRankTensor<double, NDimensions> RealType;
  typedef double ScalarRealType;
};

/** \class NumericTraits<SymmetricSecondRankTensor< unsigned long> >
 * \brief Define traits for type SymmetricSecondRankTensor<unsigned long>.
 * \ingroup DataRepresentation
 */
template <unsigned int NDimensions>
class NumericTraits<SymmetricSecondRankTensor<unsigned long, NDimensions> > {
public:
  typedef SymmetricSecondRankTensor<unsigned long, NDimensions> ValueType;
  typedef SymmetricSecondRankTensor<unsigned long, NDimensions> PrlongType;
  typedef SymmetricSecondRankTensor<unsigned long, NDimensions> AbsType;
  typedef SymmetricSecondRankTensor<unsigned long, NDimensions> AccumulateType;
  typedef SymmetricSecondRankTensor<double, NDimensions> RealType;
};


/** \class NumericTraits<SymmetricSecondRankTensor< float> >
 * \brief Define traits for type SymmetricSecondRankTensor<float>.
 * \ingroup DataRepresentation
 */
template <unsigned int NDimensions>
class NumericTraits<SymmetricSecondRankTensor<float, NDimensions> > {
public:
  typedef SymmetricSecondRankTensor<float, NDimensions> ValueType;
  typedef SymmetricSecondRankTensor<float, NDimensions> PrintType;
  typedef SymmetricSecondRankTensor<float, NDimensions> AbsType;
  typedef SymmetricSecondRankTensor<double, NDimensions> AccumulateType;
  typedef SymmetricSecondRankTensor<double, NDimensions> RealType;
  typedef double ScalarRealType;
};

/** \class NumericTraits<SymmetricSecondRankTensor< double> >
 * \brief Define traits for type SymmetricSecondRankTensor<double>.
 * \ingroup DataRepresentation
 */
template <unsigned int NDimensions>
class NumericTraits<SymmetricSecondRankTensor<double, NDimensions> > {
public:
  typedef SymmetricSecondRankTensor<double, NDimensions> ValueType;
  typedef SymmetricSecondRankTensor<double, NDimensions> PrintType;
  typedef SymmetricSecondRankTensor<double, NDimensions> AbsType;
  typedef SymmetricSecondRankTensor<long double, NDimensions> AccumulateType;
  typedef SymmetricSecondRankTensor<double, NDimensions> RealType;
  typedef double ScalarRealType;
};

/** \class NumericTraits<SymmetricSecondRankTensor< long double> >
 * \brief Define traits for type SymmetricSecondRankTensor<long double>.
 * \ingroup DataRepresentation
 */
template <unsigned int NDimensions>
class NumericTraits<SymmetricSecondRankTensor<long double, NDimensions> > {
public:
  typedef SymmetricSecondRankTensor<long double, NDimensions> ValueType;
  typedef SymmetricSecondRankTensor<long double, NDimensions> PrintType;
  typedef SymmetricSecondRankTensor<long double, NDimensions> AbsType;
  typedef SymmetricSecondRankTensor<long double, NDimensions> AccumulateType;
  typedef SymmetricSecondRankTensor<long double, NDimensions> RealType;
  typedef long double ScalarRealType;
};

} // end namespace itk

#endif // __itkNumericTraitsTensorPixel_h  
