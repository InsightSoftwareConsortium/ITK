// This is core/vnl/vnl_numeric_traits.cxx
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// Created: 12 Feb 98
//
//-----------------------------------------------------------------------------

#include <complex>
#include "vnl_numeric_traits.h"

constexpr bool vnl_numeric_traits<bool>::zero;
constexpr char vnl_numeric_traits<char>::zero;
constexpr unsigned char vnl_numeric_traits<unsigned char>::zero;
constexpr signed char vnl_numeric_traits<signed char>::zero;
constexpr short vnl_numeric_traits<short>::zero;
constexpr unsigned short vnl_numeric_traits<unsigned short>::zero;
constexpr int vnl_numeric_traits<int>::zero;
constexpr unsigned int vnl_numeric_traits<unsigned int>::zero;
constexpr long vnl_numeric_traits<long>::zero;
constexpr unsigned long vnl_numeric_traits<unsigned long>::zero;
//long long - target type will have width of at least 64 bits. (since C++11)
constexpr long long vnl_numeric_traits<long long>::zero;
constexpr unsigned long long vnl_numeric_traits<unsigned long long>::zero;


constexpr bool vnl_numeric_traits<bool>::one;
constexpr char vnl_numeric_traits<char>::one;
constexpr unsigned char vnl_numeric_traits<unsigned char>::one;
constexpr signed char vnl_numeric_traits<signed char>::one;
constexpr short vnl_numeric_traits<short>::one;
constexpr unsigned short vnl_numeric_traits<unsigned short>::one;
constexpr int vnl_numeric_traits<int>::one;
constexpr unsigned int vnl_numeric_traits<unsigned int>::one;
constexpr long vnl_numeric_traits<long>::one;
constexpr unsigned long vnl_numeric_traits<unsigned long>::one;

//long long - target type will have width of at least 64 bits. (since C++11)
constexpr long long vnl_numeric_traits<long long>::one;
constexpr unsigned long long vnl_numeric_traits<unsigned long long>::one;

constexpr bool vnl_numeric_traits<bool>::maxval;
constexpr char vnl_numeric_traits<char>::maxval;
//  It is 127 when "char" is signed and 255 when "char" is unsigned.
constexpr unsigned char vnl_numeric_traits<unsigned char>::maxval;
constexpr signed char vnl_numeric_traits<signed char>::maxval;
constexpr short vnl_numeric_traits<short>::maxval;
constexpr unsigned short vnl_numeric_traits<unsigned short>::maxval;
constexpr int vnl_numeric_traits<int>::maxval;
constexpr unsigned int vnl_numeric_traits<unsigned int>::maxval;
constexpr long vnl_numeric_traits<long>::maxval;
constexpr unsigned long vnl_numeric_traits<unsigned long>::maxval;
//long long - target type will have width of at least 64 bits. (since C++11)
constexpr long long vnl_numeric_traits<long long>::maxval;
constexpr unsigned long long vnl_numeric_traits<unsigned long long>::maxval;

constexpr float vnl_numeric_traits<float>::zero;
constexpr double vnl_numeric_traits<double>::zero;
constexpr long double vnl_numeric_traits<long double>::zero;

constexpr float vnl_numeric_traits<float>::one;
constexpr double vnl_numeric_traits<double>::one;
constexpr long double vnl_numeric_traits<long double>::one;

constexpr float vnl_numeric_traits<float>::maxval;
constexpr double vnl_numeric_traits<double>::maxval;
constexpr long double vnl_numeric_traits<long double>::maxval;

// Must use constructor-call syntax for initialization of complex specializations.
// std::complex
//
// 'complex<float>' is not suitable for being a constexpr because it is not a literal type
//                   because it is not an aggregate and has no constexpr
//                  constructors other than copy or move constructors
const std::complex<float> vnl_numeric_traits<std::complex<float> >::zero(0.0F);
const std::complex<double> vnl_numeric_traits<std::complex<double> >::zero(0.0);
const std::complex<long double> vnl_numeric_traits<std::complex<long double> >::zero(0.0L);

const std::complex<float> vnl_numeric_traits<std::complex<float> >::one(1.0F);
const std::complex<double> vnl_numeric_traits<std::complex<double> >::one(1.0);
const std::complex<long double> vnl_numeric_traits<std::complex<long double> >::one(1.0L);

// Unknown, so undefined. Will cause link errors if someone refers to it.
//const std::complex<float> vnl_numeric_traits<std::complex<float> >::maxval;
//const std::complex<double> vnl_numeric_traits<std::complex<double> >::maxval;
//const std::complex<long double> vnl_numeric_traits<std::complex<long double> >::maxval;

//--------------------------------------------------------------------------------
