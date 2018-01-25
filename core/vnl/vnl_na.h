// This is core/vnl/vnl_na.h
#ifndef vnl_na_h_
#define vnl_na_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif


#include <iosfwd>
#include <vcl_compiler.h>
#include "vnl/vnl_export.h"

//:
// \file
// \brief NA (Not Available) is a particular double (or single-precision) NaN to represent missing data.
// For example, where a vnl_vector<double> represents a series of samples from an image,
// NA could be used to represent places where the measurement was taken outside the image.
//
// NA is distinct from the two other standard meanings of NaN - Indeterminate and Error.
// It is entirely up to each algorithm to treat NA values meaningfully. Unless
// a function's interpretation of NA is explicitly documented, you should assume that
// it will be treated similarly to every other NaN.
// The IEEE754 bit value used to represent NA in double-precision is 0x7ff00000000007a2, the
// same as used by Octave and R. Initial values of NA are stored as signalling NaNs, but
// many uses will convert this to the non-signalling variant 0x7ff80000000007a2. vnl_isna()
// will accept either variant.
//
// The single precision NA is stored as 0x7f8007a2. I cannot find any external support for
// this or any other value for single precision NA. There is no automatic conversion between
// the NA values during casting, promotion, etc. If you want to convert a float to double,
// whilst preserving the NA-ness of the value, you will have to test for and set the new NA
// value explicitly.
//
// You can read and write floating point values from a stream using standard operators
// by using a conversion manipulator.
// \verbatim
// double x, y;
// is >> vnl_na_stream(x) >> vnl_na_stream(x);
// os << vnl_na_stream(x) << ' ' << vnl_na_stream(x);
// \endverbatim


//: qNaN to indicate value Not Available.
// Don't assume that any VXL functions will do something sensible in the face of NA, unless
// explicitly documented.
VNL_EXPORT double vnl_na(double dummy);

//: qNaN to indicate value Not Available.
// Don't assume that any VXL functions will do something sensible in the face of NA, unless
// explicitly documented.
VNL_EXPORT float vnl_na(float dummy);

//: True if parameter is specific NA qNaN.
// Tests for bit pattern 0x7ff00000000007a2, as used by Octave and R
VNL_EXPORT bool vnl_na_isna(double);

//: True if parameter is specific NA qNaN.
// Tests for bit pattern 0x7f8007a2
VNL_EXPORT bool vnl_na_isna(float);


//: Replace NaNs with NA, leave other values alone.
VNL_EXPORT double vnl_na_nan_to_na(double v);

//: Replace NaNs with NA, leave other values alone.
VNL_EXPORT float vnl_na_nan_to_na(float v);


//: Read a floating point number or "NA" from a stream.
// Should behave exactly like a>>x, if the extraction operator was aware of the
// character sequence \code NA.
VNL_EXPORT void vnl_na_extract(std::istream &is, double& x);


//: Write a floating point number or "NA" to a stream.
// Should behave exactly like a<<x, if the insertion operator was aware of the
// character sequence \code NA.
VNL_EXPORT void vnl_na_insert(std::ostream &is, double x);

//: Read a floating point number or "NA" from a stream.
// Should behave exactly like a>>x, if the extraction operator was aware of the
// character sequence \code NA.
VNL_EXPORT void vnl_na_extract(std::istream &is, float& x);


//: Write a floating point number or "NA" to a stream.
// Should behave exactly like a<<x, if the insertion operator was aware of the
// character sequence \code NA.
VNL_EXPORT void vnl_na_insert(std::ostream &is, float x);


//: Wrapper around a double or float that handles streaming NA.
template <class T> struct VNL_TEMPLATE_EXPORT vnl_na_stream_t
{
  T& x_;
  vnl_na_stream_t(T& x): x_(x) {}
};

//: Wrapper around a double or float that handles streaming NA.
template <class T> struct VNL_TEMPLATE_EXPORT vnl_na_stream_const_t
{
  const T& x_;
  vnl_na_stream_const_t(const T& x): x_(x) {}
};

//: Wrap a double or float to handle streaming NA.
template <class T> inline vnl_na_stream_t<T> vnl_na_stream(T& x)
{
  return vnl_na_stream_t<T>(x);
}

//: Wrap a double or float to handle streaming NA.
template <class T> inline vnl_na_stream_const_t<T> vnl_na_stream(const T& x)
{
  return vnl_na_stream_const_t<T>(x);
}

//: Insert wrapped double or float into stream, whilst handling NA.
template <class T> inline std::ostream& operator <<(std::ostream &os, const vnl_na_stream_t<T>& ns)
{
  vnl_na_insert(os, ns.x_);
  return os;
}

//: Insert wrapped double or float into stream, whilst handling NA.
template <class T> inline std::ostream& operator <<(std::ostream &os, const vnl_na_stream_const_t<T>& ns)
{
  vnl_na_insert(os, ns.x_);
  return os;
}

//: Extract wrapped double or float from stream, whilst handling NA.
template <class T> inline std::istream& operator >>(std::istream &is, const vnl_na_stream_t<T>& ns)
{
  vnl_na_extract(is, ns.x_);
  return is;
}

#endif // vnl_na_h_
