#ifndef _cxxFundamentalType_h
#define _cxxFundamentalType_h

#include "cxxCvQualifiedType.h"

namespace _cxx_
{


/**
 * Represent a C++ fundamental type.  These are defined in 3.9.1.
 */
class FundamentalType: public Type
{
public:
  typedef FundamentalType Self;
  
  /**
   * Enumerate the fundamental types.
   */
  enum Id { UnsignedChar, UnsignedShortInt, UnsignedInt, UnsignedLongInt,
            SignedChar, Char, ShortInt, Int, LongInt, WChar_t, Bool,
            Float, Double, LongDouble, Void };         
  
  virtual RepresentationType GetRepresentationType() const;

protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;
  
  FundamentalType(Id);
  FundamentalType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~FundamentalType() {}
  
private:
  /**
   * Store which integral type this is.
   */
  Id m_Id;
};


} // namespace _cxx_


#endif
