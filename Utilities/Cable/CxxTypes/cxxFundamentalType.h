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
  
  /**
   * Retrieve what kind of Type this is.
   */
  virtual RepresentationType GetRepresentationType() const
    { return FundamentalType_id; }  

  FundamentalType(Id in_id): m_Id(in_id) {}
  
protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;
  
private:
  /**
   * Store which integral type this is.
   */
  Id m_Id;
};


} // namespace _cxx_


#endif
