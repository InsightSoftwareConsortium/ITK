#ifndef _cxxPointerType_h
#define _cxxPointerType_h

#include "cxxCvQualifiedType.h"

namespace _cxx_
{


/**
 * Represent a C++ pointer type.
 */
class PointerType: public Type
{
public:
  typedef PointerType Self;
  
  /**
   * Retrieve what kind of Type this is.
   */
  virtual RepresentationType GetRepresentationType() const
    { return PointerType_id; }
  
  PointerType(const CvQualifiedType&);
  
protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;
  
private:
  /**
   * The type to which this type refers.
   */
  CvQualifiedType m_ReferencedType;
};

} // namespace _cxx_


#endif
