#ifndef _cxxArrayType_h
#define _cxxArrayType_h

#include "cxxCvQualifiedType.h"

namespace _cxx_
{


/**
 * Represents a C-style array type.
 */
class ArrayType: public Type
{
public:
  typedef ArrayType Self;
  
  /**
   * Retrieve what kind of Type this is.
   */
  virtual RepresentationType GetRepresentationType() const
    { return ArrayType_id; }

  ArrayType(const CvQualifiedType& in_elementType, unsigned long in_length):
    m_ElementType(in_elementType), m_Length(in_length) {}
  
protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;
  
private:
  /**
   * The type of the array's elements.
   */
  CvQualifiedType m_ElementType;

  /**
   * The length of the array.
   */
  unsigned long m_Length;
};

} // namespace _cxx_


#endif
