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
  
  virtual RepresentationType GetRepresentationType() const;

protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;
  
  ArrayType(const CvQualifiedType&, unsigned long);
  ArrayType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~ArrayType() {}
  
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
