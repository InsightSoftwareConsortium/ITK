#ifndef _cxxFunctionType_h
#define _cxxFunctionType_h

#include "cxxCvQualifiedType.h"

namespace _cxx_
{


/**
 * Represent a C++ function type.  This consists of the return type and
 * argument types.
 */
class FunctionType: public Type
{
public:
  typedef FunctionType Self;
  
  virtual RepresentationType GetRepresentationType() const;

protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;

  FunctionType() {}
  FunctionType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~FunctionType() {}
  
private:
  /**
   * The function's return type.
   */
  CvQualifiedType m_ReturnType;
  
  /**
   * The function's argument types.
   */
  CvQualifiedTypeList m_ArgumentList;
};

} // namespace _cxx_


#endif
