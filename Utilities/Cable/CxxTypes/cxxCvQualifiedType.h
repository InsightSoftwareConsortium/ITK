#ifndef _cxxCvQualifiedType_h
#define _cxxCvQualifiedType_h

#include "cxxType.h"

namespace _cxx_
{


/**
 * Hold cv-qualifiers attached to a type.
 */
class CvQualifiedType
{
public:
  typedef CvQualifiedType Self;
  
  CvQualifiedType(const Type*);
  CvQualifiedType(const Self&);
  
  void SetConst(bool value)    { m_Const = value; }
  void SetVolatile(bool value) { m_Volatile = value; }
  void SetRestrict(bool value) { m_Restrict = value; }
  
  bool IsConst() const    { return m_Const; }
  bool IsVolatile() const { return m_Volatile; }
  bool IsRestrict() const { return m_Restrict; }
  
  const Type* GetType() const { return m_Type; }
  
  /**
   * Test if this cv-qualified type can be converted to the given
   * cv-qualified type.
   */
  bool CanConvertTo(const Self& t)
    { return m_Type->CanConvertTo(t, m_Const, m_Volatile, m_Restrict); }
  
private:
  const Type* const m_Type;
  bool m_Const;
  bool m_Volatile;
  bool m_Restrict;  
};

/**
 * A list of cv-qualified types.
 */
typedef std::list<CvQualifiedType>  CvQualifiedTypeList;

} // namespace _cxx_


#endif
