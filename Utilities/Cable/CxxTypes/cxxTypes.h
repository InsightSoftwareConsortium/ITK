#ifndef _cxxTypes_h
#define _cxxTypes_h

#include <string>
#include <list>

typedef std::string String;

/**
 * Enumeration of identifiers for representation types.
 */
enum RepresentationType {
  Undefined_id=0,
  
  ArrayType_id, ClassType_id, PointerType_id, PointerToMemberType_id,
  ReferenceType_id, FundamentalType_id, FunctionType_id
};


class Type;
class CvQualifiedType;
class ClassType;
class PointerType;
class PointerToMemberType;
class ReferenceType;
class FundamentalType;
class ArrayType;
class FunctionType;

/**
 * A list of class types.
 */
typedef std::list<const ClassType*> ClassTypeList;


/**
 * Abstract interface to a C++ type representation.
 */
class Type
{
public:
  /**
   * Retrieve what kind of Type this is.
   */
  virtual RepresentationType GetRepresentationType() const = 0;

protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const = 0;
  
  friend CvQualifiedType;
};


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


/**
 * Represents a C++ class type.  This could have been produced by a
 * class, struct, union, template full specialization, or template
 * instantiation.
 */
class ClassType: public Type
{
public:
  typedef ClassType Self;
  
  /**
   * Retrieve what kind of Type this is.
   */
  virtual RepresentationType GetRepresentationType() const
    { return ClassType_id; }

  ClassType(const String& in_name): m_Name(in_name) {}

protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;
  
private:  
  /**
   * The name of the class.
   */
  String m_Name;
  
  /**
   * The immediate public superclasses of this class.
   * A pointer or reference ot this class can be cast up to these
   * types.
   */
  ClassTypeList m_Parents;
  
  /**
   * The list of types from which this class can construct.
   */
  CvQualifiedTypeList m_ConversionByConstructor;
  
  /**
   * The list of types to which this class can convert by type conversion
   * operator.
   */
  CvQualifiedTypeList m_ConversionOperators;
};


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
  
  PointerType(const CvQualifiedType& in_type): m_ReferencedType(in_type) {}
  
protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;
  
private:
  /**
   * The type to which this type refers.
   */
  CvQualifiedType m_ReferencedType;
};


/**
 * Represents a C++ pointer-to-member type.
 */
class PointerToMemberType: public PointerType
{
public:
  typedef PointerToMemberType Self;
  
  /**
   * Retrieve what kind of Type this is.
   */
  virtual RepresentationType GetRepresentationType() const
    { return PointerToMemberType_id; }

  PointerToMemberType(const CvQualifiedType& in_type,
                      const ClassType* in_class):
    PointerType(in_type), m_ClassType(in_class) {}
  
protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;
  
private:
  /**
   * The class type holding the member.
   */
  const ClassType* m_ClassType;
};


/**
 * Represents a C++ reference type.
 */
class ReferenceType: public Type
{
public:
  typedef ReferenceType Self;
  
  /**
   * Retrieve what kind of Type this is.
   */
  virtual RepresentationType GetRepresentationType() const
    { return ReferenceType_id; }
  
  ReferenceType(const CvQualifiedType& in_type): m_ReferencedType(in_type) {}
  
protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;
  
private:
  /**
   * The type to which this type refers.
   */
  CvQualifiedType m_ReferencedType;
};


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


/**
 * Represent a C++ function type.  This consists of the return type and
 * argument types.
 */
class FunctionType: public Type
{
public:
  typedef FunctionType Self;
  
  /**
   * Retrieve what kind of Type this is.
   */
  virtual RepresentationType GetRepresentationType() const
    { return FunctionType_id; }  

protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;
  
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


#endif


#if 0
/**
 * Represents a new name for an existing type.  All type operations are
 * passed through to the real type.
 */
class TypedefType: public Type
{
public:
  typedef TypedefType Self;
  
  /**
   * Retrieve what kind of Type this is.
   */
  virtual RepresentationType GetRepresentationType() const
    { return m_CvQualifiedType.GetType()->GetRepresentationType(); }

  TypedefType(const String& in_name, CvQualifiedType in_type):
    m_Name(in_name), m_Type(in_type) {}
  
private:
  /**
   * The new name for the type.
   */
  String m_Name;
  
  /**
   * The real type.
   */
  CvQualifiedType m_CvQualifiedType;
};

#endif
