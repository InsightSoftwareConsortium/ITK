#include "cxxTypeSystem.h"

namespace _cxx_
{


/**
 * Get the type representation for an ArrayType with the given element type
 * and length.  If one does not exist, it will be created.
 */
const ArrayType*
TypeSystem
::GetArrayType(const CvQualifiedType& in_elementType,
               unsigned long in_length)
{
  // Create a key to identify this type in the map.
  ArrayTypeKey key(in_elementType, in_length);
  
  // Look for an existing copy of this type.
  ArrayTypeMap::const_iterator i = m_ArrayTypeMap.find(key);
  
  if(i != m_ArrayTypeMap.end())
    {
    // An existing copy was found, return it.
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    ArrayType* newArrayType = new ArrayType(in_elementType, in_length);
    m_ArrayTypeMap[key] = newArrayType;
    return newArrayType;
    }
}


/**
 * Get the type representation for a ClassType with the given name.
 * If one does not exist, it will be created.
 * We allow the pointer to a non-const ClassType so that parent and
 * conversion constructor/operator information can be added.
 */
ClassType*
TypeSystem
::GetClassType(const String& in_name)
{
  // Look for an existing copy of this type.
  ClassTypeMap::const_iterator i = m_ClassTypeMap.find(in_name);
  
  if(i != m_ClassTypeMap.end())
    {
    // An existing copy was found, return it.
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    ClassType* newClassType = new ClassType(in_name);
    m_ClassTypeMap[in_name] = newClassType;
    return newClassType;
    }
}


/**
 * IMPLEMENT ME
 */
const FunctionType*
TypeSystem
::GetFunctionType(const CvQualifiedType& in_type)
{
  return new FunctionType(in_type);
}


/**
 * Get the type representation for a FundamentalType with the given id.
 * If one does not exist, it will be created.
 */
const FundamentalType*
TypeSystem
::GetFundamentalType(FundamentalType::Id in_id)
{
  // Look for an existing copy of this type.
  FundamentalTypeMap::const_iterator i = m_FundamentalTypeMap.find(in_id);
  
  if(i != m_FundamentalTypeMap.end())
    {
    // An existing copy was found, return it.
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    FundamentalType* newFundamentalType = new FundamentalType(in_id);
    m_FundamentalTypeMap[in_id] = newFundamentalType;
    return newFundamentalType;
    }
}


/**
 * Get the type representation for a PointerType with the given destination
 * type.  If one does not exist, it will be created.
 */
const PointerType*
TypeSystem
::GetPointerType(const CvQualifiedType& in_type)
{
  // Look for an existing copy of this type.
  PointerTypeMap::const_iterator i = m_PointerTypeMap.find(in_type);
  
  if(i != m_PointerTypeMap.end())
    {
    // An existing copy was found, return it.
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    PointerType* newPointerType = new PointerType(in_type);
    m_PointerTypeMap[in_type] = newPointerType;
    return newPointerType;
    }
}


/**
 * Get the type representation for a PointerToMemberType with the given
 * destination type and ClassType.  If one does not exist, it will be created.
 */
const PointerToMemberType*
TypeSystem
::GetPointerToMemberType(const CvQualifiedType& in_type,
                            const ClassType* in_class)
{
  // Create a key to identify this type in the map.
  PointerToMemberTypeKey key(in_type, in_class);
  
  // Look for an existing copy of this type.
  PointerToMemberTypeMap::const_iterator i =
    m_PointerToMemberTypeMap.find(key);
  
  if(i != m_PointerToMemberTypeMap.end())
    {
    // An existing copy was found, return it.
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    PointerToMemberType* newPointerToMemberType =
      new PointerToMemberType(in_type, in_class);
    m_PointerToMemberTypeMap[key] = newPointerToMemberType;
    return newPointerToMemberType;
    }
}


/**
 * Get the type representation for a ReferenceType with the given
 * destination type.  If one does not exist, it will be created.
 */
const ReferenceType*
TypeSystem
::GetReferenceType(const CvQualifiedType& in_type)
{
  // Look for an existing copy of this type.
  ReferenceTypeMap::const_iterator i = m_ReferenceTypeMap.find(in_type);
  
  if(i != m_ReferenceTypeMap.end())
    {
    // An existing copy was found, return it.
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    ReferenceType* newReferenceType = new ReferenceType(in_type);
    m_ReferenceTypeMap[in_type] = newReferenceType;
    return newReferenceType;
    }
}


/**
 * Destructor frees all existing type representations allocated by this
 * TypeSystem.
 */
TypeSystem
::~TypeSystem()
{
  for(ArrayTypeMap::iterator i = m_ArrayTypeMap.begin();
      i != m_ArrayTypeMap.end(); ++i)
    {
    delete i->second;
    }
  
  for(ClassTypeMap::iterator i = m_ClassTypeMap.begin();
      i != m_ClassTypeMap.end(); ++i)
    {
    delete i->second;
    }
  
  for(FundamentalTypeMap::iterator i = m_FundamentalTypeMap.begin();
      i != m_FundamentalTypeMap.end(); ++i)
    {
    delete i->second;
    }
  
  for(PointerTypeMap::iterator i = m_PointerTypeMap.begin();
      i != m_PointerTypeMap.end(); ++i)
    {
    delete i->second;
    }
  
  for(PointerToMemberTypeMap::iterator i = m_PointerToMemberTypeMap.begin();
      i != m_PointerToMemberTypeMap.end(); ++i)
    {
    delete i->second;
    }
  
  for(ReferenceTypeMap::iterator i = m_ReferenceTypeMap.begin();
      i != m_ReferenceTypeMap.end(); ++i)
    {
    delete i->second;
    }
}


} // namespace _cxx_
