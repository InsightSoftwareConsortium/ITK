#ifndef _cxxTypes_h
#define _cxxTypes_h

#include <string>
#include <list>

namespace _cxx_
{

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
typedef std::back_insert_iterator<ClassTypeList>  ClassTypeListInserter;

} // namespace _cxx_

#include "cxxType.h"
#include "cxxCvQualifiedType.h"
#include "cxxClassType.h"
#include "cxxPointerType.h"
#include "cxxPointerToMemberType.h"
#include "cxxReferenceType.h"
#include "cxxFundamentalType.h"
#include "cxxArrayType.h"
#include "cxxFunctionType.h"

#endif
