#ifndef _cxxTypes_h
#define _cxxTypes_h

#include <string>

namespace _cxx_
{

/**
 * A convenient string type.
 */
typedef std::string String;

/**
 * Enumeration of identifiers for representation types.
 */
enum RepresentationType
{
  Undefined_id=0,
  
  ArrayType_id, ClassType_id, PointerType_id, PointerToMemberType_id,
  ReferenceType_id, FundamentalType_id, FunctionType_id
};

} // namespace _cxx_

// Include all the representation types.
#include "cxxCvQualifiedType.h"
#include "cxxClassType.h"
#include "cxxPointerType.h"
#include "cxxPointerToMemberType.h"
#include "cxxReferenceType.h"
#include "cxxFundamentalType.h"
#include "cxxArrayType.h"
#include "cxxFunctionType.h"

} // namespace _cxx_

#endif

