#include "cxxTypes.h"

namespace _cxx_
{


/**
 * Retrieve what kind of Type this is.
 */
RepresentationType
PointerType
::GetRepresentationType() const
{
  return PointerType_id;
}


/**
 * Test whether this type with the given cv-qualifiers can convert to
 * the given CvQualifiedType.
 */
bool
PointerType
::CanConvertTo(const CvQualifiedType& source,
               bool isConst, bool isVolatile) const
{
  // Dispatch to appropriate test function.
  switch (source.GetType()->GetRepresentationType())
    {
    case ArrayType_id: {
    return this->CanConvertFromArrayType(source, isConst, isVolatile);
    }; break;
    case ClassType_id: {
    }; break;
    case PointerType_id: {
    }; break;
    case PointerToMemberType_id: {
    }; break;
    case ReferenceType_id: {
    }; break;
    case FundamentalType_id: {
    }; break;
    case FunctionType_id: {
    }; break;
    }
}


/**
 * Constructor takes the cv-qualified type to which the pointer points.
 */
PointerType
::PointerType(const CvQualifiedType& in_type):
  m_ReferencedType(in_type)
{
}


/**
 * Return whether the given ArrayType can be converted to this PointerType.
 * Standard: 4.2/1.
 */
bool
PointerType
::CanConvertFromArrayType(const CvQualifiedType& source,
                          bool isConst, bool isVolatile) const
{
  const ArrayType* sourceType =
    dynamic_cast<const ArrayType*>(source.GetType());
  
  // Element types must match exactly.
  return (sourceType->GetElementType()
          == CvQualifiedType(this, isConst, isVolatile));
}


} // namespace _cxx_
