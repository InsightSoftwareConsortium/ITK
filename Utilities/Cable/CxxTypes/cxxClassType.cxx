#include "cxxTypes.h"

namespace _cxx_
{

  
/**
 * Retrieve what kind of Type this is.
 */
RepresentationType
ClassType
::GetRepresentationType() const
{
  return ClassType_id;
}


/**
 * Get an insertion iterator to add public superclasses to this class.
 */
ClassTypeListInserter
ClassType
::GetParentInserter()
{
  return std::back_inserter(m_Parents);
}


/**
 * Add a public superclass to this class.
 */
void
ClassType
::AddParent(const ClassType* p)
{
  m_Parents.push_back(p);
}


/**
 *
 */
bool
ClassType
::CanConvertTo(const CvQualifiedType&, bool, bool, bool) const
{
  return false;
}


/**
 * Constructor for ClassType just takes the name of the class.  This name
 * may include template parameters of an instantiation.
 */
ClassType
::ClassType(const String& in_name):
  m_Name(in_name)
{
}


} // namespace _cxx_
