/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    genCvTypeGenerator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "genCvTypeGenerator.h"


namespace gen
{


/**
 * Generate the CvType specialization class declaration code for all the
 * types known to this generator.
 */
void CvTypeGenerator::GenerateClasses(std::ostream& os) const
{
  for(TypeOrdering::const_iterator t = m_TypeOrdering.begin();
      t != m_TypeOrdering.end(); ++t)
    {
    os << "template <> struct CvType< "
       << t->GetName().c_str()
       << " >   { static CvQualifiedType type; };\n";
    }
}


/**
 * Generate the CvType specialization data declaration code for all the
 * types known to this generator.
 */
void CvTypeGenerator::GenerateDataDeclarations(std::ostream& os) const
{
  for(TypeOrdering::const_iterator t = m_TypeOrdering.begin();
      t != m_TypeOrdering.end(); ++t)
    {
    os << "CvQualifiedType CvType< " << t->GetName().c_str() << " >::type;\n";
    }
}


/**
 * Generate the CvType specialization initialization code for all the
 * types known to this generator.
 */
void CvTypeGenerator::GenerateInitalizations(std::ostream& os) const
{
  for(TypeOrdering::const_iterator t = m_TypeOrdering.begin();
      t != m_TypeOrdering.end(); ++t)
    {
    this->GenerateInitialization(os, *t);
    }
}


/**
 * Add the given type to the set that will be generated.
 */
void CvTypeGenerator::Add(const cxx::CvQualifiedType& cvType)
{
  // If the type has already been added to the list, we are done.
  if(m_TypesAdded.find(cvType) != m_TypesAdded.end())
    { return; }
     
  // First, recursively add any "inner" types.
  const cxx::Type* type = cvType.GetType();
  switch (type->GetRepresentationType())
    {
    case cxx::ArrayType_id:
      this->Add(cxx::ArrayType::SafeDownCast(type)->GetElementType());
      break;
    case cxx::ClassType_id:
      // A ClassType has no "inner" types, but we want the first entry
      // for any class to have no cv-qualifiers.  This first entry
      // will be used for construction parent class information.
      if(cvType.IsConst() || cvType.IsVolatile())
        {
        this->Add(type->GetCvQualifiedType(false, false));
        }
      break;
    case cxx::FunctionType_id:
      this->AddFunctionTypes(cxx::FunctionType::SafeDownCast(type));
      break;
    case cxx::FundamentalType_id:
      // A FundamentalType has no "inner" types.  Terminate recursion.
      break;
    case cxx::PointerType_id:
      this->Add(cxx::PointerType::SafeDownCast(type)->GetPointedToType());
      break;
    case cxx::PointerToMemberType_id:
      {
      const cxx::PointerToMemberType* t = cxx::PointerToMemberType::SafeDownCast(type);
      this->Add(t->GetPointedToType());
      this->Add(t->GetClassType()->GetCvQualifiedType(false, false));
      }; break;
    case cxx::ReferenceType_id:
      this->Add(cxx::ReferenceType::SafeDownCast(type)->GetReferencedType());
      break;
    case cxx::Undefined_id:
    default: break;
    }
  
  // All "inner" types have been added.  Add this type.
  m_TypesAdded.insert(cvType);
  
  // All the fundamental types and type "char*" have predefined
  // specializations in the wrapper facilities.  Don't add these to the
  // ordering, but add all others.
  if(!type->IsFundamentalType() && !this->TypeIsPointerToChar(cvType))
    {
    m_TypeOrdering.push_back(cvType);
    }
}


/**
 * Called internally by CvTypeGenerator::Add() to add the "inner" types
 * of a FunctionType.
 */
void CvTypeGenerator::AddFunctionTypes(const cxx::FunctionType* functionType)
{
  // Add the function's return type.
  this->Add(functionType->GetReturnType());
  
  // Add the function's argument types.
  for(cxx::CvQualifiedTypes::const_iterator arg = functionType->GetArgumentTypes().begin();
      arg != functionType->GetArgumentTypes().end(); ++arg)
    {
    this->Add(*arg);
    }
}


/**
 * Called internally by GenerateInitializations().
 *
 * Generates code which will construct the given type's representation
 * and initialize its CvType specialization.
 */
void
CvTypeGenerator
::GenerateInitialization(std::ostream& os,
                         const cxx::CvQualifiedType& cvType) const
{
  const cxx::Type* type = cvType.GetType();
  switch (type->GetRepresentationType())
    {
    case cxx::ArrayType_id:
      os << "  CvType< " << cvType.GetName().c_str()
         << " >::type = TypeInfo::GetArrayType(CvType< "
         << cxx::ArrayType::SafeDownCast(type)->GetElementType().GetName().c_str()
         << " >::type, " << (cvType.IsConst()? "true":"false")
         << ", " << (cvType.IsVolatile()? "true":"false") << ");\n";
      break;
    case cxx::ClassType_id:
      // TODO: Generate superclass information on cv-unqualified version.
      os << "  CvType< " << cvType.GetName().c_str()
         << " >::type = TypeInfo::GetClassType(\""
         << cxx::ClassType::SafeDownCast(type)->GetName().c_str()
         << "\", " << (cvType.IsConst()? "true":"false")
         << ", " << (cvType.IsVolatile()? "true":"false") << ");\n";
      break;
    case cxx::FunctionType_id:
      // TODO: Implement.
      // cxx::FunctionType::SafeDownCast(type);
      break;
    case cxx::PointerType_id:
      os << "  CvType< " << cvType.GetName().c_str()
         << " >::type = TypeInfo::GetPointerType(CvType< "
         << cxx::PointerType::SafeDownCast(type)->GetPointedToType().GetName().c_str()
         << " >::type, " << (cvType.IsConst()? "true":"false")
         << ", " << (cvType.IsVolatile()? "true":"false") << ");\n";      
      break;
    case cxx::PointerToMemberType_id:
      {
      const cxx::PointerToMemberType* t = cxx::PointerToMemberType::SafeDownCast(type);      
      os << "  CvType< " << cvType.GetName().c_str()
         << " >::type = TypeInfo::GetPointerToMemberType(CvType< "
         << t->GetPointedToType().GetName().c_str()
         << " >::type, "
         << "ClassType::SafeDownCast(CvType< " << t->GetClassType()->Name().c_str() << " >::type.GetType())"
         << ", " << (cvType.IsConst()? "true":"false")
         << ", " << (cvType.IsVolatile()? "true":"false") << ");\n";      
      }; break;
    case cxx::ReferenceType_id:
      os << "  CvType< " << cvType.GetName().c_str()
         << " >::type = TypeInfo::GetReferenceType(CvType< "
         << cxx::ReferenceType::SafeDownCast(type)->GetReferencedType().GetName().c_str()
         << " >::type);\n";
      break;
    case cxx::FundamentalType_id:
      // All FundamentalTypes have predefined specializations.  We should
      // never get here.
    case cxx::Undefined_id:
    default: break;
    }
}


/**
 * Returns true if the given type is "char*", "const char*", "volatile char*",
 * or "const volatile char*".
 */
bool CvTypeGenerator::TypeIsPointerToChar(const cxx::CvQualifiedType& cvType) const
{
  // There must be no cv-qualifiers.
  if(cvType.IsConst() || cvType.IsVolatile())
    {
    return false;
    }  
  
  const cxx::Type* type = cvType.GetType();
  
  // The type must be a PointerType.
  if(!type->IsPointerType())
    {
    return false;
    }
  
  // The PointerType's pointed-to-type must be "char".
  return this->TypeIsChar(cxx::PointerType::SafeDownCast(type)->GetPointedToType());
}

/**
 * Returns true if the given type is "char", "const char", "volatile char",
 * or "const volatile char".
 */
bool CvTypeGenerator::TypeIsChar(const cxx::CvQualifiedType& cvType) const
{
  const cxx::Type* type = cvType.GetType();
  // The type must be a FundamentalType.
  if(!type->IsFundamentalType())
    {
    return false;
    }
  
  // The FundamentalType must be "char".
  return cxx::FundamentalType::SafeDownCast(type)->IsChar();
}

} // namespace gen
