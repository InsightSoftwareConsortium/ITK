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
       << " >   { static CvQualifiedType type; typedef "
       << t->GetType()->GenerateDeclaration("NoCv").c_str() << "; ";
    this->GenerateArgumentAs(os, *t);
    os << "};\n";
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
void CvTypeGenerator::GenerateInitializations(std::ostream& os) const
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
      // Make sure the cv-unqualified version comes first.
      if(cvType.IsConst() || cvType.IsVolatile())
        {
        this->Add(type->GetCvQualifiedType(false, false));
        }
      // Make sure any superclasses come first.
      else
        {
        const cxx::ClassType* classType = cxx::ClassType::SafeDownCast(type);
        for(cxx::ClassTypes::const_iterator p = classType->ParentsBegin();
            p != classType->ParentsEnd(); ++p)
          {
          this->Add((*p)->GetCvQualifiedType(false, false));
          }
        }
      break;
    case cxx::EnumerationType_id:
      // An EnumerationType has no "inner" types.  Terminate recursion.
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
  
  m_TypeOrdering.push_back(cvType);
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
      {
      const cxx::ClassType* classType = cxx::ClassType::SafeDownCast(type);
      // Generate superclass information on cv-unqualified version.
      if(!cvType.IsConst() && !cvType.IsVolatile()
         && (classType->ParentsBegin() != classType->ParentsEnd()))
        {
        os << "  {\n"
           << "  ClassTypes parents;\n";
        for(cxx::ClassTypes::const_iterator p = classType->ParentsBegin();
            p != classType->ParentsEnd(); ++p)
          {
          os << "  parents.push_back(ClassType::SafeDownCast(CvType< " << (*p)->GetName().c_str()
             << " >::type.GetType()));\n";
          }
        os << "  CvType< " << cvType.GetName().c_str()
           << " >::type = TypeInfo::GetClassType(\""
           << classType->GetName().c_str()
           << "\", " << (cvType.IsConst()? "true":"false")
           << ", " << (cvType.IsVolatile()? "true":"false")
           << ", " << (classType->IsAbstract()? "true":"false")
           << ", parents);\n";
        os << "  }\n";
        }
      else
        {
        os << "  CvType< " << cvType.GetName().c_str()
           << " >::type = TypeInfo::GetClassType(\""
           << classType->GetName().c_str()
           << "\", " << (cvType.IsConst()? "true":"false")
           << ", " << (cvType.IsVolatile()? "true":"false")
           << ", " << (classType->IsAbstract()? "true":"false") << ");\n";
        }
      }; break;
    case cxx::EnumerationType_id:
      os << "  CvType< " << cvType.GetName().c_str()
         << " >::type = TypeInfo::GetEnumerationType(\""
         << cxx::EnumerationType::SafeDownCast(type)->GetName().c_str()
         << "\", " << (cvType.IsConst()? "true":"false")
         << ", " << (cvType.IsVolatile()? "true":"false") << ");\n";
      break;
    case cxx::FundamentalType_id:
      os << "  CvType< " << cvType.GetName().c_str()
         << " >::type = TypeInfo::GetFundamentalType(FundamentalType::";
      switch (cxx::FundamentalType::SafeDownCast(type)->GetId())
        {
        case cxx::FundamentalType::UnsignedChar:     os << "UnsignedChar";     break;
        case cxx::FundamentalType::UnsignedShortInt: os << "UnsignedShortInt"; break;
        case cxx::FundamentalType::UnsignedInt:      os << "UnsignedInt";      break;
        case cxx::FundamentalType::UnsignedLongInt:  os << "UnsignedLongInt";  break;
        case cxx::FundamentalType::SignedChar:       os << "SignedChar";       break;
        case cxx::FundamentalType::Char:             os << "Char";             break;
        case cxx::FundamentalType::ShortInt:         os << "ShortInt";         break;
        case cxx::FundamentalType::Int:              os << "Int";              break;
        case cxx::FundamentalType::LongInt:          os << "LongInt";          break;
        case cxx::FundamentalType::WChar_t:          os << "WChar_t";          break;
        case cxx::FundamentalType::Bool:             os << "Bool";             break;
        case cxx::FundamentalType::Float:            os << "Float";            break;
        case cxx::FundamentalType::Double:           os << "Double";           break;
        case cxx::FundamentalType::LongDouble:       os << "LongDouble";       break;
        case cxx::FundamentalType::Void:             os << "Void";             break;
        }
      os << ", " << (cvType.IsConst()? "true":"false")
         << ", " << (cvType.IsVolatile()? "true":"false") << ");\n";
      break;
    case cxx::FunctionType_id:
      {
      const cxx::FunctionType* t = cxx::FunctionType::SafeDownCast(type);
      os << "  {\n"
         << "  CvQualifiedTypes argumentTypes;\n";
      for(cxx::CvQualifiedTypes::const_iterator arg = t->GetArgumentTypes().begin();
          arg != t->GetArgumentTypes().end(); ++arg)
        {
        os << "  argumentTypes.push_back(CvType< "
           << arg->GetName().c_str()
           << " >::type);\n";
        }      
      os << "  CvType< " << cvType.GetName().c_str()
         << " >::type = TypeInfo::GetFunctionType(CvType< "
         << t->GetReturnType().GetName().c_str()
         << " >::type, argumentTypes, " << (cvType.IsConst()? "true":"false")
         << ", " << (cvType.IsVolatile()? "true":"false") << ");\n"
         << "  }\n";
      }; break;
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
    case cxx::Undefined_id:
    default: break;
    }
}

/**
 * Called internally by GenerateClasses().
 *
 * Generates the
 *   typedef ArgumentAs...<T> ArgumentFor;
 * functor type to be used when passing an argument of the given type
 * to a wrapped function.
 */
void
CvTypeGenerator
::GenerateArgumentAs(std::ostream& os,
                     const cxx::CvQualifiedType& cvType) const
{
  const cxx::Type* type = cvType.GetType();
  switch (type->GetRepresentationType())
    {
    case cxx::ArrayType_id:
      {
      os << "typedef ArgumentAsPointerTo_array< "
         << cxx::ArrayType::SafeDownCast(type)->GetElementType().GetName().c_str()
         << " > ArgumentFor; ";
      }; break;
    case cxx::ClassType_id:
    case cxx::EnumerationType_id:
    case cxx::FundamentalType_id:
      {
      os << "typedef ArgumentAsInstanceOf< "
         << cvType.GetName().c_str()
         << " > ArgumentFor; ";
      }; break;
    case cxx::PointerType_id:
      {
      cxx::CvQualifiedType pointedToType = cxx::PointerType::SafeDownCast(type)->GetPointedToType();
      if(pointedToType.IsFunctionType())
        {
        os << "typedef ArgumentAsPointerToFunction< "
           << pointedToType.GetName().c_str()
           << " > ArgumentFor; ";
        }
      else if(pointedToType.IsPointerType()
              || pointedToType.IsEnumerationType()
              || (pointedToType.IsFundamentalType()
                  && !cxx::FundamentalType::SafeDownCast(pointedToType.GetType())->IsVoid()))
        {
        os << "typedef ArgumentAsPointerTo_array< "
           << pointedToType.GetName().c_str()
           << " > ArgumentFor; ";
        }
      else
        {
        os << "typedef ArgumentAsPointerTo< "
           << pointedToType.GetName().c_str()
           << " > ArgumentFor; ";
        }
      }; break;
    case cxx::PointerToMemberType_id:
      {
      // TODO: Implement.
      }; break;
    case cxx::ReferenceType_id:
      {
      cxx::CvQualifiedType referencedType = cxx::ReferenceType::SafeDownCast(type)->GetReferencedType();
      if(referencedType.IsConst() && !referencedType.IsVolatile()
         && !(referencedType.IsClassType()
              && cxx::ClassType::SafeDownCast(referencedType.GetType())->IsAbstract()))
        {
        os << "typedef ArgumentAsReferenceTo_const< "
           << referencedType.GetType()->Name().c_str()
           << " > ArgumentFor; ";
        }
      else
        {
        os << "typedef ArgumentAsReferenceTo< "
           << referencedType.GetName().c_str()
           << " > ArgumentFor; ";
        }
      }; break;
    case cxx::FunctionType_id:
    case cxx::Undefined_id:
    default: break;
    }
}


} // namespace gen
