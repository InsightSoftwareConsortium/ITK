/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapFunctionSelector.cxx
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

#include "wrapFunctionSelector.h"
#include "wrapTypeInfo.h"
#include "wrapFunctionBase.h"
#include "wrapConstructor.h"
#include "wrapMethod.h"
#include "wrapStaticMethod.h"
#include "wrapWrapperFacility.h"

#include <iostream>

namespace _wrap_
{

/**
 *
 */
FunctionSelector::FunctionSelector(const WrapperFacility* facility,
                                   int objc, Tcl_Obj*CONST objv[],
                                   unsigned int argumentCount):
  m_WrapperFacility(facility),
  m_Objc(objc),
  m_Objv(objv),
  m_ArgumentCount(argumentCount)
{
}


/**
 *
 */
FunctionSelector::~FunctionSelector()
{
  // Make sure all array arguments are freed.
  for(std::vector<Argument*>::const_iterator i = m_ArrayArguments.begin();
      i != m_ArrayArguments.end(); ++i)
    {
    delete *i;
    }
}

CvQualifiedTypes FunctionSelector::GetArgumentTypes() const
{
  CvQualifiedTypes argumentTypes;
  for(Arguments::const_iterator arg = m_Arguments.begin();
      arg != m_Arguments.end(); ++arg)
    {
    argumentTypes.push_back(arg->GetType());
    }
  return argumentTypes;
}

const Arguments& FunctionSelector::GetArguments() const
{
  return m_Arguments;
}


void FunctionSelector::SetImplicitArgument(bool staticOnly)
{
  if(staticOnly)
    {
    // If only static methods are allowed, use void as the implicit object
    // argument type so that no non-static methods can be considered.
    Argument argument;
    argument.SetType(TypeInfo::GetFundamentalType(FundamentalType::Void, false, false));
    m_Arguments.push_back(argument);
    }
  else
    {
    // Determine the type of the object.  This should be the wrapped
    // type or a subclass of it, but possibly with cv-qualifiers added.
    Argument argument = m_WrapperFacility->GetObjectArgument(m_Objv[0]);
    CvQualifiedType objectType = argument.GetType();
    if(objectType.IsPointerType())
      {
      // Automatically dereference the pointer.
      objectType = PointerType::SafeDownCast(objectType.GetType())->GetPointedToType();
      argument.SetType(objectType);
      m_Arguments.push_back(argument);
      }
    else
      {
      m_Arguments.push_back(argument);
      }
    }
}


void FunctionSelector::AddCandidate(FunctionBase* candidate)
{
  // 13.3.2/2
  // First, to be a viable function, a candidate function shall have enough
  // parameters to agree in number with the arguments in the list.
  if(candidate->GetNumberOfParameters() == m_ArgumentCount)
    {
    m_Candidates.push_back(candidate);
    m_MatchedArguments.push_back(std::vector<bool>(m_ArgumentCount));
    }
}

/**
 * THIS IS A HACK VERSION!  It should be reimplemented to do full
 * overload resolution.
 */
FunctionBase* FunctionSelector::ResolveOverload()
{
  for(unsigned int candidateIndex = 0; candidateIndex < m_Candidates.size();
      ++candidateIndex)
    {
    if(this->CandidateIsViable(candidateIndex, m_Arguments))
      {
      return m_Candidates[candidateIndex];
      }
    }
  return NULL;
}


/**
 * THIS IS A HACK VERSION!  It should be reimplemented to do full
 * overload resolution.
 */
FunctionBase* FunctionSelector::ResolveOverloadWithSeparateArguments()
{
  for(unsigned int candidateIndex = 0; candidateIndex < m_Candidates.size();
      ++candidateIndex)
    {
    if(this->CandidateIsViable(candidateIndex,
                               m_CandidateArguments[candidateIndex]))
      {
      m_Arguments = m_CandidateArguments[candidateIndex];
      return m_Candidates[candidateIndex];
      }
    }
  return NULL;
}


/**
 * 13.3.2 Viable Functions
 */
bool FunctionSelector::CandidateIsViable(unsigned int candidateIndex,
                                         const Arguments& arguments)
{
  bool viable = true;
  
  // 13.3.2/3
  // Second, for F to be a viable function, there shall exist for each
  // argument an implicit conversion sequence (13.3.3.1) that converts
  // that argument to the corresponding parameter of F.
  const FunctionBase::ParameterTypes& parameterTypes =
    m_Candidates[candidateIndex]->GetParameterTypes();
  
  unsigned int parameterIndex = 0;
  for(Arguments::const_iterator argument = arguments.begin();
      argument != arguments.end(); ++argument, ++parameterIndex)
    {
    CvQualifiedType from = argument->GetType();
    const Type* to = parameterTypes[parameterIndex];
    if(this->CxxConversionPossible(from, to))
      {
      m_MatchedArguments[candidateIndex][parameterIndex] = true;
      }
    else
      {        
      // If the "to" type is void and this is the first argument,
      // assume that it matches.  THIS IS A HACK for static methods to match
      // the implicit object parameter to any object (13.3.1/4).
      if((parameterIndex == 0) && to->IsFundamentalType()
         && FundamentalType::SafeDownCast(to)->IsVoid())
        {
        m_MatchedArguments[candidateIndex][parameterIndex] = true;
        }
      else
        {
        m_MatchedArguments[candidateIndex][parameterIndex] = false;
        viable = false;
        }
      }
    }
  return viable;
}


/**
 * Called by ResolveOverload to determine if the given conversion can be
 * done for passing an argument to a parameter.
 */
bool FunctionSelector::CxxConversionPossible(const CvQualifiedType& from,
                                             const Type* to) const
{
  // If the type is a reference, see if it can be bound.
  if(to->IsReferenceType())
    {
    const ReferenceType* toRef = ReferenceType::SafeDownCast(to);
    if(Conversions::ReferenceCanBindAsIdentity(from, toRef)
       || Conversions::ReferenceCanBindAsDerivedToBase(from, toRef)
       || (m_WrapperFacility->GetConversion(from, to) != NULL))
      {
      return true;
      }
    // If the type references a const object (not volatile,
    // though), a temporary is allowed.  See if a conversion to
    // the referenced type is available.
    CvQualifiedType toCvType = toRef->GetReferencedType();
    if(toCvType.IsConst() && !toCvType.IsVolatile()
       && m_WrapperFacility->GetConversion(from, toCvType.GetType()))
      {
      return true;
      }
    return false;
    }
  // If the types are identical, the argument/parameter pair is valid.
  else if(Type::Equal(to, from.GetType()))
    {
    return true;
    }
  else if((to->IsEitherPointerType() && from.GetType()->IsEitherPointerType())
          && Conversions::IsValidQualificationConversion(PointerType::SafeDownCast(from.GetType()),
                                                         PointerType::SafeDownCast(to)))
    {
    return true;
    }
  else if(to->IsPointerType() && from.IsArrayType()
          && (PointerType::SafeDownCast(to)->GetPointedToType()
            == ArrayType::SafeDownCast(from.GetType())->GetElementType()))
    {
    return true;
    }
  else if((to->IsArrayType() && from.GetType()->IsPointerType())
          && Conversions::IsValidQualificationConversion(
            PointerType::SafeDownCast(from.GetType()),
            PointerType::SafeDownCast(TypeInfo
                                      ::GetPointerType(ArrayType::SafeDownCast(to)->GetElementType(),
                                                       false, false).GetType())))
    {
    return true;
    }
  else if(m_WrapperFacility->GetConversion(from, to) != NULL)
    {
    return true;
    }
  return false;
}

/**
 *
 */
ConstructorSelector::ConstructorSelector(const WrapperFacility* facility,
                                         int objc, Tcl_Obj*CONST objv[]):
  FunctionSelector(facility, objc, objv, objc-1)
{
}


/**
 *
 */
ConstructorSelector::~ConstructorSelector()
{
}

void ConstructorSelector::AddCandidate(Constructor* candidate)
{
  this->FunctionSelector::AddCandidate(candidate);
}

Constructor* ConstructorSelector::Select()
{
  // Guess the argument types and try to do C++ overload resolution.
  // This should be successful most of the time.
  this->GuessArguments();
  FunctionBase* constructor = this->ResolveOverload();
  // If a method was selected, return it.
  if(constructor)
    {
    return dynamic_cast<Constructor*>(constructor);
    }
  // No methods matched.  Try some magic.
  for(unsigned int candidateIndex = 0;
      candidateIndex != m_Candidates.size(); ++candidateIndex)
    {
    m_CandidateArguments.push_back(m_Arguments);
    this->TryMagic(candidateIndex);
    }
  return dynamic_cast<Constructor*>(this->ResolveOverloadWithSeparateArguments());
}


void ConstructorSelector::GuessArguments()
{
  for(int i=1; i < m_Objc; ++i)
    {
    m_Arguments.push_back(m_WrapperFacility->GetObjectArgument(m_Objv[i]));
    }  
}


/**
 *
 */
MethodSelector::MethodSelector(const WrapperFacility* facility,
                               int objc, Tcl_Obj*CONST objv[]):
  FunctionSelector(facility, objc, objv, objc-1)
{
}


/**
 *
 */
MethodSelector::~MethodSelector()
{
}


void MethodSelector::AddCandidate(Method* candidate)
{
  this->FunctionSelector::AddCandidate(candidate);
}


Method* MethodSelector::Select(bool staticOnly)
{
  // Guess the argument types and try to do C++ overload resolution.
  // This should be successful most of the time.
  this->SetImplicitArgument(staticOnly);
  this->GuessArguments();
  FunctionBase* method = this->ResolveOverload();
  // If a method was selected, return it.
  if(method)
    {
    return dynamic_cast<Method*>(method);
    }
  // No methods matched.  Try some magic.
  for(unsigned int candidateIndex = 0;
      candidateIndex != m_Candidates.size(); ++candidateIndex)
    {
    m_CandidateArguments.push_back(m_Arguments);
    this->TryMagic(candidateIndex);
    }
  return dynamic_cast<Method*>(this->ResolveOverloadWithSeparateArguments());
}

bool FunctionSelector::TryMagic(int candidateIndex)
{
  for(unsigned int parameterIndex = 0;
      parameterIndex != m_Arguments.size(); ++parameterIndex)
    {
    if(!m_MatchedArguments[candidateIndex][parameterIndex])
      {
      if(!this->TryMagic(candidateIndex, parameterIndex))
        {
        return false;
        }
      }
    }
  return true;
}

void MethodSelector::GuessArguments()
{
  for(int i=2; i < m_Objc; ++i)
    {
    m_Arguments.push_back(m_WrapperFacility->GetObjectArgument(m_Objv[i]));
    }  
}


bool FunctionSelector::TryMagic(int candidateIndex, int parameterIndex)
{
  const FunctionBase::ParameterTypes&
    parameterTypes = m_Candidates[candidateIndex]->GetParameterTypes();
  
  CvQualifiedType from = m_Arguments[parameterIndex].GetType();
  const Type* to = parameterTypes[parameterIndex];

  if(to->IsReferenceType() && from.IsPointerType())
    {
    const ReferenceType* toRef = ReferenceType::SafeDownCast(to);
    CvQualifiedType fromObj = PointerType::SafeDownCast(from.GetType())->GetPointedToType();
    
    if(Conversions::ReferenceCanBindAsIdentity(fromObj, toRef)
       || Conversions::ReferenceCanBindAsDerivedToBase(fromObj, toRef))
      {
      m_CandidateArguments[candidateIndex][parameterIndex].SetType(fromObj);
      return true;
      }
    }
  if(to->IsPointerType())
    {
    CvQualifiedType pointedToType =
      PointerType::SafeDownCast(to)->GetPointedToType();
    if(pointedToType.IsPointerType()
       || pointedToType.IsEnumerationType()
       || (pointedToType.IsFundamentalType()
           && !FundamentalType::SafeDownCast(pointedToType.GetType())->IsVoid()))
      {
      Tcl_Obj* obj = m_Objv[parameterIndex+(m_Objc-m_ArgumentCount)];
      int length=0;
      Tcl_Obj** elementObjs;
      if((TCL_OK == Tcl_ListObjGetElements(m_WrapperFacility->GetInterpreter(),
                                           obj, &length, &elementObjs))
         && (length > 0))
        {
        Argument* elements = new Argument[length];
        for(int i=0; i < length; ++i)
          {
          elements[i] = m_WrapperFacility->GetObjectArgument(elementObjs[i]);
          if(!this->CxxConversionPossible(elements[i].GetType(),
                                          pointedToType.GetType()))
            {
            delete [] elements;
            elements = NULL;
            break;
            }
          }  
        if(elements)
          {
          m_ArrayArguments.push_back(elements);
          CvQualifiedType arrayType = TypeInfo::GetArrayType(pointedToType, length);
          m_CandidateArguments[candidateIndex][parameterIndex].SetToObject(elements, arrayType);
          return true;
          }
        }
      }
    }
  
  return false;
}

} // namespace _wrap_
