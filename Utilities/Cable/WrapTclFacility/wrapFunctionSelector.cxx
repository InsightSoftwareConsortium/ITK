/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapFunctionSelector.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include "wrapFunctionSelector.h"
#include "wrapTypeInfo.h"
#include "wrapWrapperBase.h"

namespace _wrap_
{

/**
 *
 */
FunctionSelector::FunctionSelector(const WrapperBase* wrapper,
                                   int objc, Tcl_Obj*CONST objv[]):
  m_Wrapper(wrapper),
  m_Objc(objc),
  m_Objv(objv)
{
}


/**
 *
 */
FunctionSelector::~FunctionSelector()
{
}

const CvQualifiedTypes& FunctionSelector::GetArgumentTypes() const
{
  return m_ArgumentTypes;
}

void FunctionSelector::AddCandidateMethod(FunctionBase* candidate)
{
  // 13.3.2/2
  // First, to be a viable function, a candidate function shall have enough
  // parameters to agree in number with the arguments in the list.
  if(candidate->GetNumberOfParameters() == (m_Objc-1))
    {
    m_Candidates.push_back(candidate);
    m_MatchedArguments.push_back(std::vector<bool>(m_Objc-1));
    }
}

void FunctionSelector::AddCandidateConstructor(FunctionBase* candidate)
{
  // 13.3.2/2
  // First, to be a viable function, a candidate function shall have enough
  // parameters to agree in number with the arguments in the list.
  if(candidate->GetNumberOfParameters() == (m_Objc-2))
    {
    m_Candidates.push_back(candidate);
    m_MatchedArguments.push_back(std::vector<bool>(m_Objc-2));
    }
}

FunctionBase* FunctionSelector::SelectConstructor()
{
  this->GuessArgumentTypes();
  return this->ResolveOverload();
}

FunctionBase* FunctionSelector::SelectMethod(bool staticOnly)
{
  this->SetImplicitArgumentType(staticOnly);
  this->GuessArgumentTypes();
  return this->ResolveOverload();
}

void FunctionSelector::SetImplicitArgumentType(bool staticOnly)
{
  CvQualifiedType objectType;
  if(!staticOnly)
    {
    // Determine the type of the object.  This should be the wrapped
    // type or a subclass of it, but possibly with cv-qualifiers added.
    objectType = m_Wrapper->GetObjectType(m_Objv[0]);
    }
  else
    {
    // If only static methods are allowed, use void as the implicit object
    // argument type so that no non-static methods can be considered.
    objectType = TypeInfo::GetFundamentalType(FundamentalType::Void, false, false);
    }

  // Add the implicit object argument.
  if(objectType.IsPointerType())
    {
    // Automatically dereference the pointer.
    m_ArgumentTypes.push_back(PointerType::SafeDownCast(objectType.GetType())->GetPointedToType());
    }
  else
    {
    m_ArgumentTypes.push_back(objectType);
    }
}


void FunctionSelector::GuessArgumentTypes()
{
  for(int i=2; i < m_Objc; ++i)
    {
    m_ArgumentTypes.push_back(m_Wrapper->GetObjectType(m_Objv[i]));
    }  
}



/**
 * THIS IS A HACK VERSION!  It should be reimplemented to do full
 * overload resolution.
 */
FunctionBase* FunctionSelector::ResolveOverload()
{
  // Construct a set of viable functions from the set of candidates.
  std::vector<unsigned int> viableFunctions;
  
  // 13.3.2 Viable Functions
  for(unsigned int candidateIndex = 0; candidateIndex < m_Candidates.size();
      ++candidateIndex)
    {
    bool viable = true;
    
    // 13.3.2/3
    // Second, for F to be a viable function, there shall exist for each
    // argument an implicit conversion sequence (13.3.3.1) that converts
    // that argument to the corresponding parameter of F.
    const FunctionBase::ParameterTypes& parameterTypes =
      m_Candidates[candidateIndex]->GetParameterTypes();
    
    unsigned int parameterIndex = 0;
    for(CvQualifiedTypes::const_iterator argument = m_ArgumentTypes.begin();
        argument != m_ArgumentTypes.end(); ++argument, ++parameterIndex)
      {
      CvQualifiedType from = *argument;
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
        if((argument == m_ArgumentTypes.begin()) && to->IsFundamentalType()
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

    if(viable)
      {
      viableFunctions.push_back(candidateIndex);
      }
    }
  
  // If no viable functions remain, return NULL.
  if(viableFunctions.empty())
    {
    return NULL;
    }
  
  // Skip the rest of overload resolution.  Just take the first viable
  // function.  
  return m_Candidates[viableFunctions[0]];
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
       || (m_Wrapper->GetConversionFunction(from, to) != NULL))
      {
      return true;
      }
    // If the type references a const object (not volatile,
    // though), a temporary is allowed.  See if a conversion to
    // the referenced type is available.
    CvQualifiedType toCvType = toRef->GetReferencedType();
    if(toCvType.IsConst() && !toCvType.IsVolatile()
       && m_Wrapper->GetConversionFunction(from, toCvType.GetType()))
      {
      return true;
      }
    return false;
    }
  // If the types are identical, the argument/parameter pair is valid.
  else if(to->Id() == from.GetType()->Id())
    {
    return true;
    }
  else if((to->IsEitherPointerType() && from.GetType()->IsEitherPointerType())
          && Conversions::IsValidQualificationConversion(PointerType::SafeDownCast(from.GetType()),
                                                         PointerType::SafeDownCast(to)))
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
  else if(m_Wrapper->GetConversionFunction(from, to) != NULL)
    {
    return true;
    }
  return false;
}



} // namespace _wrap_
