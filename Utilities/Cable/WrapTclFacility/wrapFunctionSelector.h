/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapFunctionSelector.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapFunctionSelector_h
#define _wrapFunctionSelector_h

#include "wrapFunctionBase.h"

#include <vector>

namespace _wrap_
{

class WrapperBase;

/**
 * 
 */
class _wrap_EXPORT FunctionSelector
{
public:
  FunctionSelector(const WrapperBase* wrapper, int objc, Tcl_Obj*CONST objv[]);
  virtual ~FunctionSelector();
  
  const CvQualifiedTypes& GetArgumentTypes() const;
  
  void AddCandidateConstructor(FunctionBase*);
  void AddCandidateMethod(FunctionBase*);

  FunctionBase* SelectConstructor();
  FunctionBase* SelectMethod(bool);
  
  typedef std::vector<FunctionBase*> CandidateFunctions;
private:
  void SetImplicitArgumentType(bool staticOnly);
  void GuessArgumentTypes();
  FunctionBase* ResolveOverload();
  bool CxxConversionPossible(const CvQualifiedType& from,
                             const Type* to) const;

  
  const WrapperBase* m_Wrapper;
  CandidateFunctions m_Candidates;
  std::vector< std::vector<bool> >  m_MatchedArguments;
  CvQualifiedTypes m_ArgumentTypes;
  int m_Objc;
  Tcl_Obj*CONST* m_Objv;
};

} // namespace _wrap_

#endif
