/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapFunctionSelector.h
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
#ifndef _wrapFunctionSelector_h
#define _wrapFunctionSelector_h

#include "wrapArgument.h"

#include <vector>

namespace _wrap_
{

class WrapperBase;
class FunctionBase;
class Constructor;
class Method;
class StaticMethod;
class WrapperFacility;

/**
 * 
 */
class _wrap_EXPORT FunctionSelector
{
public:
  FunctionSelector(const WrapperFacility*, int, Tcl_Obj*CONST[], unsigned int);
  virtual ~FunctionSelector();
  
  CvQualifiedTypes GetArgumentTypes() const;
  
  const Arguments& GetArguments() const;
protected:  
  typedef std::vector<FunctionBase*> CandidateFunctions;
  void SetImplicitArgument(bool staticOnly);
  void AddCandidate(FunctionBase*);
  FunctionBase* ResolveOverload();
  FunctionBase* ResolveOverloadWithSeparateArguments();
  bool CandidateIsViable(unsigned int candidateIndex,
                         const Arguments& arguments);
  bool CxxConversionPossible(const CvQualifiedType& from,
                             const Type* to) const;
  bool TryMagic(int candidateIndex);
  bool TryMagic(int candidateIndex, int parameterIndex);  
  
  const WrapperFacility* m_WrapperFacility;
  int m_Objc;
  Tcl_Obj*CONST* m_Objv;
  unsigned int m_ArgumentCount;
  
  CandidateFunctions m_Candidates;
  std::vector< std::vector<bool> >  m_MatchedArguments;
  std::vector< Arguments >  m_CandidateArguments;
  Arguments m_Arguments;
  std::vector< Argument* > m_ArrayArguments;
};


class _wrap_EXPORT ConstructorSelector: public FunctionSelector
{
public:
  ConstructorSelector(const WrapperFacility*, int, Tcl_Obj*CONST[]);
  ~ConstructorSelector();
  
  void AddCandidate(Constructor*);
  Constructor* Select();
protected:
  void GuessArguments();  
};

class _wrap_EXPORT MethodSelector: public FunctionSelector
{
public:
  MethodSelector(const WrapperFacility*, int, Tcl_Obj*CONST[]);
  ~MethodSelector();
  
  void AddCandidate(Method*);
  Method* Select(bool);
protected:
  void GuessArguments();  
};


} // namespace _wrap_

#endif
