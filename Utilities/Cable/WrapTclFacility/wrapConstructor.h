/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapConstructor.h
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
#ifndef _wrapConstructor_h
#define _wrapConstructor_h

#include "wrapFunctionBase.h"
#include "wrapArgument.h"

namespace _wrap_
{

class WrapperBase;

/**
 * Pointer to a function that implements a wrapped constructor.
 */
typedef void (*ConstructorWrapper)(const WrapperBase*, const Arguments&);

/**
 * The subclass of FunctionBase which is used for constructor
 * wrappers.
 */
class _wrap_EXPORT Constructor: public FunctionBase
{
public:
  // Pull a typedef out of the superclass.
  typedef FunctionBase::ParameterTypes ParameterTypes;
  
  Constructor(WrapperBase* wrapper,
              ConstructorWrapper constructorWrapper,
              const String& name,
              const ParameterTypes& parameterTypes = ParameterTypes());
  virtual String GetPrototype() const;
  void Call(const Arguments&) const;
private:
  const WrapperBase* m_Wrapper;
  ConstructorWrapper m_ConstructorWrapper;
};

} // namespace _wrap_

#endif
