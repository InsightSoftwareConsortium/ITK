/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapArgument.h
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
#ifndef _wrapArgument_h
#define _wrapArgument_h

#include "wrapUtils.h"
#include "wrapAnything.h"

#include <vector>

namespace _wrap_
{

/**
 * Holds an argument after extraction from a Tcl object, but before
 * passing to the final conversion function.  This may hold a pointer
 * to the actual object, or an instance of a fundamental type.
 */
class _wrap_EXPORT Argument
{
public:
  Argument();
  Argument(const Argument&);
  Argument& operator=(const Argument&);

  typedef Anything::ObjectType   ObjectType;
  typedef Anything::FunctionType FunctionType;
  
  Anything GetValue() const;
  const CvQualifiedType& GetType() const;
  void SetType(const CvQualifiedType&);
  void SetToObject(ObjectType object, const CvQualifiedType& type);
  void SetToBool(bool);
  void SetToInt(int);
  void SetToLong(long);
  void SetToDouble(double);
  void SetToPointer(ObjectType v, const CvQualifiedType& pointerType);
  void SetToFunction(FunctionType f,
                     const CvQualifiedType& functionPointerType);
private:
  enum ArgumentId { Uninitialized_id=0, Object_id, Pointer_id, Function_id,
                    bool_id, int_id, long_id, double_id };
  
  /**
   * The pointer to the actual object.
   */
  Anything m_Anything;

  /**
   * The type of the object.
   */
  CvQualifiedType m_Type;

  /**
   * Which type of Argument this is.
   */
  ArgumentId m_ArgumentId;
  
  /**
   * If a temporary is needed to hold the value extracted from the
   * Tcl object, this will hold it.
   */
  union
  {
    bool m_bool;
    int m_int;
    long m_long;
    double m_double;
  } m_Temp;
};

  
/**
 * Represent function arguments.
 */
typedef std::vector<Argument> Arguments;
  
} // namespace _wrap_

#endif
