/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxFundamentalType.h
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
#ifndef _cxxFundamentalType_h
#define _cxxFundamentalType_h

#include "cxxCvQualifiedType.h"

namespace _cxx_
{


/**
 * Represent a C++ fundamental type.  These are defined in 3.9.1.
 */
class _cxx_EXPORT FundamentalType: public Type
{
public:
  typedef FundamentalType Self;
  
  /**
   * Enumerate the fundamental types.
   */
  enum Id { UnsignedChar, UnsignedShortInt, UnsignedInt, UnsignedLongInt,
            SignedChar, Char, ShortInt, Int, LongInt, WChar_t, Bool,
            Float, Double, LongDouble, Void, NumberOfTypes};
  
  virtual RepresentationType GetRepresentationType() const;
  static FundamentalType* SafeDownCast(Type*);
  static const FundamentalType* SafeDownCast(const Type*);
  
  virtual String GenerateName(const String& indirection,
                              bool isConst, bool isVolatile) const;

  /*@{
   * Test the id of this FundamentalType.
   */
  bool IsUnsignedChar() const     { return (m_Id == UnsignedChar); }
  bool IsUnsignedShortInt() const { return (m_Id == UnsignedShortInt); }
  bool IsUnsignedInt() const      { return (m_Id == UnsignedInt); }
  bool IsUnsignedLongInt() const  { return (m_Id == UnsignedLongInt); }
  bool IsSignedChar() const       { return (m_Id == SignedChar); }
  bool IsChar() const             { return (m_Id == Char); }
  bool IsShortInt() const         { return (m_Id == ShortInt); }
  bool IsInt() const              { return (m_Id == Int); }
  bool IsLongInt() const          { return (m_Id == LongInt); }
  bool IsWChar_t() const          { return (m_Id == WChar_t); }
  bool IsBool() const             { return (m_Id == Bool); }
  bool IsFloat() const            { return (m_Id == Float); }
  bool IsDouble() const           { return (m_Id == Double); }
  bool IsLongDouble() const       { return (m_Id == LongDouble); }
  bool IsVoid() const             { return (m_Id == Void); }
  //@}
  
protected:
  FundamentalType(Id);
  FundamentalType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~FundamentalType() {}
  
private:
  /**
   * Store which integral type this is.
   */
  Id m_Id;
  
  friend class TypeSystem;
  
  static const char* fundamentalTypeNames[NumberOfTypes];
};


} // namespace _cxx_


#endif
