/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapConversionTable.h
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
#ifndef _wrapConversionTable_h
#define _wrapConversionTable_h

#include "wrapUtils.h"
#include "wrapAnything.h"
#include <map>

namespace _wrap_
{

/**
 * A class to maintain a table of conversion functions for a Tcl interpreter.
 *
 * The table is keyed on the "from" and "to" types.  The top level
 * cv-qualifiers on the "from" type can affect the conversion, but
 * those on the "to" type cannot.
 *
 * 8.3.5/3 Top level cv-qualifiers on target type never matter for
 * conversions.  They only affect the parameter inside the function body.
 */
class _wrap_EXPORT ConversionTable
{
public:  
  ConversionTable();
  bool Exists(const CvQualifiedType& from, const Type* to) const;
  void SetConversion(const CvQualifiedType& from,
                     const Type* to, ConversionFunction);
  ConversionFunction GetConversion(const CvQualifiedType& from,
                                   const Type* to) const;
  
private:
  void InitializePredefinedConversions();
  
  typedef std::pair<CvQualifiedType, const Type*> ConversionKey;
  typedef std::map<ConversionKey, ConversionFunction> ConversionMap;

  /**
   * Map from type in/out pair to conversion function.
   */
  ConversionMap m_ConversionMap;
};

} // namespace _wrap_

#endif
