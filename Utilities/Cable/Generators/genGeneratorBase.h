/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    genGeneratorBase.h
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
#ifndef _genGeneratorBase_h
#define _genGeneratorBase_h

#include "cableConfigurationRepresentation.h"
#include <iostream>

namespace gen
{

/**
 * Class to simplify indentation printing.
 */
class Indent
{
public:
  Indent(int indent): m_Indent(indent) {}
  void Print(std::ostream& os) const;
  Indent Next() const { return Indent(m_Indent+2); }
  Indent Previous() const { return Indent(m_Indent-2); }
private:
  int m_Indent;
};

std::ostream& operator<<(std::ostream&, const Indent&);
std::ostream& operator<<(std::ostream& os, const String& str);

/**
 * Defines the interface to all Generator classes.
 */
class GeneratorBase
{
public:
  GeneratorBase(const configuration::CableConfiguration* in_config):
    m_CableConfiguration(in_config) {}
  virtual ~GeneratorBase() {}
  
  virtual void Generate()=0;
protected:
  /**
   * The configuration that controls generation.
   */
  configuration::CableConfiguration::ConstPointer m_CableConfiguration;
  
  static bool MakeDirectory(const char*);
};

} // namespace gen

#endif
