/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapWrapperTable.cxx
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
#include "wrapWrapperTable.h"
#include "wrapException.h"

namespace _wrap_
{

/**
 * Constructor takes interpreter to which the WrapperTable will be
 * attached.
 */
WrapperTable::WrapperTable(Tcl_Interp* interp):
  m_Interpreter(interp)
{
}


/**
 * Return whether a wrapper for the given type exists.
 */
bool WrapperTable::Exists(const Type* type) const
{
  return (m_WrapperMap.count(type) > 0);
}
  

/**
 * Register a wrapper for the given type.
 */
void WrapperTable::SetWrapper(const Type* type, WrapperBase* wrapper)
{
  m_WrapperMap[type] = wrapper;
}
  
 
/**
 * Retrieve the wrapper for the given type.  If none exists, NULL is
 * returned.
 */
WrapperBase*
WrapperTable::GetWrapper(const Type* type) const
{
  WrapperMap::const_iterator i = m_WrapperMap.find(type);
  if(i != m_WrapperMap.end())
    {
    return i->second;
    }
  return NULL;
}


/**
 * Get an WrapperTable object set up to deal with the given Tcl interpreter.
 * If one exists, it will be returned.  Otherwise, a new one will be
 * created.
 */
WrapperTable* WrapperTable::GetForInterpreter(Tcl_Interp* interp)
{
  // See if an WrapperTable exists for the given interpreter.
  if(interpreterWrapperTableMap.count(interp) == 0)
    {
    // No, we must create a new WrapperTable for this interpreter.
    interpreterWrapperTableMap[interp] = new WrapperTable(interp);
    }
  
  // Return the WrapperTable.
  return interpreterWrapperTableMap[interp];  
}


/**
 * Map from a Tcl interpreter to the WrapperTable for it.
 */
WrapperTable::InterpreterWrapperTableMap WrapperTable::interpreterWrapperTableMap;

} // namespace _wrap_
