/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapPointerToPointerMap.cxx
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
#include "wrapPointerToPointerMap.h"

#include <map>

namespace _wrap_
{

typedef std::map<const void*, void*> StlInternalMap;

/**
 * This class hides the STL map used to implement the
 * PointerToPointerMap so that it only appears here, in the .cxx file.
 */
struct PointerToPointerMap::InternalMap: public StlInternalMap
{
  typedef StlInternalMap::value_type value_type;
  typedef StlInternalMap::const_iterator const_iterator;
};


/**
 * The constructor creates the internal representation of the map.
 */
PointerToPointerMap::PointerToPointerMap(): m_InternalMap(new InternalMap)
{
}


/**
 * The destructor destroys the internal representation of the map.
 */
PointerToPointerMap::~PointerToPointerMap()
{
  delete m_InternalMap;
}


/**
 * Set the entry for the given key to the given value.
 */
void PointerToPointerMap::Set(const void* key, void* value)
{
  m_InternalMap->insert(InternalMap::value_type(key, value));
}


/**
 * Return the pointer stored for the given key.  If no such key
 * exists, a NULL pointer is returned.  Use
 * PointerToPointerMap::Contains to disambiguate between a NULL
 * pointer returned for this reason and a NULL pointer stored in the
 * map.
 */
void* PointerToPointerMap::Get(const void* key) const
{
  InternalMap::const_iterator i = m_InternalMap->find(key);
  if(i != m_InternalMap->end())
    {
    return i->second;
    }
  else
    {
    return 0;
    }
}


/**
 * Return whether the map contains an entry with the given key.
 */
bool PointerToPointerMap::Contains(const void* key) const
{
  return (m_InternalMap->find(key) != m_InternalMap->end());
}

} // namespace _wrap_
