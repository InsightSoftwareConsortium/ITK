/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapAnything.h
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
#ifndef _wrapAnything_h
#define _wrapAnything_h

namespace _wrap_
{


/**
 * The general argument to a conversion function.
 * Casts between data and function pointers are not allowed.  Therfore,
 * we need to represent a general object as a class type.  Since only one
 * of the pointers is needed at a time, it can be a union.
 */
union Anything
{
  typedef void* ObjectType;
  typedef void (*FunctionType)();
  ObjectType   object;
  FunctionType function;
  
  Anything() {}
  Anything(const void* obj): object(const_cast<void*>(obj)) {}
  Anything(FunctionType func): function(func) {}
};


/**
 * The general type of a conversion function.  A real conversion function
 * will return something, but they are all cast to this for storage
 * in the table.
 */
typedef void (*ConversionFunction)(Anything);


} // namespace _wrap_

#endif
