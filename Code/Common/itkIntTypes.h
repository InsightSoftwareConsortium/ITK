/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIntTypes.h
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
#ifndef __itkIntTypes_h
#define __itkIntTypes_h

#ifdef __cplusplus
extern "C" {
#endif

/** Convenient and more descriptive integer types. */
typedef char      ITK_INT8;
typedef int       ITK_INT32;

#ifndef WIN32
typedef long long   ITK_INT64;
#endif

#ifdef WIN32
typedef long      ITK_INT64;
#endif

typedef unsigned char ITK_UINT8;
typedef unsigned short  ITK_UINT16;
typedef unsigned    ITK_UINT32;

#ifndef WIN32
typedef unsigned long long  ITK_UINT64;
#endif

#ifdef WIN32
typedef unsigned long ITK_UINT64;
#endif

typedef int       ITK_INTPTR;
typedef unsigned    ITK_UINTPTR;

#ifdef __cplusplus
}
#endif

#endif  /* __itkIntTypes_h */

