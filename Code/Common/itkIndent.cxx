/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIndent.cxx
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
#include "itkIndent.h"
#include "itkObjectFactory.h"

#define ITK_STD_INDENT 2
#define ITK_NUMBER_OF_BLANKS 40

namespace itk
{

static const char blanks[ITK_NUMBER_OF_BLANKS+1] =
    "                                        ";

/**
 * Instance creation.
 */
Indent*
Indent::
New()
{
  return new Self;
}
  
  
/**
 * Determine the next indentation level. Keep indenting by two until the 
 * max of forty.
 */
Indent 
Indent
::GetNextIndent()
{
  int indent = m_Indent + ITK_STD_INDENT;
  if ( indent > ITK_NUMBER_OF_BLANKS )
    {
    indent = ITK_NUMBER_OF_BLANKS;
    }
  return indent;
}
 
/**
 * Print out the indentation. Basically output a bunch of spaces.
 */
std::ostream& 
operator<<(std::ostream& os, Indent& ind)
{
  os << blanks + (ITK_NUMBER_OF_BLANKS-ind.m_Indent) ;
  return os;
}

} // end namespace itk
