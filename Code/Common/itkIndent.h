/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIndent.h
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
#ifndef __itkIndent_h
#define __itkIndent_h

#include "itkMacro.h"
#include <iostream>

namespace itk
{

/** \class Indent
 * \brief Control indentation during Print() invocation.
 *
 * Indent is used to control indentation during the chaining print 
 * process. This way nested objects can correctly indent themselves.
 * This class works with the print methods defined in Object (i.e.,
 * the public method Print() and the protected methods PrintSelf(),
 * PrintHeader(), and PrintTrailer().
 */

class ITK_EXPORT Indent
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Indent  Self;

  /**
   * Method for creation through the object factory.
   */
  static Self* New();
  
  /**
   * Destroy this instance.
   */
  void Delete() {delete this;}

  /** 
   * Construct the object with an initial indentation level.
   */
  Indent(int ind=0) {m_Indent=ind;}

  /**
   * Return the name of the class.
   */
  static const char *GetClassName() {return "Indent";}

  /** 
   * Determine the next indentation level. Keep indenting by two until the 
   * a maximum of forty spaces is reached. 
   */
  Indent GetNextIndent();

  /** 
   * Print out the indentation. Basically output a bunch of spaces. 
   */
  friend ITK_EXPORT std::ostream& operator<<(std::ostream& os, Indent& o); 

private:
  int m_Indent;
};

} // end namespace itk
  
#endif
