/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTclStringStream.h
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
#ifndef _itkTclStringStream_h
#define _itkTclStringStream_h

// Need to include at least one ITK header.
#include "itkMacro.h"

#include <strstream>
#include <tcl.h>

namespace itk
{

/** \Class TclStringStream
 *  \brief Provides access to C++ ostreams from Tcl.
 *
 * An instance of this class can be passed as an argument to any
 * method expecting std::ostream&.  The output written to the stream
 * is recorded and can be retrieved in Tcl.  There are two intended uses:
 *
 * 1.)  Use the default constructor to create an instance.  After
 *      all writing to it is done, call "GetString()" to get the result.
 *      If the stream will be used again, call "Reset()" to prepare it for
 *      a new string.
 *
 *    set s [itk::TclStringStream]
 *    $obj Print $s
 *    puts "Result: [$s GetString]"
 *    $s Reset
 *    $obj2 Print $s
 *    puts "Result2: [$s GetString]"
 *
 * 2.)  Use the constructor taking a Tcl interpreter as an argument.  After
 *      writing has been done, destroying the object will set the Tcl
 *      interpreter's result to the value of the string:
 *
 *    $obj Print [itk::TclStringStream [wrap::Interpreter]]
 *
 * In this example, the destructor is invoked after the call to
 * Print(std::ostream&), which sets the Tcl result to the string
 * written to the ostream by Print.  The construction of the
 * TclStringStream can be hidden in a Tcl procedure:
 *
 *    proc itkTclResultStream {} {
 *      return [itk::TclStringStream [wrap::Interpreter]];
 *    }
 *
 *    $obj Print [itkTclResultStream]
 *    
 */
class TclStringStream: public std::ostrstream
{
public:
  typedef TclStringStream Self;
  typedef std::ostrstream Superclass;
  
  TclStringStream();
  TclStringStream(Tcl_Interp*);
  ~TclStringStream();
  
  const char* GetString();
  void Reset();
private:
  TclStringStream(const TclStringStream&); // Not implemented.
  void operator=(const TclStringStream&); // Not implemented.
  
  void TerminateStream();
  
  bool m_Terminated;
  Tcl_Interp* m_Interpreter;
};

}

#endif
