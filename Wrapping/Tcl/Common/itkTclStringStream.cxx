/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTclStringStream.cxx
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
#include "itkTclStringStream.h"

namespace itk
{

/**
 * Default constructor.  Use this to create a re-usable instance.
 */
TclStringStream::TclStringStream():
  m_Terminated(false),
  m_Interpreter(0)
{
}


/**
 * Use this constructor to create a temporary instance that will write
 * the string to the Tcl result upon destruction.
 */
TclStringStream::TclStringStream(Tcl_Interp* interp):
  m_Terminated(false),
  m_Interpreter(interp)
{
}


/**
 * Destructor will set the Tcl result to the string value if an
 * interpreter was provided to the constructor, and GetString() and
 * Reset() were never called.
 */
TclStringStream::~TclStringStream() 
{
  this->TerminateStream();
  if(m_Interpreter)
    {
    Tcl_ResetResult(m_Interpreter);
    Tcl_AppendResult(m_Interpreter, this->str(), 0);
    }
  this->rdbuf()->freeze(0);
}


/**
 * Get the string that has been written to the stream.  This disables
 * further writing until Reset() is called.
 */
const char* TclStringStream::GetString()
{
  m_Interpreter = 0;
  this->TerminateStream();
  return this->str();
}


/**
 * Reset the stream to accept new input starting from an empty string.
 */
void TclStringStream::Reset()
{
  m_Interpreter = 0;
  m_Terminated = false;
  this->seekp(0, std::ios::beg);    
}


/**
 * Internal method used to make sure the stream has been terminated
 * with std::ends before the string is obtained.
 */
void TclStringStream::TerminateStream()
{
  if(!m_Terminated)
    {
    m_Terminated = true;
    (*this) << std::ends;
    }
}

} // namespace itk
