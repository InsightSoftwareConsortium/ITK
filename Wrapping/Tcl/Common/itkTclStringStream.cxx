/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTclStringStream.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTclStringStream.h"

namespace itk
{

/**
 * Default constructor.  Use this to create a re-usable instance.
 */
TclStringStream::TclStringStream():
  m_Interpreter(0)
{
}


/**
 * Use this constructor to create a temporary instance that will write
 * the string to the Tcl result upon destruction.
 */
TclStringStream::TclStringStream(Tcl_Interp* interp):
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
  if(m_Interpreter)
    {
    Tcl_ResetResult(m_Interpreter);
    Tcl_AppendResult(m_Interpreter,
                     const_cast<char*>(this->str().c_str()), 0);
    }
}


/**
 * Get the string that has been written to the stream.  This disables
 * further writing until Reset() is called.
 */
const char* TclStringStream::GetString()
{
  m_Interpreter = 0;
  return this->str().c_str();
}


/**
 * Reset the stream to accept new input starting from an empty string.
 */
void TclStringStream::Reset()
{
  m_Interpreter = 0;
  this->seekp(0, std::ios::beg);    
}

} // namespace itk
