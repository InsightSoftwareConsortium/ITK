/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStringStream.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkTclStringStream_h
#define _itkTclStringStream_h

// Need to include at least one ITK header.
#include "itkMacro.h"

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
 *    $obj Print [itk::TclStringStream [cable::Interpreter]]
 *
 * In this example, the destructor is invoked after the call to
 * Print(std::ostream&), which sets the Tcl result to the string
 * written to the ostream by Print.  The construction of the
 * TclStringStream can be hidden in a Tcl procedure:
 *
 *    proc itkTclResultStream {} {
 *      return [itk::TclStringStream [cable::Interpreter]]
 *    }
 *
 *    $obj Print [itkTclResultStream]
 *    
 */
class StringStream: public itk::OStringStream
{
public:
  typedef StringStream Self;
  typedef itk::OStringStream Superclass;
  
  StringStream();
  ~StringStream();
  std::ostream& GetStream() { return *this;}
  const char* GetString();
  void Reset();
private:
  std::string m_String;
  StringStream(const StringStream&); // Not implemented.
  void operator=(const StringStream&); // Not implemented.
  
};

}

#endif
