/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMException.h
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
#ifndef __itkFEMException_h
#define __itkFEMException_h


// FEM classes need to use bad_cast exception and other typeinfo stuff.
// For some reason bad_cast excepction doesn't work on MSVC compiler if
// we include <typeinfo>, but does work when we include <typeinfo.h>.
// However on some some systems (linux???) <typeinfo.h> doesn't exist, so
// we must include <typeinfo> instead.
#ifdef _MSC_VER
#include <typeinfo.h>
#else
#include <typeinfo>
#endif

#include "itkExceptionObject.h"
#include <string>

namespace itk {
namespace fem {




/**
 * \file itkFEMException.h
 * \brief Declaration of several exception classes that are used
    within the FEM code.
 */




/**
 * \class FEMException
 * \brief Base class for all exception's that can occur within FEM classes.
 */
class FEMException : public ::itk::ExceptionObject
{
public:
  /**
   * Constructor. Must provide a string, that specifies name of
   * the file where the exception occured and an integer for the
   * line number. An optional argument specifies the location
   * (usually the name of the class and member function). Normally
   * you should use __FILE__ and __LINE__ macros to specify file name
   * and line number.
   */
  FEMException(const char *file, unsigned int lineNumber, std::string location="Unknown");

};




/**
 * \class FEMExceptionIO
 * \brief Base class for all IO exception's that can occur within FEM classe.
 *
 * This class is normally used when reading or writing objects from/to stream.
 */
class FEMExceptionIO : public FEMException
{
public:
  /**
   * Constructor. In order to construct this exception object, four parameters
   * must be provided: file, lineNumber, location and a detailed description
   * of the exception.
   */
  FEMExceptionIO(const char *file, unsigned int lineNumber, std::string location, std::string moreDescription);

};




/**
 * \class FEMExceptionWrongClass
 * \brief Bad object exception.
 * 
 * This exception occures, when a the pointer that was passed to a
 * function or member, was pointing to the wrong class of object.
 * Usially this means that the dynamic_cast operator failed.
 * This exception object can be generated automatically from the
 * bad_cast exception. Information about the file and line number
 * are in this case not available.
 */
class FEMExceptionWrongClass : public FEMException
{
public:
  FEMExceptionWrongClass(const char *file, unsigned int lineNumber, std::string location);

};




/**
 * \class FEMExceptionObjectNotFound
 * \brief Object not found exception.
 *
 * This exception occures, when a search for an object with given
 * global number was unsuccessful.
 */
class FEMExceptionObjectNotFound : public FEMException
{
public:
  FEMExceptionObjectNotFound(const char *file, unsigned int lineNumber, std::string location, std::string baseClassName, int GN);

  /**
   * Base class of the searched object.
   */
  std::string m_baseClassName;
  int m_GN;

};




/**
 * \class FEMExceptionSolution
 * \brief Base class for all exceptions that can occur when solving FEM problem.
 *
 * This class is normally used when an error occurs while the problem is
 * already in memory and something went wrong while trying to solve it.
 */
class FEMExceptionSolution : public FEMException
{
public:
  /**
   * Constructor. In order to construct this exception object, four parameters
   * must be provided: file, lineNumber, location and a detailed description
   * of the exception.
   */
  FEMExceptionSolution(const char *file, unsigned int lineNumber, std::string location, std::string moreDescription);
 
};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMException_h
