/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkIndent_h
#define itkIndent_h

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
 *
 * \ingroup OSSystemObjects
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT Indent
{
public:
  /** Standard class typedefs. */
  typedef Indent Self;

  /** Method for creation through the object factory. */
  static Self * New();

  /** Destroy this instance. */
  void Delete() { delete this; }

  /** Construct the object with an initial indentation level. */
  Indent(int ind = 0) { m_Indent = ind; }

  /** Return the name of the class. */
  static const char * GetNameOfClass() { return "Indent"; }

  /** Determine the next indentation level. Keep indenting by two until the
   * a maximum of forty spaces is reached.  */
  Indent GetNextIndent();

  /** Print out the indentation. Basically output a bunch of spaces.  */
  friend ITKCommon_EXPORT std::ostream & operator<<(std::ostream & os, const Indent & o);

private:
  int m_Indent;
};
} // end namespace itk

#endif
