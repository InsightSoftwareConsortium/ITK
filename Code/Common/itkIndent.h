/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIndent.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 *
 * \ingroup OSSystemObjects
 */

class ITKCommon_EXPORT Indent
{
public:
  /** Standard class typedefs. */
  typedef Indent  Self;

  /** Method for creation through the object factory. */
  static Self* New();
  
  /** Destroy this instance. */
  void Delete() {delete this;}

  /** Construct the object with an initial indentation level. */
  Indent(int ind=0) {m_Indent=ind;}

  /** Return the name of the class. */
  static const char *GetNameOfClass() {return "Indent";}

  /** Determine the next indentation level. Keep indenting by two until the 
   * a maximum of forty spaces is reached.  */
  Indent GetNextIndent();

  /** Print out the indentation. Basically output a bunch of spaces.  */
  friend ITKCommon_EXPORT std::ostream& operator<<(std::ostream& os, const Indent& o); 

private:
  int m_Indent;
};

} // end namespace itk
  
#endif
