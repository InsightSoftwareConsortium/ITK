/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIndent.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * Indent is used to control indentation during the chaining print 
 * process. This way nested objects can correctly indent themselves.
 * This class works with the print methods defined in Object (i.e.,
 * the public method Print() and the protected methods PrintSelf(),
 * PrintHeader(), and PrintTrailer().
 */

#ifndef __itkIndent_h
#define __itkIndent_h

#include "itkWin32Header.h"
#include <iostream>

namespace itk
{

class ITK_EXPORT Indent
{
public:
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

} // namespace itk
  
#endif
