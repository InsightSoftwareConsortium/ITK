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
 * itkIndent is used to control indentation during the chaining print 
 * process. This way nested objects can correctly indent themselves.
 * This class works with the print methods defined in itkObject (i.e.,
 * the public method Print() and the protected methods PrintSelf(),
 * PrintHeader(), and PrintTrailer().
 */

#ifndef __itkIndent_h
#define __itkIndent_h

#include "itkWin32Header.h"
#include <iostream>

class ITK_EXPORT itkIndent
{
public:
  /**
   * Create an instance of this class.
   */
  static itkIndent *New();
  
  /**
   * Destroy this instance.
   */
  void Delete() {delete this;}

  /** 
   * Construct the object with an initial indentation level.
   */
  itkIndent(int ind=0) {m_Indent=ind;}

  /**
   * Return the name of the class.
   */
  static const char *GetClassName() {return "itkIndent";}

  /** 
   * Determine the next indentation level. Keep indenting by two until the 
   * a maximum of forty spaces is reached. 
   */
  itkIndent GetNextIndent();

  /** 
   * Print out the indentation. Basically output a bunch of spaces. 
   */
  friend ITK_EXPORT std::ostream& operator<<(std::ostream& os, itkIndent& o); 

private:
  int m_Indent;
};

#endif
