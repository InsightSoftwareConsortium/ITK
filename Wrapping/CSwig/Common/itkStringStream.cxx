/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStringStream.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkStringStream.h"
#include <iostream>

namespace itk
{

/**
 * Default constructor.  Use this to create a re-usable instance.
 */
StringStream::StringStream()
{
}


/**
 * Destructor will set the  result to the string value if an
 * interpreter was provided to the constructor, and GetString() and
 * Reset() were never called.
 */
StringStream::~StringStream() 
{
}


/**
 * Get the string that has been written to the stream.  This disables
 * further writing until Reset() is called.
 */
const char* StringStream::GetString()
{
  m_String = this->str();
  return m_String.c_str();
}


/**
 * Reset the stream to accept new input starting from an empty string.
 */
void StringStream::Reset()
{
  this->seekp(0, std::ios::beg);    
}

} // namespace itk
