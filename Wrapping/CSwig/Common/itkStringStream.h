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


namespace itk
{

/** \Class StringStream
 *  \brief Provides access to C++ ostreams.
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
