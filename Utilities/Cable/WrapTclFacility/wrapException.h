/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapException.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapException_h
#define _wrapException_h

#include "wrapUtils.h"

namespace _wrap_
{

class TclException
{
public:
  TclException(const String& message): m_Message(message) {}
  TclException(const String& message, const String& target):
    m_Message(message+": "+target) {}
  const String& GetMessage() { return m_Message; }
private:
  String m_Message;
};

#define _wrap_UndefinedInstanceNameException(x) \
  TclException("Undefined instance name", x)  
#define _wrap_UndefinedObjectTypeException(x) \
  TclException("Undefined object type", x)
#define _wrap_UnknownConversionException(x, y) \
  TclException("Don't know how to convert "+String(x)+" to "+String(y))

} // namespace _wrap_

#endif
