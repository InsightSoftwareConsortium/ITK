/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cableXmlAttributes.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cableXmlAttributes_h
#define _cableXmlAttributes_h

#include "cableXmlParseException.h"

#include <map>

namespace xml
{


/**
 * Store the set of attributes provided to an element tag, and their values.
 */
class Attributes
{
public:
  void Set(const String& a, const String& v);
  const char* Get(const String& a) const;
  int GetAsInteger(const String& a) const;
  bool GetAsBoolean(const String& a) const;
  bool Have(const String& a) const;
  
private:
  /**
   * Map from attribute name to its value.
   */
  std::map<String, String>  m_Attrs;
};


/**
 * An attribute requested from an element begin tag is not known.
 */
class MissingAttributeException: public ParseException
{
public:
  MissingAttributeException(const char* missing):
    ParseException(), m_Missing(missing) {}
  virtual ~MissingAttributeException() {}
  
  void Print(std::ostream& os) const
    {
      os << "Missing element attribute: " << m_Missing.c_str()
         << std::endl;
    }
private:
  String m_Missing;
};


} // namespace xml
  
#endif
