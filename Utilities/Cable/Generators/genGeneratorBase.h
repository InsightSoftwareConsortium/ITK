/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    genGeneratorBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _genGeneratorBase_h
#define _genGeneratorBase_h

#include "cableConfigurationRepresentation.h"
#include <iostream>

namespace gen
{

/**
 * Class to simplify indentation printing.
 */
class Indent
{
public:
  Indent(int indent): m_Indent(indent) {}
  void Print(std::ostream& os) const;
  Indent Next() const { return Indent(m_Indent+2); }
  Indent Previous() const { return Indent(m_Indent-2); }
private:
  int m_Indent;
};

std::ostream& operator<<(std::ostream&, const Indent&);
std::ostream& operator<<(std::ostream& os, const String& str);

/**
 * Defines the interface to all Generator classes.
 */
class GeneratorBase
{
public:
  GeneratorBase(const configuration::CableConfiguration* in_config):
    m_CableConfiguration(in_config) {}
  virtual ~GeneratorBase() {}
  
  virtual void Generate()=0;
protected:
  /**
   * The configuration that controls generation.
   */
  configuration::CableConfiguration::ConstPointer m_CableConfiguration;
  
  static bool MakeDirectory(const char*);
};

} // namespace gen

#endif
