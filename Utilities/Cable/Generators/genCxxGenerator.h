/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    genCxxGenerator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _genCxxGenerator_h
#define _genCxxGenerator_h

#include "configRep.h"

namespace gen
{

typedef std::string String;

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

/**
 * Generation class for C++ wrappers.
 */
class CxxGenerator
{
public:
  CxxGenerator(const configuration::CableConfiguration* in_config):
    m_CableConfiguration(in_config) {}
  ~CxxGenerator() {}
  
  void Generate();
private:
  void GeneratePackage(const configuration::Package*);  
  void GenerateIncludes(std::ostream&, std::ostream&,
                        const configuration::Headers*);  
  void GenerateStartingNamespace(std::ostream&, std::ostream&,
                                 const configuration::PackageNamespace*);
  void GenerateNamespace(std::ostream&, std::ostream&, Indent,
                         const configuration::PackageNamespace*);
  void GenerateWrapperSet(std::ostream&, const Indent&,
                          const configuration::WrapperSet*);
  void GenerateInstantiationSet(std::ostream&, const Indent&,
                                const configuration::InstantiationSet*);

  Indent OpenNamespace(std::ostream&, std::ostream&,
                       Indent, const configuration::Namespace*) const;
  Indent CloseNamespace(std::ostream&, std::ostream&,
                        Indent, const configuration::Namespace*) const;
  
  bool MakeDirectory(const char*);

  /**
   * The configuration that controls generation.
   */
  configuration::CableConfiguration::ConstPointer m_CableConfiguration;
};

} // namespace gen
  
#endif
