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

#include "genGeneratorBase.h"

namespace gen
{


/**
 * Generation class for C++ wrappers.
 */
class CxxGenerator: public GeneratorBase
{
public:
  CxxGenerator(const configuration::CableConfiguration* in_config):
    GeneratorBase(in_config) {}
  virtual ~CxxGenerator() {}
  
  static GeneratorBase* GetInstance(const configuration::CableConfiguration*);  
  
  virtual void Generate();  
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
};

} // namespace gen
  
#endif
