/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    genTclGenerator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _genTclGenerator_h
#define _genTclGenerator_h

#include "genGeneratorBase.h"
#include "sourceRep.h"

namespace gen
{


/**
 * Generation class for Tcl wrappers.
 */
class TclGenerator: public GeneratorBase
{
public:
  TclGenerator(const configuration::CableConfiguration* in_config,
               const source::Namespace* in_globalNamespace):
    GeneratorBase(in_config), m_GlobalNamespace(in_globalNamespace) {}
  virtual ~TclGenerator() {}
  
  static GeneratorBase* GetInstance(const configuration::CableConfiguration*,
                                    const source::Namespace*);  
  
  virtual void Generate();  
private:
  void GeneratePackage(const configuration::Package*);  
  void GenerateIncludes(std::ostream&, const configuration::Headers*);  
  void GenerateNamespace(std::ostream&,
                         const configuration::PackageNamespace*);
  void GenerateWrapperSet(std::ostream&, const configuration::WrapperSet*);
  void GenerateClassWrapper(std::ostream&, const source::Class*);

  /**
   * The global namespace that was parsed from the source file.
   */
  const source::Namespace* m_GlobalNamespace;
};

} // namespace gen
  
#endif
