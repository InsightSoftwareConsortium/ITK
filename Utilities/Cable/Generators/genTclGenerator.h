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
  const source::Namespace* m_GlobalNamespace;
};

} // namespace gen
  
#endif
