/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    genTclGenerator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "genTclGenerator.h"

#include <iostream>
#include <fstream>

namespace gen
{

using namespace configuration;


/**
 * Construct an instance of this generator and return it.
 */
GeneratorBase* TclGenerator::GetInstance(const CableConfiguration* in_config,
                                         const source::Namespace* in_globalNamespace)
{
  return new TclGenerator(in_config, in_globalNamespace);
}


/**
 * Generate Tcl wrappers for all packages specified in the configuration.
 */
void
TclGenerator
::Generate()
{
  std::cout << "TclGenerator::Generate()" << std::endl;
}


} // namespace gen
