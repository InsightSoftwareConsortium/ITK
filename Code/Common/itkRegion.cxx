/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegion.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkRegion.h"

namespace itk
{

/**
 * Instantiate object.
 */
Region
::Region()
{
}

/**
 * Destructor for the Region class.
 */
Region
::~Region()
{
}

/**
 * Print internal instance variables.
 */
void 
Region
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

