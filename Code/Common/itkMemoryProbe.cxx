/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMemoryProbe.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkMemoryProbe.h"

namespace itk
{
MemoryProbe
::MemoryProbe():
  ResourceProbe< MemoryProbe::MemoryLoadType, double >("Memory", "kB")
{}

MemoryProbe
::~MemoryProbe()
{}

MemoryProbe::MemoryLoadType
MemoryProbe
::GetInstantValue(void) const
{
  return static_cast< MemoryProbe::MemoryLoadType >( m_MemoryObserver.GetMemoryUsage() );
}
} // end namespace itk
