/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegion.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkRegion.h"

namespace itk
{
void
Region
::Print(std::ostream & os, Indent indent) const
{
  this->PrintHeader(os, indent);
  this->PrintSelf( os, indent.GetNextIndent() );
  this->PrintTrailer(os, indent);
}

void
Region
::PrintHeader(std::ostream & os, Indent indent) const
{
  os << indent << this->GetNameOfClass() << " (" << this << ")\n";
}

void
Region
::PrintTrailer( std::ostream & itkNotUsed(os), Indent itkNotUsed(indent) ) const
{}

void
Region
::PrintSelf(std::ostream &, Indent) const
{}
} // end namespace itk
