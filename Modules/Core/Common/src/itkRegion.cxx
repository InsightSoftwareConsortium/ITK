/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkRegion.h"

namespace itk
{
void
Region::Print(std::ostream & os, Indent indent) const
{
  this->PrintHeader(os, indent);
  this->PrintSelf(os, indent.GetNextIndent());
  this->PrintTrailer(os, indent);
}

void
Region::PrintHeader(std::ostream & os, Indent indent) const
{
  os << indent << this->GetNameOfClass() << " (" << this << ")\n";
}

void
Region::PrintTrailer(std::ostream & itkNotUsed(os), Indent itkNotUsed(indent)) const
{}

void
Region::PrintSelf(std::ostream &, Indent) const
{}
} // end namespace itk
