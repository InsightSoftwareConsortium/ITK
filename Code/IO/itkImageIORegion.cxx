/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIORegion.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImageIORegion.h"

namespace itk
{

std::ostream & operator<<(std::ostream &os, const ImageIORegion &region)
{
  region.Print(os);
  return os;
}

void
ImageIORegion
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  
  os << indent << "Dimension: " << this->GetImageDimension() << std::endl;
  os << indent << "Index: ";
  for(ImageIORegion::IndexType::const_iterator i = this->GetIndex().begin();
      i != this->GetIndex().end(); ++i)
    {
    os << *i << " ";
    }
  os << std::endl;
  os << indent << "Size: ";
  for(ImageIORegion::SizeType::const_iterator k = this->GetSize().begin();
      k != this->GetSize().end(); ++k)
    {
    os << *k << " ";
    }
  os << std::endl;
}

} //namespace itk
