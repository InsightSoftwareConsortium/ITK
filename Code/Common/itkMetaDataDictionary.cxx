/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaDataDictionary.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMetaDataDictionary.h"

void
itk::MetaDataDictionary
::Print(std::ostream& os) const
{
  for(itk::MetaDataDictionary::const_iterator it=this->begin();
      it != this->end();
      it++)
    {
    os << it->first <<  "  " ;
    it->second->Print(os);
    }
}

itk::MetaDataDictionary
::~MetaDataDictionary()
{
}
