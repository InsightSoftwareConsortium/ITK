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
::PrintSelf(std::ostream& os, Indent indent) const
{
  for(itk::MetaDataDictionary::const_iterator it=this->begin();
      it != this->end();
      it++)
  {
    it->second->PrintSelf(os,indent);
  }
}

itk::MetaDataDictionary
::~MetaDataDictionary()
{
  //Nothing special to do here.
}
