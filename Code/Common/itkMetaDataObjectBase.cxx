/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaDataObjectBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMetaDataObjectBase.h"

void
itk::MetaDataObjectBase
::Print(std::ostream& os) const
{
  //Superclass::Print(os);
  os <<  "Unknown Print Characteristics" << std::endl;
}


const char *
itk::MetaDataObjectBase
::GetMetaDataObjectTypeName(void) const
{
  return typeid(itk::MetaDataObjectBase).name();
}

const std::type_info &
itk::MetaDataObjectBase
::GetMetaDataObjectTypeInfo(void) const
{
  return typeid(itk::MetaDataObjectBase);
}

itk::MetaDataObjectBase
::MetaDataObjectBase()
{
  //Nothing to do here
}

itk::MetaDataObjectBase
::~MetaDataObjectBase()
{
  //std::cout << "              MetaDataObjectBase Deleteing: " << this << std::endl;
  //Nothing to do here
}

//itk::MetaDataObjectBase
//::New(void)
//{
//  return new Self;
//}

