/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaDataObject.txx
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
#ifndef __itkMetaDataObject_txx
#define __itkMetaDataObject_txx

#include "itkMetaDataObject.h"

namespace itk
{

template<class MetaDataObjectType>
MetaDataObject<MetaDataObjectType>
::MetaDataObject(void)
{
  //Nothing to do, m_MetaDataObjectValue takes this types default value.
}

template<class MetaDataObjectType>
MetaDataObject<MetaDataObjectType>
::~MetaDataObject(void)
{
  //Nothing to do here.
}


template<class MetaDataObjectType>
MetaDataObject<MetaDataObjectType>
::MetaDataObject(const MetaDataObjectType InitializerValue)
  :m_MetaDataObjectValue(InitializerValue)
{
  //Nothing to be done here
}

template<class MetaDataObjectType>
MetaDataObject<MetaDataObjectType>
::MetaDataObject(const MetaDataObject<MetaDataObjectType> &TemplateObject)
  :m_MetaDataObjectValue(TemplateObject.m_MetaDataObjectValue)
{
  //Nothing to be done here
}

template<class MetaDataObjectType>
const char *
MetaDataObject<MetaDataObjectType>
::GetMetaDataObjectTypeName(void) const
{
  return typeid(MetaDataObjectType).name();
}

template<class MetaDataObjectType>
const std::type_info &
MetaDataObject<MetaDataObjectType>
::GetMetaDataObjectTypeInfo(void) const
{
  return typeid(MetaDataObjectType);
}

template<class MetaDataObjectType>
const MetaDataObjectType &
MetaDataObject<MetaDataObjectType>
::GetMetaDataObjectValue(void) const
{
  return m_MetaDataObjectValue;
}

template<class MetaDataObjectType>
void
MetaDataObject<MetaDataObjectType>
::SetMetaDataObjectValue(const MetaDataObjectType & NewValue )
{
  m_MetaDataObjectValue=NewValue;
}

template<class MetaDataObjectType>
void
MetaDataObject<MetaDataObjectType>
::Print(std::ostream& os) const
{
  Superclass::Print(os);
}

// Define a specialization of the Print function
// for some basic types. We don't use the initial
// NATIVE_TYPE_METADATAPRINT macro because it lacks
// the inline keyword and it tries to specialize
// for const types which does not compile on MSVC
#define NATIVE_TYPE_METADATAPRINT_NOCONST(TYPE_NAME)   \
template <> \
inline void MetaDataObject< TYPE_NAME > \
::Print(std::ostream& os) const \
{ \
  os << this->m_MetaDataObjectValue << std::endl; \
}

NATIVE_TYPE_METADATAPRINT_NOCONST( unsigned char )
NATIVE_TYPE_METADATAPRINT_NOCONST( short )
NATIVE_TYPE_METADATAPRINT_NOCONST( unsigned short )
NATIVE_TYPE_METADATAPRINT_NOCONST( int )
NATIVE_TYPE_METADATAPRINT_NOCONST( unsigned int )
NATIVE_TYPE_METADATAPRINT_NOCONST( long )
NATIVE_TYPE_METADATAPRINT_NOCONST( unsigned long )
NATIVE_TYPE_METADATAPRINT_NOCONST( float )
NATIVE_TYPE_METADATAPRINT_NOCONST( double )
NATIVE_TYPE_METADATAPRINT_NOCONST( std::string )

// undef the macro to clean up things
#undef NATIVE_TYPE_METADATAPRINT_NOCONST

} // end namespace itk

#endif
