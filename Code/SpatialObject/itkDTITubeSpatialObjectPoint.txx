/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDTITubeSpatialObjectPoint.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDTITubeSpatialObjectPoint_txx
#define __itkDTITubeSpatialObjectPoint_txx

#include "itkDTITubeSpatialObjectPoint.h"
#include <itksys/SystemTools.hxx>

namespace itk 
{

/** Constructor */
template< unsigned int TPointDimension >
DTITubeSpatialObjectPoint< TPointDimension >
::DTITubeSpatialObjectPoint( void ) 
{ 
  // Initialize the tensor matrix to identity
  for(unsigned int i=0;i<6;i++)
    {
    m_TensorMatrix[i] = 0;
    }
  m_TensorMatrix[0] = 1;
  m_TensorMatrix[3] = 1;
  m_TensorMatrix[5] = 1;
}

/** Destructor */
template< unsigned int TPointDimension >
DTITubeSpatialObjectPoint< TPointDimension >
::~DTITubeSpatialObjectPoint( void ) 
{
}


template< unsigned int TPointDimension >
void
DTITubeSpatialObjectPoint< TPointDimension >
::PrintSelf( std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

/** Translate the enumerated types to a string */
template< unsigned int TPointDimension >
std::string 
DTITubeSpatialObjectPoint< TPointDimension >
::TranslateEnumToChar(FieldEnumType name) const
{
  // Do the translation
  switch(name)
    {
    case 0:
      return "FA";
      break;
    case 1:
      return "ADC";
      break;
    case 2:
      return "GA";
      break;
    default:
      return "";
    }
  return "";
}

/** Add a field to the point list*/
template< unsigned int TPointDimension >
void
DTITubeSpatialObjectPoint< TPointDimension >
::AddField(const char* name,float value)
{
  FieldType field(itksys::SystemTools::LowerCase(name),value);
  m_Fields.push_back(field);
}


/** Add a field to the point list*/
template< unsigned int TPointDimension >
void
DTITubeSpatialObjectPoint< TPointDimension >
::AddField(FieldEnumType name,float value)
{
  std::string charname = this->TranslateEnumToChar(name);

  if(charname.size() > 0)
    {
    FieldType field(itksys::SystemTools::LowerCase(charname).c_str(),value);
    m_Fields.push_back(field);
    }
  else
    {
    std::cout << "DTITubeSpatialObjectPoint::AddField() : enum not defined" << std::endl;
    }

}

/** Return the value of the given field */
template< unsigned int TPointDimension >
float
DTITubeSpatialObjectPoint< TPointDimension >
::GetField(const char* name) const
{
  FieldListType::const_iterator it = m_Fields.begin();
  while(it != m_Fields.end())
    {
    if(!strcmp((*it).first.c_str(),itksys::SystemTools::LowerCase(name).c_str()))
      {
      return (*it).second;
      }
    it++;
    }
  return -1;
}

/** Add a field to the point list*/
template< unsigned int TPointDimension >
float
DTITubeSpatialObjectPoint< TPointDimension >
::GetField(FieldEnumType name) const
{
  std::string charname = this->TranslateEnumToChar(name);
  if(charname.size() > 0)
    {
    return this->GetField(itksys::SystemTools::LowerCase(charname).c_str());
    }
  else
    {
    std::cout << "DTITubeSpatialObjectPoint::GetField() : enum not defined" << std::endl;
    return -1;
    }

  return -1;
}

template< unsigned int TPointDimension >
typename DTITubeSpatialObjectPoint< TPointDimension >::Self & 
DTITubeSpatialObjectPoint< TPointDimension >
::operator=(const DTITubeSpatialObjectPoint & rhs) 
{
  // Copy the extra fields
  const FieldListType & fields = rhs.GetFields();
  FieldListType::const_iterator it = fields.begin();
  while(it != fields.end())
    {
    this->AddField((*it).first.c_str(),(*it).second);
    }

  this->m_ID = rhs.m_ID;

  for(unsigned int i=0;i<6;i++)
    {
    m_TensorMatrix[i] = rhs.m_TensorMatrix[i];
    }

  m_NumDimensions = rhs.m_NumDimensions;
  this->m_X = rhs.m_X;
  this->m_T = rhs.m_T;
  this->m_Normal1 = rhs.m_Normal1;
  this->m_Normal2 = rhs.m_Normal2;
  this->m_Color = rhs.m_Color;
  return * this;
}

} // end namespace itk

#endif
