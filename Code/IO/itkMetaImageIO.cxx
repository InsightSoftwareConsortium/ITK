/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkMetaImageIO.h"
#include "itkExceptionObject.h"

namespace itk
{

MetaImageIO::MetaImageIO()
  {
  if(MET_SystemByteOrderMSB())
    m_ByteOrder = BigEndian;
  else
    m_ByteOrder = LittleEndian;
  } 



MetaImageIO::~MetaImageIO()
  {
  m_Ifstream.close();
  }



void MetaImageIO::PrintSelf(std::ostream& os, Indent indent) const
  {
  Superclass::PrintSelf(os, indent);
  m_MetaImage.PrintInfo();
  }


// This method will only test if the header looks like a
// MetaImage.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool MetaImageIO::CanReadFile( const char* filename ) 
  { 
  
  std::ifstream inputStream;

  inputStream.open( filename, std::ios::in | std::ios::binary );

  if( inputStream.fail() )
  {
    return false;
  }

  char key[8000];

  inputStream >> key;

  if( inputStream.eof() )
    {
    inputStream.close();
    return false;
    }

  if( strcmp(key,"NDims")==0 ) 
    {
    inputStream.close();
    return true;
    }

  inputStream.close();
  return false;

  }
  

void MetaImageIO::ReadImageInformation()
  { 
  if(!m_MetaImage.Read(m_FileName.c_str()), false)
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("File cannot be read");
    throw exception;
    }

  if(m_MetaImage.BinaryData())
    {
    this->SetFileType(Binary);
    }
  else
    {
    this->SetFileType(ASCII);
    }

  this->SetNumberOfComponents(m_MetaImage.ElementNumberOfChannels());

  switch(m_MetaImage.ElementType())
    {
    default:
    case MET_OTHER:
    case MET_NONE:
      this->SetPixelType( UNKNOWN );
      this->SetComponentType( UNKNOWN );
      break;
    case MET_CHAR:
    case MET_CHAR_ARRAY:
    case MET_STRING:
    case MET_ASCII_CHAR:
      this->SetPixelType( CHAR );
      this->SetComponentType( CHAR );
      break;
    case MET_UCHAR:
    case MET_UCHAR_ARRAY:
      this->SetPixelType( UCHAR );
      this->SetComponentType( UCHAR );
      break;
    case MET_SHORT:
    case MET_SHORT_ARRAY:
      this->SetPixelType( SHORT );
      this->SetComponentType( SHORT );
      break;
    case MET_USHORT:
    case MET_USHORT_ARRAY:
      this->SetPixelType( USHORT );
      this->SetComponentType( USHORT );
      break;
    case MET_INT:
    case MET_INT_ARRAY:
      this->SetPixelType( INT );
      this->SetComponentType( INT );
      break;
    case MET_UINT:
    case MET_UINT_ARRAY: 
      this->SetPixelType( UINT );
      this->SetComponentType( UINT );
      break;
    case MET_FLOAT:
    case MET_FLOAT_ARRAY: 
      this->SetPixelType( FLOAT );
      this->SetComponentType( FLOAT );
      break;
    case MET_DOUBLE:
    case MET_DOUBLE_ARRAY:
      this->SetPixelType( DOUBLE );
      this->SetComponentType( DOUBLE );
      break;
    case MET_FLOAT_MATRIX:
      this->SetPixelType( FLOAT );
      this->SetComponentType( FLOAT );
      this->SetNumberOfComponents(m_NumberOfComponents * m_NumberOfComponents);
      break;
    }
  
  this->SetNumberOfDimensions(m_MetaImage.NDims());

  int i;
  for(i=0; i<m_NumberOfDimensions; i++)
    {
    this->SetDimensions(i, m_MetaImage.DimSize(i));
    this->SetSpacing(i, m_MetaImage.ElementSpacing(i));
    this->SetOrigin(i, m_MetaImage.Position(i));
    } 
  } 


void MetaImageIO::Read(void* buffer)
  { 
  if(!m_MetaImage.Read(m_FileName.c_str(), true, buffer))
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("File cannot be read");
    throw exception;
    }

  m_MetaImage.ElementByteOrderFix();
  } 

MetaImage * MetaImageIO::GetMetaImagePointer(void)
  {
  return & m_MetaImage;
  }

bool MetaImageIO::CanWriteFile(const char*)
  {
  return true;
  }

  
void 
MetaImageIO
::WriteImageInformation(void)
  {
  } 





/**
 *
 */
void 
MetaImageIO
::Write( const void* buffer) 
  { 
  
  } 





} // end namespace itk
