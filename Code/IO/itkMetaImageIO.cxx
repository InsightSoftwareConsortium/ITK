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

#include <string>
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


void MetaImageIO::SetDataFileName( const char* filename ) 
  { 
  m_MetaImage.ElementDataFileName( filename );
  }

 
// This method will only test if the header looks like a
// MetaImage.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool MetaImageIO::CanReadFile( const char* filename ) 
  { 

  // First check the extension
  std::string fname = filename;
  if(  fname == ""  || 
       !( fname.find(".mha") < fname.length() || 
          fname.find(".mhd") < fname.length()    ) )
    {
    itkDebugMacro(<<"The filename extension is not recognized");
    return false;
    }

  // Now check the file content
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
  if( strcmp(key,"ObjectType")==0 ) 
    {
    inputStream.close();
    return true;
    }
  if( strcmp(key,"TransformType")==0 ) 
    {
    inputStream.close();
    return true;
    }
  if( strcmp(key,"ID")==0 ) 
    {
    inputStream.close();
    return true;
    }
  if( strcmp(key,"ParentID")==0 ) 
    {
    inputStream.close();
    return true;
    }
  if( strcmp(key,"BinaryData")==0 ) 
    {
    inputStream.close();
    return true;
    }
  if( strcmp(key,"Comment")==0 ) 
    {
    inputStream.close();
    return true;
    }
  if( strcmp(key,"AcquisitionDate")==0 ) 
    {
    inputStream.close();
    return true;
    }
  if( strcmp(key,"Modality")==0 ) 
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
  for(i=0; i<(int)m_NumberOfDimensions; i++)
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


bool MetaImageIO::CanWriteFile( const char * name )
  {
  std::string filename = name;
  if(  filename == "" )
    {
    return false;
    }
  if( filename.find(".mha") < filename.length() || 
      filename.find(".mhd") < filename.length()    )
    {
    return true;
    }
  return false;
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
  int nDims = this->GetNumberOfDimensions();

  bool binaryData = false;
  if(this->GetFileType() == Binary)
    {
    binaryData = true;
    }

  int nChannels = this->GetNumberOfComponents();

  MET_ValueEnumType eType;
  switch(m_PixelType)
    {
    default:
    case UNKNOWN:
          eType = MET_OTHER;
          break;
    case CHAR:
          eType = MET_CHAR;
          break;
    case UCHAR:
          eType = MET_UCHAR;
          break;
    case SHORT:
          eType = MET_SHORT;
          break;
    case USHORT:
          eType = MET_USHORT;
          break;
    case INT:
          eType = MET_INT;
          break;
    case UINT:
          eType = MET_UINT;
          break;
    case FLOAT:
          eType = MET_FLOAT;
          break;
    case DOUBLE:
          eType = MET_DOUBLE;
          break;
    }
  
  int i;
  int * dSize = new int[nDims];
  float * eSpacing = new float[nDims];
  float * eOrigin = new float[nDims];
  for(i=0; i<nDims; i++)
    {
    dSize[i] = this->GetDimensions(i);
    eSpacing[i] = this->GetSpacing(i);
    eOrigin[i] = this->GetOrigin(i);
    } 

  m_MetaImage.InitializeEssential(nDims, dSize, eSpacing, eType, nChannels,
                                  (void *)buffer);
  m_MetaImage.Position(eOrigin);
  m_MetaImage.BinaryData(binaryData);

  if(strlen(m_MetaImage.ElementDataFileName())==0)
    {
    std::string dataName;
    dataName = m_FileName + ".raw";
    m_MetaImage.Write(m_FileName.c_str(), dataName.c_str());
    }
  else
    {
    m_MetaImage.Write(m_FileName.c_str());
    }

  delete dSize;
  delete eSpacing;
  delete eOrigin;
  } 





} // end namespace itk
