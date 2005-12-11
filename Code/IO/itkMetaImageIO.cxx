/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include <string>
#include "itkMetaImageIO.h"
#include "itkExceptionObject.h"
#include "itkSpatialOrientation.h"
#include "itkMetaDataObject.h"
#include "itkIOCommon.h"

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
  if(  fname == "" )
    {
    itkDebugMacro(<<"No filename specified.");
    return false;
    }

  bool extensionFound = false;
  std::string::size_type mhaPos = fname.rfind(".mha");
  if ((mhaPos != std::string::npos)
      && (mhaPos == fname.length() - 4))
    {
    extensionFound = true;
    }

  std::string::size_type mhdPos = fname.rfind(".mhd");
  if ((mhdPos != std::string::npos)
      && (mhdPos == fname.length() - 4))
    {
    extensionFound = true;
    }
  
  if( !extensionFound )
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
  if(!m_MetaImage.Read(m_FileName.c_str(), false))
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
      this->SetPixelType( UNKNOWNPIXELTYPE );
      this->SetComponentType( UNKNOWNCOMPONENTTYPE );
      break;
    case MET_CHAR:
    case MET_ASCII_CHAR:
      this->SetPixelType( SCALAR );
      this->SetComponentType( CHAR );
      break;
    case MET_CHAR_ARRAY:
    case MET_STRING:
      this->SetPixelType( VECTOR );
      this->SetComponentType( CHAR );
      break;
    case MET_UCHAR:
      this->SetPixelType( SCALAR );
      this->SetComponentType( UCHAR );
      break;
    case MET_UCHAR_ARRAY:
      this->SetPixelType( VECTOR );
      this->SetComponentType( UCHAR );
      break;
    case MET_SHORT:
      this->SetPixelType( SCALAR );
      this->SetComponentType( SHORT );
      break;
    case MET_SHORT_ARRAY:
      this->SetPixelType( VECTOR );
      this->SetComponentType( SHORT );
      break;
    case MET_USHORT:
      this->SetPixelType( SCALAR );
      this->SetComponentType( USHORT );
      break;
    case MET_USHORT_ARRAY:
      this->SetPixelType( VECTOR );
      this->SetComponentType( USHORT );
      break;
    case MET_INT:
      this->SetPixelType( SCALAR );
      if(sizeof(int) == MET_ValueTypeSize[MET_INT])
        {
        this->SetComponentType( INT );
        }
      else if(sizeof(long) == MET_ValueTypeSize[MET_INT])
        {
        this->SetComponentType( LONG );
        }
      break;
    case MET_INT_ARRAY:
      this->SetPixelType( VECTOR );
      if(sizeof(int) == MET_ValueTypeSize[MET_INT])
        {
        this->SetComponentType( INT );
        }
      else if(sizeof(long) == MET_ValueTypeSize[MET_INT])
        {
        this->SetComponentType( LONG );
        }
      break;
    case MET_UINT:
      this->SetPixelType( SCALAR );
      if(sizeof(unsigned int) == MET_ValueTypeSize[MET_UINT])
        {
        this->SetComponentType( UINT );
        }
      else if(sizeof(unsigned long) == MET_ValueTypeSize[MET_UINT])
        {
        this->SetComponentType( ULONG );
        }
      break;
    case MET_UINT_ARRAY: 
      this->SetPixelType( VECTOR );
      if(sizeof(int) == MET_ValueTypeSize[MET_INT])
        {
        this->SetComponentType( UINT );
        }
      else if(sizeof(long) == MET_ValueTypeSize[MET_INT])
        {
        this->SetComponentType( ULONG );
        }
      break;
    case MET_LONG:
      this->SetPixelType( SCALAR );
      if(sizeof(long) == MET_ValueTypeSize[MET_LONG])
        {
        this->SetComponentType( LONG );
        }
      else if(sizeof(int) == MET_ValueTypeSize[MET_LONG])
        {
        this->SetComponentType( INT );
        }
      break;
    case MET_LONG_ARRAY:
      this->SetPixelType( VECTOR );
      if(sizeof(long) == MET_ValueTypeSize[MET_LONG])
        {
        this->SetComponentType( LONG );
        }
      else if(sizeof(int) == MET_ValueTypeSize[MET_LONG])
        {
        this->SetComponentType( INT );
        }
      break;
    case MET_ULONG:
      this->SetPixelType( SCALAR );
      if(sizeof(unsigned long) == MET_ValueTypeSize[MET_ULONG])
        {
        this->SetComponentType( ULONG );
        }
      else if(sizeof(unsigned int) == MET_ValueTypeSize[MET_ULONG])
        {
        this->SetComponentType( UINT );
        }
      break;
    case MET_ULONG_ARRAY:
      this->SetPixelType( VECTOR );
      if(sizeof(unsigned long) == MET_ValueTypeSize[MET_ULONG])
        {
        this->SetComponentType( ULONG );
        }
      else if(sizeof(unsigned int) == MET_ValueTypeSize[MET_ULONG])
        {
        this->SetComponentType( UINT );
        }
      break;
    case MET_LONG_LONG:
      this->SetPixelType( SCALAR );
      if(sizeof(long) == MET_ValueTypeSize[MET_LONG_LONG])
        {
        this->SetComponentType( LONG );
        }
      else if(sizeof(int) == MET_ValueTypeSize[MET_LONG_LONG])
        {
        this->SetComponentType( INT );
        }
      else 
        {
        this->SetComponentType( UNKNOWNCOMPONENTTYPE );
        }
      break;
    case MET_LONG_LONG_ARRAY:
      this->SetPixelType( VECTOR );
      if(sizeof(long) == MET_ValueTypeSize[MET_LONG_LONG])
        {
        this->SetComponentType( LONG );
        }
      else if(sizeof(int) == MET_ValueTypeSize[MET_LONG_LONG])
        {
        this->SetComponentType( INT );
        }
      else 
        {
        this->SetComponentType( UNKNOWNCOMPONENTTYPE );
        }
      break;
    case MET_ULONG_LONG:
      this->SetPixelType( SCALAR );
      if(sizeof(unsigned long) == MET_ValueTypeSize[MET_ULONG_LONG])
        {
        this->SetComponentType( ULONG );
        }
      else if(sizeof(unsigned int) == MET_ValueTypeSize[MET_ULONG_LONG])
        {
        this->SetComponentType( UINT );
        }
      else 
        {
        this->SetComponentType( UNKNOWNCOMPONENTTYPE );
        }
      break;
    case MET_ULONG_LONG_ARRAY:
      this->SetPixelType( VECTOR );
      if(sizeof(unsigned long) == MET_ValueTypeSize[MET_ULONG_LONG])
        {
        this->SetComponentType( ULONG );
        }
      else if(sizeof(unsigned int) == MET_ValueTypeSize[MET_ULONG_LONG])
        {
        this->SetComponentType( UINT );
        }
      else 
        {
        this->SetComponentType( UNKNOWNCOMPONENTTYPE );
        }
      break;
    case MET_FLOAT:
      this->SetPixelType( SCALAR );
      if(sizeof(float) == MET_ValueTypeSize[MET_FLOAT])
        {
        this->SetComponentType( FLOAT );
        }
      else if(sizeof(double) == MET_ValueTypeSize[MET_FLOAT])
        {
        this->SetComponentType( DOUBLE );
        }
      break;
    case MET_FLOAT_ARRAY: 
      this->SetPixelType( VECTOR );
      if(sizeof(float) == MET_ValueTypeSize[MET_FLOAT])
        {
        this->SetComponentType( FLOAT );
        }
      else if(sizeof(double) == MET_ValueTypeSize[MET_FLOAT])
        {
        this->SetComponentType( DOUBLE );
        }
      break;
    case MET_DOUBLE:
      this->SetPixelType( SCALAR );
      this->SetComponentType( DOUBLE );
      if(sizeof(double) == MET_ValueTypeSize[MET_DOUBLE])
        {
        this->SetComponentType( DOUBLE );
        }
      else if(sizeof(float) == MET_ValueTypeSize[MET_DOUBLE])
        {
        this->SetComponentType( FLOAT );
        }
      break;
    case MET_DOUBLE_ARRAY:
      this->SetPixelType( VECTOR );
      if(sizeof(double) == MET_ValueTypeSize[MET_DOUBLE])
        {
        this->SetComponentType( DOUBLE );
        }
      else if(sizeof(float) == MET_ValueTypeSize[MET_DOUBLE])
        {
        this->SetComponentType( FLOAT );
        }
      break;
    case MET_FLOAT_MATRIX:
      this->SetPixelType( VECTOR );
      if(sizeof(float) == MET_ValueTypeSize[MET_FLOAT])
        {
        this->SetComponentType( FLOAT );
        }
      else if(sizeof(double) == MET_ValueTypeSize[MET_FLOAT])
        {
        this->SetComponentType( DOUBLE );
        }
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

  itk::MetaDataDictionary & thisMetaDict = this->GetMetaDataDictionary();

  /* TO - DO */
  /*
  Record type read - as in:
  itk::EncapsulateMetaData<std::string>(thisMetaDict, ITK_OnDiskStorageTypeName,
                                        std::string(typeid(float).name()));
  */
  if(m_NumberOfDimensions == 3)
    {
    itk::SpatialOrientation::ValidCoordinateOrientationFlags coordOrient;
    int zero = 0;
    bool coordUndefined = false;
    switch(m_MetaImage.AnatomicalOrientation(zero))
      {
      case MET_ORIENTATION_UNKNOWN:
      default:
        {
        coordUndefined = true;
        break;
        }
      case MET_ORIENTATION_RL:  
        {
        switch(m_MetaImage.AnatomicalOrientation(1))
          {
          case MET_ORIENTATION_UNKNOWN:
          default:
            {
            coordUndefined = true;
            break;
            }
          case MET_ORIENTATION_PA:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_IS)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS;
              }
            break;
            }
          case MET_ORIENTATION_AP:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_IS)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS;
              }
            break;
            }
          case MET_ORIENTATION_IS:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_PA)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA;
              }
            break;
            }
          case MET_ORIENTATION_SI:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_PA)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA;
              }
            break;
            }
          }
        break;
        }
      case MET_ORIENTATION_LR:  
        {
        switch(m_MetaImage.AnatomicalOrientation(1))
          {
          case MET_ORIENTATION_UNKNOWN:
          default:
            {
            coordUndefined = true;
            break;
            }
          case MET_ORIENTATION_PA:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_IS)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS;
              }
            break;
            }
          case MET_ORIENTATION_AP:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_IS)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS;
              }
            break;
            }
          case MET_ORIENTATION_IS:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_PA)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA;
              }
            break;
            }
          case MET_ORIENTATION_SI:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_PA)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA;
              }
            break;
            }
          }
        break;
        }
      case MET_ORIENTATION_PA:
        {
        switch(m_MetaImage.AnatomicalOrientation(1))
          {
          case MET_ORIENTATION_UNKNOWN:
          default:
            {
            coordUndefined = true;
            break;
            }
          case MET_ORIENTATION_RL:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_IS)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS;
              }
            break;
            }
          case MET_ORIENTATION_LR:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_IS)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS;
              }
            break;
            }
          case MET_ORIENTATION_IS:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_RL)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL;
              }
            break;
            }
          case MET_ORIENTATION_SI:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_RL)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL;
              }
            break;
            }
          }
        break;
        }
      case MET_ORIENTATION_AP:
        {
        switch(m_MetaImage.AnatomicalOrientation(1))
          {
          case MET_ORIENTATION_UNKNOWN:
          default:
            {
            coordUndefined = true;
            break;
            }
          case MET_ORIENTATION_RL:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_IS)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS;
              }
            break;
            }
          case MET_ORIENTATION_LR:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_IS)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS;
              }
            break;
            }
          case MET_ORIENTATION_IS:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_RL)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL;
              }
            break;
            }
          case MET_ORIENTATION_SI:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_RL)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL;
              }
            break;
            }
          }
        break;
        }
      case MET_ORIENTATION_IS:
        {
        switch(m_MetaImage.AnatomicalOrientation(1))
          {
          case MET_ORIENTATION_UNKNOWN:
          default:
            {
            coordUndefined = true;
            break;
            }
          case MET_ORIENTATION_RL:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_PA)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA;
              }
            break;
            }
          case MET_ORIENTATION_LR:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_PA)
              {
              coordOrient =
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP;
              }
            else
              {
              coordOrient =
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA;
              }
            break;
            }
           case MET_ORIENTATION_PA:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_RL)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL;
              }
            break;
            }
          case MET_ORIENTATION_AP:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_RL)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL;
              }
            break;
            }
          }
        break;
        }
      case MET_ORIENTATION_SI:
        {
        switch(m_MetaImage.AnatomicalOrientation(1))
          {
          case MET_ORIENTATION_UNKNOWN:
          default:
            {
            coordUndefined = true;
            break;
            }
          case MET_ORIENTATION_RL:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_PA)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA;
              }
            break;
            }
          case MET_ORIENTATION_LR:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_PA)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA;
              }
            break;
            }
          case MET_ORIENTATION_PA:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_RL)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL;
              }
            break;
            }
          case MET_ORIENTATION_AP:
            {
            if(m_MetaImage.AnatomicalOrientation(2) == MET_ORIENTATION_RL)
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR;
              }
            else
              {
              coordOrient = 
                  itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL;
              }
            break;
            }
          }
        break;
        }
      }
    if(!coordUndefined)
      {
      itk::EncapsulateMetaData
          <itk::SpatialOrientation::ValidCoordinateOrientationFlags>
          (thisMetaDict, ITK_CoordinateOrientation, coordOrient);
      }
    
    // Read direction cosines if the dimension of the image is 3.
    //
    const double *transformMatrix = m_MetaImage.TransformMatrix();
    vnl_vector< double > directionAxis( this->GetNumberOfDimensions() );
    for( unsigned int i=0; i < this->GetNumberOfDimensions(); i++)
      {
      for( unsigned int j=0; j < this->GetNumberOfDimensions(); j++)
        {
        directionAxis[j] = transformMatrix[i*this->GetNumberOfDimensions() + j];
        }
      this->SetDirection( i, directionAxis );
      }
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

  std::string::size_type mhaPos = filename.rfind(".mha");
  if ((mhaPos != std::string::npos)
      && (mhaPos == filename.length() - 4))
    {
    return true;
    }

  std::string::size_type mhdPos = filename.rfind(".mhd");
  if ((mhdPos != std::string::npos)
      && (mhdPos == filename.length() - 4))
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
  switch(m_ComponentType)
    {
    default:
    case UNKNOWNCOMPONENTTYPE:
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
    case LONG:
      if(sizeof(long) == MET_ValueTypeSize[MET_LONG])
        eType = MET_LONG;
      else if(sizeof(long) == MET_ValueTypeSize[MET_INT])
        eType = MET_INT;
      else if(sizeof(long) == MET_ValueTypeSize[MET_LONG_LONG])
        eType = MET_LONG_LONG;
      break;
    case ULONG:
      if(sizeof(long) == MET_ValueTypeSize[MET_LONG])
        eType = MET_ULONG;
      else if(sizeof(long) == MET_ValueTypeSize[MET_INT])
        eType = MET_UINT;
      else if(sizeof(long) == MET_ValueTypeSize[MET_LONG_LONG])
        eType = MET_ULONG_LONG;
      break;
    case INT:
      eType = MET_INT;
      if(sizeof(int) == MET_ValueTypeSize[MET_INT])
        eType = MET_INT;
      else if(sizeof(int) == MET_ValueTypeSize[MET_LONG])
        eType = MET_LONG;
      break;
    case UINT:
      if(sizeof(int) == MET_ValueTypeSize[MET_INT])
        eType = MET_UINT;
      else if(sizeof(int) == MET_ValueTypeSize[MET_LONG])
        eType = MET_ULONG;
      break;
    case FLOAT:
      if(sizeof(float) == MET_ValueTypeSize[MET_FLOAT])
        eType = MET_FLOAT;
      else if(sizeof(float) == MET_ValueTypeSize[MET_DOUBLE])
        eType = MET_DOUBLE;
      break;
    case DOUBLE:
      if(sizeof(double) == MET_ValueTypeSize[MET_DOUBLE])
        eType = MET_DOUBLE;
      else if(sizeof(double) == MET_ValueTypeSize[MET_FLOAT])
        eType = MET_FLOAT;
      break;
    }
  
  int i;
  int * dSize = new int[nDims];
  float * eSpacing = new float[nDims];
  double * eOrigin = new double[nDims];
  for(i=0; i<nDims; i++)
    {
    dSize[i] = this->GetDimensions(i);
    eSpacing[i] = this->GetSpacing(i);
    eOrigin[i] = this->GetOrigin(i);
    } 
  ImageIORegion::IndexType indx = this->GetIORegion().GetIndex();
  for(i=0; i<nDims; i++)
    {
    int j;
    for(j=0; j<nDims; j++)
      {
      eOrigin[i] += indx[j] * eSpacing[i] * this->GetDirection(j)[i];
      }
    }

  m_MetaImage.InitializeEssential(nDims, dSize, eSpacing, eType, nChannels,
                                  const_cast<void *>(buffer));
  m_MetaImage.Position(eOrigin);
  m_MetaImage.BinaryData(binaryData);

  if(nDims == 3)
    {
    itk::MetaDataDictionary & thisMetaDict = this->GetMetaDataDictionary();
  
    itk::SpatialOrientation::ValidCoordinateOrientationFlags coordOrient;
    if( itk::ExposeMetaData
          <itk::SpatialOrientation::ValidCoordinateOrientationFlags>
          (thisMetaDict, ITK_CoordinateOrientation, coordOrient) )
      {
      switch(coordOrient)
        {
        default:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP:
          {
          m_MetaImage.AnatomicalOrientation(0, MET_ORIENTATION_RL);
          break;
          }
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP:
          {
          m_MetaImage.AnatomicalOrientation(0, MET_ORIENTATION_LR);
          break;
          }
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR:
          {
          m_MetaImage.AnatomicalOrientation(0, MET_ORIENTATION_AP);
          break;
          }
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR:
          {
          m_MetaImage.AnatomicalOrientation(0, MET_ORIENTATION_PA);
          break;
          }
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP:
          {
          m_MetaImage.AnatomicalOrientation(0, MET_ORIENTATION_IS);
          break;
          }
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP:
          {
          m_MetaImage.AnatomicalOrientation(0, MET_ORIENTATION_SI);
          break;
          }
        }
      switch(coordOrient)
        {
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP:
          {
          m_MetaImage.AnatomicalOrientation(1, MET_ORIENTATION_RL);
          break;
          }
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP:
          {
          m_MetaImage.AnatomicalOrientation(1, MET_ORIENTATION_LR);
          break;
          }
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR:
          {
          m_MetaImage.AnatomicalOrientation(1, MET_ORIENTATION_AP);
          break;
          }
        default:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR:
          {
          m_MetaImage.AnatomicalOrientation(1, MET_ORIENTATION_PA);
          break;
          }
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
          {
          m_MetaImage.AnatomicalOrientation(1, MET_ORIENTATION_IS);
          break;
          }
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP:
          {
          m_MetaImage.AnatomicalOrientation(1, MET_ORIENTATION_SI);
          break;
          }
        }
      switch(coordOrient)
        {
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR:
          {
          m_MetaImage.AnatomicalOrientation(2, MET_ORIENTATION_RL);
          break;
          }
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL:
          {
          m_MetaImage.AnatomicalOrientation(2, MET_ORIENTATION_LR);
          break;
          }
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA:
          {
          m_MetaImage.AnatomicalOrientation(2, MET_ORIENTATION_AP);
          break;
          }
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP:
          {
          m_MetaImage.AnatomicalOrientation(2, MET_ORIENTATION_PA);
          break;
          }
        default:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
          {
          m_MetaImage.AnatomicalOrientation(2, MET_ORIENTATION_IS);
          break;
          }
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS:
        case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS:
          {
          m_MetaImage.AnatomicalOrientation(2, MET_ORIENTATION_SI);
          break;
          }
        }
      }

    // Propagage direction cosine information .
    double *transformMatrix = 
    static_cast< double *>(malloc(this->GetNumberOfDimensions() * 
                    this->GetNumberOfDimensions() * sizeof(double)));
    for( unsigned int i=0; i < this->GetNumberOfDimensions(); i++)
      {
      for( unsigned int j=0; j < this->GetNumberOfDimensions(); j++)
        {
        transformMatrix[i*this->GetNumberOfDimensions() +j ] =
                                           this->GetDirection(i)[j];
        }
      }
    m_MetaImage.TransformMatrix( transformMatrix );      
    free(transformMatrix);
    }

  
  m_MetaImage.CompressedData(m_UseCompression);
  m_MetaImage.Write(m_FileName.c_str());

  delete []dSize;
  delete []eSpacing;
  delete []eOrigin;
}





} // end namespace itk
