/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

#include "itkMetaImageIO.h"
#include "itkExceptionObject.h"



namespace itk
{

MetaImageIO::MetaImageIO()
{
  this->SetNumberOfDimensions(2);
  m_MetaPixelType = UCHAR;
}



MetaImageIO::~MetaImageIO()
{
  m_Ifstream.close();
}



void MetaImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "MetaPixelType " << m_MetaPixelType << "\n";
}



const double* 
MetaImageIO::GetSpacing() const
{
  return m_Spacing;
}


  
const double* 
MetaImageIO::GetOrigin() const
{
  return m_Origin;
}

 

 
bool MetaImageIO::CanReadFile(const char* file) 
{ 
  const bool acceptableHeader = this->ReadHeader(file);
  m_Ifstream.close();
  return acceptableHeader;
}
  


const std::type_info& MetaImageIO::GetPixelType() const
{
  switch(m_MetaPixelType)
    {
    case CHAR:
      return typeid(char);
    case UCHAR:
      return typeid(unsigned char);
    case SHORT:
      return typeid(short);
    case USHORT:
      return typeid(unsigned short);
    case INT:
      return typeid(int);
    case UINT:
      return typeid(unsigned int);
    case LONG:
      return typeid(long);
    case ULONG:
      return typeid(unsigned long);
    case FLOAT:
      return typeid(float);
    case DOUBLE:
      return typeid(double);
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Pixel Type Unknown");
      throw exception;
    }
}

  
unsigned int MetaImageIO::GetComponentSize() const
{
  switch(m_MetaPixelType)
    {
    case CHAR:
      return sizeof(char);
    case UCHAR:
      return sizeof(unsigned char);
    case SHORT:
      return sizeof(short);
    case USHORT:
      return sizeof(unsigned short);
    case INT:
      return sizeof(int);
    case UINT:
      return sizeof(unsigned int);
    case LONG:
      return sizeof(long);
    case ULONG:
      return sizeof(unsigned long);
    case FLOAT:
      return sizeof(float);
    case DOUBLE:
      return sizeof(double);
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Pixel Type Unknown");
      throw exception;
    }
  return 1;
}

  
void MetaImageIO::Load(void* buffer)
{
  unsigned int dimensions = this->GetNumberOfDimensions();
  unsigned int numberOfPixels = 1;
  for( unsigned int dim=0; dim< dimensions; dim++ )
    {
    numberOfPixels *= m_Dimensions[ dim ];
    }

  unsigned int pixelSize =  this->GetComponentSize();

  char * p = static_cast<char *>(buffer);
  for( unsigned int pixelnumber = 0; pixelnumber< numberOfPixels; pixelnumber++)
  {
    for(unsigned int bytes=0; bytes<pixelSize; bytes++)
    {
      m_Ifstream.get(*p);
      p++;
    }
  }

}




bool MetaImageIO::ReadHeader(const char* fname)
{
  m_Ifstream.open( fname );

  if( m_Ifstream.fail() )
  {
    return false;
  }

  char key[8000];

  while( !m_Ifstream.eof() )
  {

  m_Ifstream >> key;

  if( m_Ifstream.eof() )
    {
    return false;
    }

  if( strcmp(key,"NDims")==0 ) 
    {
    unsigned int dimension;
    m_Ifstream >> key;
    if( strcmp( key, "=" ) != 0 ) 
      {
      //missing "=" 
      return false;
      }
    m_Ifstream >> dimension;
    this->SetNumberOfDimensions( dimension );
    continue;
    }

  if( strcmp(key,"DimSize")==0 ) 
    {
    m_Ifstream >> key;
    if( strcmp( key, "=" ) != 0 ) 
      {
      //missing "=" 
      return false;
      }
    for( unsigned int dim=0; dim< this->GetNumberOfDimensions(); dim++ )
      {
      m_Ifstream >> m_Dimensions[ dim ];
      }
    continue;
    }

  if( strcmp(key,"ElementSize")==0 ) 
    {
    m_Ifstream >> key;
    if( strcmp( key, "=" ) != 0 ) 
      {
      //missing "=" 
      return false;
      }
    for( unsigned int dim=0; dim< this->GetNumberOfDimensions(); dim++ )
      {
      m_Ifstream >> m_Spacing[ dim ];
      }
    continue;
    }

  if( strcmp(key,"ElementNBits")==0 ) 
    {
    m_Ifstream >> key;
    if( strcmp( key, "=" ) != 0 ) 
      {
      //missing "=" 
      return false;
      }
    unsigned long elmentNumberOfBits;
    m_Ifstream >> elmentNumberOfBits;
    continue;
    }

  
  if( strcmp(key,"ElementType")==0 ) 
    {
    m_Ifstream >> key;
    if( strcmp( key, "=" ) != 0 ) 
      {
      //missing "=" 
      return false;
      }
    char elementType[512];
    m_Ifstream >> elementType;

    if( strcmp( elementType, "MET_UCHAR" ) == 0 )
      {
      m_ComponentType = UCHAR;
      }
    if( strcmp( elementType, "MET_CHAR" ) == 0 )
      {
      m_ComponentType = CHAR;
      }
    if( strcmp( elementType, "MET_USHORT" ) == 0 )
      {
      m_ComponentType = USHORT;
      }
    if( strcmp( elementType, "MET_SHORT" ) == 0 )
      {
      m_ComponentType = SHORT;
      }
    if( strcmp( elementType, "MET_UINT" ) == 0 )
      {
      m_ComponentType = UINT;
      }
    if( strcmp( elementType, "MET_INT" ) == 0 )
      {
      m_ComponentType = INT;
      }
    if( strcmp( elementType, "MET_ULONG" ) == 0 )
      {
      m_ComponentType = ULONG;
      }
    if( strcmp( elementType, "MET_LONG" ) == 0 )
      {
      m_ComponentType = LONG;
      }
    if( strcmp( elementType, "MET_UFLOAT" ) == 0 )
      {
      m_ComponentType = FLOAT;
      }
    if( strcmp( elementType, "MET_DOUBLE" ) == 0 )
      {
      m_ComponentType = DOUBLE;
      }

    continue;
    }

  if( strcmp( key, "ElementDataFile" ) == 0 )
    {
    m_Ifstream >> key;
    if( strcmp( key, "=" ) != 0 ) 
      {
      //missing "=" 
      return false;
      }
    m_Ifstream >> key;
    if( strcmp( key, "LOCAL" ) != 0 ) 
      {
      //missing "=" 
      return false;
      }
    // end of header : succesful read
    this->SetFileName(fname); 
    return true;
    }


    // Unknown code, get the rest of the line
    m_Ifstream.getline(key,2000,'\n');

  }

  
  return false;

}



void MetaImageIO::ReadImageInformation()
{
  this->ReadHeader(m_FileName.c_str());
}


} // end namespace itk
