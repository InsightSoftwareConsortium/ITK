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
#include "itkByteSwapper.h"



namespace itk
{

MetaImageIO::MetaImageIO()
{
  this->SetNumberOfDimensions(2);
  m_MetaPixelType  = UCHAR;
  m_ImageByteOrder = BigEndian;
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


bool 
MetaImageIO::GetSeparatorCharacter(std::ifstream & ifs) const
{
  bool found = false;
  const int limit = 100;  // the separator character is
                          // expected in a range of 100
                          // character from the token.
  int counter = 0;
  while( counter < limit ) 
    {
    char inp = ifs.get();
    if( inp == '=' )
      {
      found = true;
      break;
      }
    counter++;
    }
  return found;
}



// This method will only test if the header looks like a
// MetaImage.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool MetaImageIO::CanReadFile( const char* filename ) 
{ 
  
  unsigned int dimensions = 0;

  std::ifstream inputStream;

  inputStream.open( filename, std::ios::in | std::ios::binary );

  if( inputStream.fail() )
  {
    return false;
  }

  char key[8000];

  const int maxLineLength = 10000;
  char restOfTheLine[maxLineLength];

  while( !inputStream.eof() )
  {

  inputStream >> key;

  if( inputStream.eof() )
    {
    return false;
    }

  if( strcmp(key,"NDims")==0 ) 
    {
    if( !GetSeparatorCharacter( inputStream ) ) 
      {
      return false;
      }
    inputStream >> dimensions;
    inputStream.getline( restOfTheLine, maxLineLength );
    continue;
    }

  if( strcmp(key,"ElementByteOrderMSB")==0 ) 
    {
    if( !GetSeparatorCharacter( inputStream ) ) 
      {
      return false;
      }
    inputStream >> key;
    if( strcmp( key, "False" ) == 0 )
    {
      m_ImageByteOrder = LittleEndian;
    }
    else if ( strcmp( key, "True" ) == 0 )
    {
      m_ImageByteOrder = BigEndian;
    }
    else 
    {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("ImageByteOrder unknown type");
      throw exception;
    }
    inputStream.getline( restOfTheLine, maxLineLength );
    continue;
    }

  if( strcmp(key,"DimSize")==0 ) 
    {
    if( !GetSeparatorCharacter( inputStream ) )
      {
      return false;
      }
    unsigned int sizeInADimension;
    for( unsigned int dim=0; dim < dimensions; dim++ )
      {
      inputStream >> sizeInADimension;
      }
    inputStream.getline( restOfTheLine, maxLineLength );
    continue;
    }

  if( strcmp(key,"ElementSize")==0 ) 
    {
    if( !GetSeparatorCharacter( inputStream ) )
      {
      return false;
      }
    float spacingInADimension;
    for( unsigned int dim=0; dim < dimensions; dim++ )
      {
      inputStream >> spacingInADimension;
      }
    inputStream.getline( restOfTheLine, maxLineLength );
    continue;
    }

  if( strcmp(key,"ElementNBits")==0 ) 
    {
    if( !GetSeparatorCharacter( inputStream ) )
      {
      return false;
      }
    unsigned long elmentNumberOfBits;
    inputStream >> elmentNumberOfBits;
    inputStream.getline( restOfTheLine, maxLineLength );
    continue;
    }

  
  if( strcmp(key,"ElementType")==0 ) 
    {
    if( !GetSeparatorCharacter( inputStream ) )
      {
      return false;
      }
    char elementType[512];
    inputStream >> elementType;
    inputStream.getline( restOfTheLine, maxLineLength );
    continue;
    }

  if( strcmp( key, "ElementDataFile" ) == 0 )
    {
    if( !GetSeparatorCharacter( inputStream ) )
      {
      return false;
      }
    inputStream >> key;
    //if( strcmp( key, "LOCAL" ) != 0 ) 
    //  {
    //  return false;
    //  }
    // end of header : succesful read
    return true;
    }

    // Unknown code, get the rest of the line
    inputStream.getline( restOfTheLine, maxLineLength );

  }

  
  return false;


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
      *p = m_Ifstream.get();
      p++;
    }
  }

  SwapBytesIfNecessary( buffer, numberOfPixels );

}




void MetaImageIO::ReadImageInformation()
{
  m_Ifstream.open( m_FileName.c_str(), std::ios::in | std::ios::binary );
  if( m_Ifstream.fail() )
  {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("File cannot be read");
    throw exception;
  }

  char key[8000];

  const int maxLineLength = 10000;
  char  restOfTheLine[ maxLineLength ];

  while( !m_Ifstream.eof() )
  {

  m_Ifstream >> key;

  if( m_Ifstream.eof() )
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("Unexpected end of file");
    throw exception;
    }

  if( strcmp(key,"NDims")==0 ) 
    {
    unsigned int dimension;
    if( !GetSeparatorCharacter( m_Ifstream ) )
      {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Missing \"=\" after NDims");
      throw exception;
      }
    m_Ifstream >> dimension;
    m_Ifstream.getline( restOfTheLine, maxLineLength );
    this->SetNumberOfDimensions( dimension );
    continue;
    }

  if( strcmp(key,"DimSize")==0 ) 
    {
    if( !GetSeparatorCharacter( m_Ifstream ) )
      {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Missing \"=\" after DimSize");
      throw exception;
      }
    for( unsigned int dim=0; dim< this->GetNumberOfDimensions(); dim++ )
      {
      m_Ifstream >> m_Dimensions[ dim ];
      }
    m_Ifstream.getline( restOfTheLine, maxLineLength );
    continue;
    }

  if( strcmp(key,"ElementSize")==0 ) 
    {
    if( !GetSeparatorCharacter( m_Ifstream ) )
      {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Missing \"=\" after ElementSize");
      throw exception;
      }
    for( unsigned int dim=0; dim< this->GetNumberOfDimensions(); dim++ )
      {
      m_Ifstream >> m_Spacing[ dim ];
      }
    m_Ifstream.getline( restOfTheLine, maxLineLength );
    continue;
    }

  if( strcmp(key,"ElementNBits")==0 ) 
    {
    if( !GetSeparatorCharacter( m_Ifstream ) )
      {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Missing \"=\" after ElementNBits");
      throw exception;
      }
    unsigned long elmentNumberOfBits;
    m_Ifstream >> elmentNumberOfBits;
    m_Ifstream.getline( restOfTheLine, maxLineLength );
    continue;
    }

  
  if( strcmp(key,"ElementType")==0 ) 
    {
    if( !GetSeparatorCharacter( m_Ifstream ) )
      {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Missing \"=\" after ElementType");
      throw exception;
      }
    char elementType[512];
    m_Ifstream >> elementType;
    m_Ifstream.getline( restOfTheLine, maxLineLength );

    if( strcmp( elementType, "MET_UCHAR" ) == 0 )
      {
      m_ComponentType = UCHAR;
      m_MetaPixelType = UCHAR;
      }
    else if( strcmp( elementType, "MET_CHAR" ) == 0 )
      {
      m_ComponentType = CHAR;
      m_MetaPixelType = CHAR;
      }
    else if( strcmp( elementType, "MET_USHORT" ) == 0 )
      {
      m_ComponentType = USHORT;
      m_MetaPixelType = USHORT;
      }
    else if( strcmp( elementType, "MET_SHORT" ) == 0 )
      {
      m_ComponentType = SHORT;
      m_MetaPixelType = SHORT;
      }
    else if( strcmp( elementType, "MET_UINT" ) == 0 )
      {
      m_ComponentType = UINT;
      m_MetaPixelType = UINT;
      }
    else if( strcmp( elementType, "MET_INT" ) == 0 )
      {
      m_ComponentType = INT;
      m_MetaPixelType = INT;
      }
    else if( strcmp( elementType, "MET_ULONG" ) == 0 )
      {
      m_ComponentType = ULONG;
      m_MetaPixelType = ULONG;
      }
    else if( strcmp( elementType, "MET_LONG" ) == 0 )
      {
      m_ComponentType = LONG;
      m_MetaPixelType = LONG;
      }
    else if( strcmp( elementType, "MET_UFLOAT" ) == 0 )
      {
      m_ComponentType = FLOAT;
      m_MetaPixelType = FLOAT;
      }
    else if( strcmp( elementType, "MET_DOUBLE" ) == 0 )
      {
      m_ComponentType = DOUBLE;
      m_MetaPixelType = DOUBLE;
      }

    continue;
    }

  if( strcmp( key, "ElementDataFile" ) == 0 )
    {
    if( !GetSeparatorCharacter( m_Ifstream ) )
      {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Missing \"=\" after ElementDataFile");
      throw exception;
      }
    m_Ifstream >> key;
    m_Ifstream.getline( restOfTheLine, maxLineLength );
    if( strcmp( key, "LOCAL" ) != 0 ) 
      {
      size_t endOfPath = m_FileName.find_last_of( "\\/" );
      if( endOfPath > m_FileName.max_size() )
        {
        endOfPath = 0; // there is no path
        }
      m_Ifstream.close();        // Close the header
      std::string dataFileName;
      dataFileName = m_FileName.substr( 0, endOfPath+1 );     
      dataFileName += key;
      m_FileName = dataFileName;    // Compose the data filename
      m_Ifstream.open( m_FileName.c_str(), std::ios::in | std::ios::binary );
      return;
      }
    else 
      {
      // That is the end of the header
      return;
      }
    }

    // Unknown code, get the rest of the line
    m_Ifstream.getline( restOfTheLine, maxLineLength );

  }
  
}




void 
MetaImageIO
::SwapBytesIfNecessary( void* buffer, unsigned long numberOfPixels )
{
  switch(m_MetaPixelType)
    {
    case CHAR:
      {
      if ( m_ImageByteOrder == LittleEndian &&
        ByteSwapper<char>::IsBigEndian() )
        {
        ByteSwapper<char>::SwapRangeBE((char*)buffer, numberOfPixels );
        }
      else if ( m_ImageByteOrder == BigEndian &&
        ByteSwapper<char>::IsLittleEndian() )
        {
        ByteSwapper<char>::SwapRangeLE((char *)buffer, numberOfPixels );
        }
      break;
      }
    case UCHAR:
      {
      if ( m_ImageByteOrder == LittleEndian &&
        ByteSwapper<unsigned char>::IsBigEndian() )
        {
        ByteSwapper<unsigned char>::SwapRangeBE((unsigned char*)buffer, numberOfPixels );
        }
      else if ( m_ImageByteOrder == BigEndian &&
        ByteSwapper<unsigned char>::IsLittleEndian() )
        {
        ByteSwapper<unsigned char>::SwapRangeLE((unsigned char *)buffer, numberOfPixels );
        }
      break;
      }
    case SHORT:
      {
      if ( m_ImageByteOrder == LittleEndian &&
        ByteSwapper<short>::IsBigEndian() )
        {
        ByteSwapper<short>::SwapRangeBE((short*)buffer, numberOfPixels );
        }
      else if ( m_ImageByteOrder == BigEndian &&
        ByteSwapper<short>::IsLittleEndian() )
        {
        ByteSwapper<short>::SwapRangeLE((short *)buffer, numberOfPixels );
        }
      break;
      }
    case USHORT:
      {
      if ( m_ImageByteOrder == LittleEndian &&
        ByteSwapper<unsigned short>::IsBigEndian() )
        {
        ByteSwapper<unsigned short>::SwapRangeBE((unsigned short*)buffer, numberOfPixels );
        }
      else if ( m_ImageByteOrder == BigEndian &&
        ByteSwapper<unsigned short>::IsLittleEndian() )
        {
        ByteSwapper<unsigned short>::SwapRangeLE((unsigned short *)buffer, numberOfPixels );
        }
      break;
      }
    case INT:
      {
      if ( m_ImageByteOrder == LittleEndian &&
        ByteSwapper<int>::IsBigEndian() )
        {
        ByteSwapper<int>::SwapRangeBE((int*)buffer, numberOfPixels );
        }
      else if ( m_ImageByteOrder == BigEndian &&
        ByteSwapper<int>::IsLittleEndian() )
        {
        ByteSwapper<int>::SwapRangeLE((int *)buffer, numberOfPixels );
        }
      break;
      }
    case UINT:
      {
      if ( m_ImageByteOrder == LittleEndian &&
        ByteSwapper<unsigned int>::IsBigEndian() )
        {
        ByteSwapper<unsigned int>::SwapRangeBE((unsigned int*)buffer, numberOfPixels );
        }
      else if ( m_ImageByteOrder == BigEndian &&
        ByteSwapper<unsigned int>::IsLittleEndian() )
        {
        ByteSwapper<unsigned int>::SwapRangeLE((unsigned int *)buffer, numberOfPixels );
        }
      break;
      }
    case LONG:
      {
      if ( m_ImageByteOrder == LittleEndian &&
        ByteSwapper<long>::IsBigEndian() )
        {
        ByteSwapper<long>::SwapRangeBE((long*)buffer, numberOfPixels );
        }
      else if ( m_ImageByteOrder == BigEndian &&
        ByteSwapper<long>::IsLittleEndian() )
        {
        ByteSwapper<long>::SwapRangeLE((long *)buffer, numberOfPixels );
        }
      break;
      }
    case ULONG:
      {
      if ( m_ImageByteOrder == LittleEndian &&
        ByteSwapper<unsigned long>::IsBigEndian() )
        {
        ByteSwapper<unsigned long>::SwapRangeBE((unsigned long*)buffer, numberOfPixels );
        }
      else if ( m_ImageByteOrder == BigEndian &&
        ByteSwapper<unsigned long>::IsLittleEndian() )
        {
        ByteSwapper<unsigned long>::SwapRangeLE((unsigned long *)buffer, numberOfPixels );
        }
      break;
      }
    case FLOAT:
      {
      if ( m_ImageByteOrder == LittleEndian &&
        ByteSwapper<float>::IsBigEndian() )
        {
        ByteSwapper<float>::SwapRangeBE((float*)buffer, numberOfPixels );
        }
      else if ( m_ImageByteOrder == BigEndian &&
        ByteSwapper<float>::IsLittleEndian() )
        {
        ByteSwapper<float>::SwapRangeLE((float *)buffer, numberOfPixels );
        }
      break;
      }
    case DOUBLE:
      {
      if ( m_ImageByteOrder == LittleEndian &&
        ByteSwapper<double>::IsBigEndian() )
        {
        ByteSwapper<double>::SwapRangeBE((double*)buffer, numberOfPixels );
        }
      else if ( m_ImageByteOrder == BigEndian &&
        ByteSwapper<double>::IsLittleEndian() )
        {
        ByteSwapper<double>::SwapRangeLE((double *)buffer, numberOfPixels );
        }
      }
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Pixel Type Unknown");
      throw exception;
    }

}




} // end namespace itk
