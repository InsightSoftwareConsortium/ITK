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
#include "itkByteSwapper.h"



namespace itk
{

MetaImageIO::MetaImageIO()
{
  this->SetNumberOfDimensions(2);
  m_PixelType  = UCHAR;
  m_ByteOrder = BigEndian;
}



MetaImageIO::~MetaImageIO()
{
  m_Ifstream.close();
}



void MetaImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "PixelType " << m_PixelType << "\n";
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
      m_ByteOrder = LittleEndian;
    }
    else if ( strcmp( key, "True" ) == 0 )
    {
      m_ByteOrder = BigEndian;
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

  if( strcmp(key,"ElementSize")==0 ||
      strcmp(key,"ElementSpacing") ==0 ) 
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
  

bool MetaImageIO::CanWriteFile(const char*)
{
  if(   m_FileName != "" &&
      ( m_FileName.find(".mha") < m_FileName.length() ||
        m_FileName.find(".mhd") < m_FileName.length()    ) )
    {
    return true;
    }
  return false;
}



const std::type_info& MetaImageIO::GetPixelType() const
{
  switch(m_PixelType)
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
  switch(m_PixelType)
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

  
void MetaImageIO::Read(void* buffer)
{
  unsigned int dimensions = this->GetNumberOfDimensions();
  unsigned int numberOfPixels = 1;
  for( unsigned int dim=0; dim< dimensions; dim++ )
    {
    numberOfPixels *= m_Dimensions[ dim ];
    }

  char * p = static_cast<char *>(buffer);

  const unsigned int imageSizeInBytes = this->GetImageSizeInBytes();
  unsigned int numberOfUpdates = 100;
  while( imageSizeInBytes < numberOfUpdates )
    {
    numberOfUpdates /= 10;
    }

  unsigned int numberOfBytesReadBetweenProgressUpdates = imageSizeInBytes / numberOfUpdates;
  unsigned int numberOfBytesLeftForReading = imageSizeInBytes;
  unsigned int numberOfBytesRead = 0;


  while( numberOfBytesLeftForReading )
    {

    m_Ifstream.read( p, numberOfBytesReadBetweenProgressUpdates );

    bool success = !m_Ifstream.bad();
    if( !success )
      {
      itkExceptionMacro("Error reading image data.");
      }

    p += numberOfBytesReadBetweenProgressUpdates;
    numberOfBytesLeftForReading -= numberOfBytesReadBetweenProgressUpdates;
    numberOfBytesRead           += numberOfBytesReadBetweenProgressUpdates;

    if( numberOfBytesLeftForReading < numberOfBytesReadBetweenProgressUpdates )
      {
      numberOfBytesReadBetweenProgressUpdates = numberOfBytesLeftForReading;
      }

    this->UpdateProgress( static_cast<float>( numberOfBytesRead ) / 
                          static_cast<float>( imageSizeInBytes  ) );

    }

  m_Ifstream.close();

  SwapBytesIfNecessary( buffer, numberOfPixels );

  this->UpdateProgress( 1.0f );

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
  if( strcmp(key, "ElementSpacing")==0  ) 
    {
    if( !GetSeparatorCharacter( m_Ifstream ) )
      {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Missing \"=\" after ElementSpacing");
      throw exception;
      }
    for( unsigned int dim=0; dim< this->GetNumberOfDimensions(); dim++ )
      {
      m_Ifstream >> m_Spacing[ dim ];
      }
    m_Ifstream.getline( restOfTheLine, maxLineLength );
    continue;
    }
  if( strcmp(key,   "ElementSize" )==0 )  // same as ElementSpacing
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
      m_PixelType = UCHAR;
      }
    else if( strcmp( elementType, "MET_CHAR" ) == 0 )
      {
      m_ComponentType = CHAR;
      m_PixelType = CHAR;
      }
    else if( strcmp( elementType, "MET_USHORT" ) == 0 )
      {
      m_ComponentType = USHORT;
      m_PixelType = USHORT;
      }
    else if( strcmp( elementType, "MET_SHORT" ) == 0 )
      {
      m_ComponentType = SHORT;
      m_PixelType = SHORT;
      }
    else if( strcmp( elementType, "MET_UINT" ) == 0 )
      {
      m_ComponentType = UINT;
      m_PixelType = UINT;
      }
    else if( strcmp( elementType, "MET_INT" ) == 0 )
      {
      m_ComponentType = INT;
      m_PixelType = INT;
      }
    else if( strcmp( elementType, "MET_ULONG" ) == 0 )
      {
      m_ComponentType = ULONG;
      m_PixelType = ULONG;
      }
    else if( strcmp( elementType, "MET_LONG" ) == 0 )
      {
      m_ComponentType = LONG;
      m_PixelType = LONG;
      }
    else if( strcmp( elementType, "MET_UFLOAT" ) == 0 )
      {
      m_ComponentType = FLOAT;
      m_PixelType = FLOAT;
      }
    else if( strcmp( elementType, "MET_DOUBLE" ) == 0 )
      {
      m_ComponentType = DOUBLE;
      m_PixelType = DOUBLE;
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
      m_Ifstream.close();        // Close the header
      std::string dataFileName;
      size_t endOfPath = m_FileName.find_last_of( "\\/" );
      if( endOfPath == std::string::npos )
        {
        dataFileName = key;     // there is no path
        }
      else
        {
        dataFileName = m_FileName.substr( 0, endOfPath+1 );
        dataFileName += key;
        }

      m_FileName = dataFileName;    // Compose the data filename
      m_Ifstream.open( m_FileName.c_str(), std::ios::in | std::ios::binary );
      if( !m_Ifstream )
        {
        itkExceptionMacro("Error opening image data file.");
        }
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
  switch(m_PixelType)
    {
    case CHAR:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper<char>::SwapRangeFromSystemToLittleEndian(
                                  (char*)buffer, numberOfPixels );
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper<char>::SwapRangeFromSystemToBigEndian(
                                  (char *)buffer, numberOfPixels );
        }
      break;
      }
    case UCHAR:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper<unsigned char>::SwapRangeFromSystemToLittleEndian(
                        (unsigned char*)buffer, numberOfPixels );
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper<unsigned char>::SwapRangeFromSystemToBigEndian(
                        (unsigned char *)buffer, numberOfPixels );
        }
      break;
      }
    case SHORT:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper<short>::SwapRangeFromSystemToLittleEndian(
                                  (short*)buffer, numberOfPixels );
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper<short>::SwapRangeFromSystemToBigEndian(
                                  (short *)buffer, numberOfPixels );
        }
      break;
      }
    case USHORT:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper<unsigned short>::SwapRangeFromSystemToLittleEndian(
                                (unsigned short*)buffer, numberOfPixels );
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper<unsigned short>::SwapRangeFromSystemToBigEndian(
                                (unsigned short *)buffer, numberOfPixels );
        }
      break;
      }
    case INT:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper<int>::SwapRangeFromSystemToLittleEndian(
                                    (int*)buffer, numberOfPixels );
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper<int>::SwapRangeFromSystemToBigEndian(
                                    (int *)buffer, numberOfPixels );
        }
      break;
      }
    case UINT:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper<unsigned int>::SwapRangeFromSystemToLittleEndian(
                                    (unsigned int*)buffer, numberOfPixels );
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper<unsigned int>::SwapRangeFromSystemToBigEndian(
                                    (unsigned int *)buffer, numberOfPixels );
        }
      break;
      }
    case LONG:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper<long>::SwapRangeFromSystemToLittleEndian(
                                  (long*)buffer, numberOfPixels );
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper<long>::SwapRangeFromSystemToBigEndian(
                                  (long *)buffer, numberOfPixels );
        }
      break;
      }
    case ULONG:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper<unsigned long>::SwapRangeFromSystemToLittleEndian(
                                      (unsigned long*)buffer, numberOfPixels );
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper<unsigned long>::SwapRangeFromSystemToBigEndian(
                                      (unsigned long *)buffer, numberOfPixels );
        }
      break;
      }
    case FLOAT:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper<float>::SwapRangeFromSystemToLittleEndian(
                                  (float*)buffer, numberOfPixels );
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper<float>::SwapRangeFromSystemToBigEndian(
                                  (float *)buffer, numberOfPixels );
        }
      break;
      }
    case DOUBLE:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper<double>::SwapRangeFromSystemToLittleEndian(
                                  (double*)buffer, numberOfPixels );
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper<double>::SwapRangeFromSystemToBigEndian(
                                  (double *)buffer, numberOfPixels );
        }
      break;
      }
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Pixel Type Unknown");
      throw exception;
    }

}




/**
 *
 */
void 
MetaImageIO
::WriteImageInformation(void)
{

  m_Ofstream.open( m_FileName.c_str(), std::ios::out | std::ios::binary );
  if( m_Ofstream.fail() )
    {
    itkExceptionMacro(<<"File cannot be open for writing");
    }


  const unsigned int numberOfDimensions = this->GetNumberOfDimensions();

  m_Ofstream << "NDims = " << numberOfDimensions << std::endl;

  m_Ofstream << "DimSize = ";
  for( unsigned int i=0; i< numberOfDimensions; i++ )
    {
    m_Ofstream <<  this->GetDimensions( i ) << "  ";  
    }
  m_Ofstream << std::endl;

  m_Ofstream << "ElementSpacing = ";
  for( unsigned int j=0; j< numberOfDimensions; j++ )
    {
    m_Ofstream <<  this->GetSpacing( j ) << "  ";  
    }
  m_Ofstream << std::endl;


  m_Ofstream << "ElementType = ";
  std::string metaImagePixelType;
  switch( m_PixelType )
  {
  case UCHAR:
      metaImagePixelType = "MET_UCHAR";
      break;
  case CHAR:
      metaImagePixelType = "MET_CHAR";
      break;
  case USHORT:
      metaImagePixelType = "MET_USHORT";
      break;
  case SHORT:
      metaImagePixelType = "MET_SHORT";
      break;
  case UINT:
      metaImagePixelType = "MET_UINT";
      break;
  case INT:
      metaImagePixelType = "MET_INT";
      break;
  case ULONG:
      metaImagePixelType = "MET_ULONG";
      break;
  case LONG:
      metaImagePixelType = "MET_LONG";
      break;
  case FLOAT:
      metaImagePixelType = "MET_FLOAT";
      break;
  case DOUBLE:
      metaImagePixelType = "MET_DOUBLE";
      break;
  default:
      itkExceptionMacro(<<"Unknown type of pixel");
  }
  m_Ofstream << metaImagePixelType << std::endl; 

  std::string rawFilename = m_FileName;

  rawFilename.replace( m_FileName.rfind( ".mha" ), 4, ".raw" );

  m_Ofstream << "ElementDataFile = " << rawFilename << std::endl; 
  
  m_Ofstream.close();

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
