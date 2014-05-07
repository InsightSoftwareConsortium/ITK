/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkGiplImageIO.h"
#include "itkByteSwapper.h"
#include <iostream>
#include "itk_zlib.h"

namespace itk
{
class GiplImageIOInternals
{
public:
  gzFile m_GzFile;
};

/*  IMAGE TYPE DEFINITIONS  */

#define GIPL_BINARY   1
#define GIPL_CHAR     7
#define GIPL_U_CHAR   8
#define GIPL_SHORT    15
#define GIPL_U_SHORT  16
#define GIPL_U_INT    31
#define GIPL_INT      32
#define GIPL_FLOAT    64
#define GIPL_DOUBLE   65
#define GIPL_C_SHORT    144
#define GIPL_C_INT      160
#define GIPL_C_FLOAT    192
#define GIPL_C_DOUBLE   193
#define GIPL_SURFACE    200
#define GIPL_POLYGON    201

/*  ORIENTATION DEFINITIONS (flag1)  */

#define UNDEFINED 0
#define UNDEFINED_PROJECTION 1
#define AP_PROJECTION 2
#define LATERAL_PROJECTION 3
#define OBLIQUE_PROJECTION 4
#define UNDEFINED_TOMO 8
#define AXIAL 9
#define CORONAL 10
#define SAGITTAL 11
#define OBLIQUE_TOMO 12

/*  FORMAT DEFINITIONS  */

#define FORMAT_GIPL  0
#define FORMAT_GIPL_STRING "Gipl"
#define FORMAT_MAYO  1
#define FORMAT_MAYO_STRING  "Mayo"
#define FORMAT_NM_IGE  2
#define FORMAT_NM_IGE_STRING  "Starcam"

#define GIPL_MAGIC_NUMBER 0xefffe9b0
#define GIPL_MAGIC_NUMBER2 0x2ae389b8

/** Constructor */
GiplImageIO::GiplImageIO()
{
  m_Internal = new GiplImageIOInternals;
  m_Internal->m_GzFile = ITK_NULLPTR;
  m_ByteOrder = BigEndian;
  m_IsCompressed = false;
}

/** Destructor */
GiplImageIO::~GiplImageIO()
{
  if ( m_IsCompressed )
    {
    if ( m_Internal->m_GzFile != ITK_NULLPTR )
      {
      gzclose(m_Internal->m_GzFile);
      m_Internal->m_GzFile = ITK_NULLPTR;
      }
    }
  else
    {
    m_Ifstream.close();
    }
  delete m_Internal;
}

bool GiplImageIO::CanReadFile(const char *filename)
{
  // First check the filename extension
  bool extensionFound = CheckExtension(filename);

  if ( !extensionFound )
    {
    itkDebugMacro(<< "The filename extension is not recognized");
    return false;
    }

  // Now check the content
  if ( m_IsCompressed == false )
    {
    std::ifstream inputStream;
    try
      {
      this->OpenFileForReading( inputStream, filename );
      }
    catch( ExceptionObject & )
      {
      return false;
      }

    inputStream.seekg(252);
    unsigned int magic_number;
    inputStream.read( (char *)&magic_number, static_cast< std::streamsize >( sizeof( unsigned int ) ) );

    if ( m_ByteOrder == BigEndian )
      {
      ByteSwapper< unsigned int >::SwapFromSystemToBigEndian(&magic_number);
      }
    else if ( m_ByteOrder == LittleEndian )
      {
      ByteSwapper< unsigned int >::SwapFromSystemToLittleEndian(&magic_number);
      }

    if ( ( magic_number == GIPL_MAGIC_NUMBER ) || ( magic_number == GIPL_MAGIC_NUMBER2 ) )
      {
      inputStream.close();
      return true;
      }

    inputStream.close();
    }
  else
    {
    m_Internal->m_GzFile = gzopen(filename, "rb");
    if ( m_Internal->m_GzFile == ITK_NULLPTR )
      {
      return false;
      }

    gzseek(m_Internal->m_GzFile, 252, SEEK_SET);
    unsigned int magic_number;
    gzread( m_Internal->m_GzFile, (char *)&magic_number, static_cast< unsigned int >( sizeof( unsigned int ) ) );

    if ( m_ByteOrder == BigEndian )
      {
      ByteSwapper< unsigned int >::SwapFromSystemToBigEndian(&magic_number);
      }
    else if ( m_ByteOrder == LittleEndian )
      {
      ByteSwapper< unsigned int >::SwapFromSystemToLittleEndian(&magic_number);
      }

    if ( ( magic_number == GIPL_MAGIC_NUMBER ) || ( magic_number == GIPL_MAGIC_NUMBER2 ) )
      {
      gzclose(m_Internal->m_GzFile);
      m_Internal->m_GzFile = ITK_NULLPTR;
      return true;
      }
    gzclose(m_Internal->m_GzFile);
    m_Internal->m_GzFile = ITK_NULLPTR;
    }
  return false;
}

bool GiplImageIO::CanWriteFile(const char *name)
{
  std::string filename = name;

  if ( filename == "" )
    {
    itkDebugMacro(<< "No filename specified.");
    }

  bool extensionFound = CheckExtension(name);

  if ( !extensionFound )
    {
    itkDebugMacro(<< "The filename extension is not recognized");
    return false;
    }

  return true;
}

void GiplImageIO::Read(void *buffer)
{
  const uint32_t dimensions = this->GetNumberOfDimensions();
  uint32_t numberOfPixels = 1;

  for ( unsigned int dim = 0; dim < dimensions; dim++ )
    {
    numberOfPixels *= static_cast<uint32_t>(m_Dimensions[dim]);
    }

  char *p = static_cast< char * >( buffer );
  if ( m_IsCompressed )
    {
    gzread( m_Internal->m_GzFile, p, static_cast< unsigned int >( this->GetImageSizeInBytes() ) );
    }
  else
    {
    m_Ifstream.read( p, static_cast< std::streamsize >( this->GetImageSizeInBytes() ) );
    }

  bool success;
  if ( m_IsCompressed )
    {
    if ( p != ITK_NULLPTR )
      {
      success = true;
      }
    else
      {
      success = false;
      }
    }
  else
    {
    success = !m_Ifstream.bad();
    }

  if ( m_IsCompressed )
    {
    gzclose(m_Internal->m_GzFile);
    m_Internal->m_GzFile = ITK_NULLPTR;
    }
  else
    {
    m_Ifstream.close();
    }
  if ( !success )
    {
    itkExceptionMacro("Error reading image data.");
    }

  SwapBytesIfNecessary(buffer, numberOfPixels);
}

/**
 *  Read Information about the Gipl file
 *  and put the cursor of the stream just before the first data pixel
 */
void GiplImageIO::ReadImageInformation()
{
  unsigned int i;

  CheckExtension( m_FileName.c_str() );

  if ( m_IsCompressed )
    {
    m_Internal->m_GzFile = gzopen(m_FileName.c_str(), "rb");
    if ( m_Internal->m_GzFile == ITK_NULLPTR )
      {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("File cannot be read");
      throw exception;
      }
    }
  else
    {
    this->OpenFileForReading( m_Ifstream, m_FileName );
    }

  unsigned short dims[4];

  unsigned int numberofdimension = 0;
  for ( i = 0; i < 4; i++ )
    {
    dims[i] = 0;
    }

  for ( i = 0; i < 4; i++ )
    {
    if ( m_IsCompressed )
      {
      gzread( m_Internal->m_GzFile, (char *)&dims[i], static_cast< unsigned int >( sizeof( unsigned short ) ) );
      }
    else
      {
      m_Ifstream.read( (char *)&dims[i], sizeof( unsigned short ) );
      }
    if ( m_ByteOrder == BigEndian )
      {
      ByteSwapper< unsigned short >::SwapFromSystemToBigEndian(&dims[i]);
      }
    else if ( m_ByteOrder == LittleEndian )
      {
      ByteSwapper< unsigned short >::SwapFromSystemToLittleEndian(&dims[i]);
      }

    if ( dims[i] > 0 )
      {
      if ( i < 3 )
        {
        numberofdimension++;
        }
      else if ( dims[i] > 1 )
        {
        numberofdimension++;
        }
      }
    }

  this->SetNumberOfDimensions(numberofdimension);

  for ( i = 0; i < numberofdimension; i++ )
    {
    m_Dimensions[i] = dims[i];
    }

  unsigned short image_type;

  if ( m_IsCompressed )
    {
    gzread( m_Internal->m_GzFile, (char *)&image_type, sizeof( unsigned short ) );
    }
  else
    {
    m_Ifstream.read( (char *)&image_type, sizeof( unsigned short ) );
    }

  if ( m_ByteOrder == BigEndian )
    {
    ByteSwapper< unsigned short >::SwapFromSystemToBigEndian(&image_type);
    }

  m_PixelType = SCALAR;
  switch ( image_type )
    {
    case  GIPL_BINARY:
      m_ComponentType = UCHAR; break;
    case  GIPL_CHAR:
      m_ComponentType = CHAR; break;
    case  GIPL_U_CHAR:
      m_ComponentType = UCHAR; break;
    case  GIPL_SHORT:
      m_ComponentType = SHORT; break;
    case  GIPL_U_SHORT:
      m_ComponentType = USHORT; break;
    case  GIPL_U_INT:
      m_ComponentType = UINT; break;
    case  GIPL_INT:
      m_ComponentType = INT; break;
    case  GIPL_FLOAT:
      m_ComponentType = FLOAT; break;
    case  GIPL_DOUBLE:
      m_ComponentType = DOUBLE; break;
    }

  float pixdim[4];           /*   10   16  X,Y,Z,T pixel dimensions mm */
  for ( i = 0; i < 4; i++ )
    {
    if ( m_IsCompressed )
      {
      gzread( m_Internal->m_GzFile, (char *)&pixdim[i], sizeof( float ) );
      }
    else
      {
      m_Ifstream.read( (char *)&pixdim[i], sizeof( float ) );
      }
    if ( m_ByteOrder == BigEndian )
      {
      ByteSwapper< float >::SwapFromSystemToBigEndian(&pixdim[i]);
      }
    else if ( m_ByteOrder == LittleEndian )
      {
      ByteSwapper< float >::SwapFromSystemToLittleEndian(&pixdim[i]);
      }

    if ( i < numberofdimension )
      {
      m_Spacing[i] = pixdim[i];
      }
    }

  char line1[80];            /*   26   80  Patient / Text field        */
  for ( i = 0; i < 80; i++ )
    {
    if ( m_IsCompressed )
      {
      gzread( m_Internal->m_GzFile, (char *)&line1[i], static_cast< unsigned int >( sizeof( char ) ) );
      }
    else
      {
      m_Ifstream.read( (char *)&line1[i], sizeof( char ) );
      }
    }

  float matrix[20];          /*  106   80                              */
  for ( i = 0; i < 20; i++ )
    {
    if ( m_IsCompressed )
      {
      gzread( m_Internal->m_GzFile, (char *)&matrix[i], static_cast< unsigned int >( sizeof( float ) ) );
      }
    else
      {
      m_Ifstream.read( (char *)&matrix[i], sizeof( float ) );
      }

    if ( m_ByteOrder == BigEndian )
      {
      ByteSwapper< float >::SwapFromSystemToBigEndian(&matrix[i]);
      }
    else if ( m_ByteOrder == LittleEndian )
      {
      ByteSwapper< float >::SwapFromSystemToLittleEndian(&matrix[i]);
      }
    }

  char flag1;                /*  186    1  Orientation flag (below)    */
  if ( m_IsCompressed )
    {
    gzread( m_Internal->m_GzFile, (char *)&flag1, static_cast< unsigned int >( sizeof( char ) ) );
    }
  else
    {
    m_Ifstream.read( (char *)&flag1, sizeof( char ) );
    }

  if ( m_ByteOrder == BigEndian )
    {
    ByteSwapper< char >::SwapFromSystemToBigEndian(&flag1);
    }
  else if ( m_ByteOrder == LittleEndian )
    {
    ByteSwapper< char >::SwapFromSystemToLittleEndian(&flag1);
    }

  char flag2;                /*  187    1                              */
  if ( m_IsCompressed )
    {
    gzread( m_Internal->m_GzFile, (char *)&flag2, static_cast< unsigned int >( sizeof( char ) ) );
    }
  else
    {
    m_Ifstream.read( (char *)&flag2, sizeof( char ) );
    }

  if ( m_ByteOrder == BigEndian )
    {
    ByteSwapper< char >::SwapFromSystemToBigEndian(&flag2);
    }
  else if ( m_ByteOrder == LittleEndian )
    {
    ByteSwapper< char >::SwapFromSystemToLittleEndian(&flag2);
    }

  double min;                /*  188    8  Minimum voxel value         */
  if ( m_IsCompressed )
    {
    gzread( m_Internal->m_GzFile, (char *)&min, static_cast< unsigned int >( sizeof( double ) ) );
    }
  else
    {
    m_Ifstream.read( (char *)&min, sizeof( double ) );
    }

  double max;                /*  196    8  Maximum voxel value         */
  if ( m_IsCompressed )
    {
    gzread( m_Internal->m_GzFile, (char *)&max, static_cast< unsigned int >( sizeof( double ) ) );
    }
  else
    {
    m_Ifstream.read( (char *)&max, sizeof( double ) );
    }

  double origin[4];          /*  204   32  X,Y,Z,T offset              */
  for ( i = 0; i < 4; i++ )
    {
    if ( m_IsCompressed )
      {
      gzread( m_Internal->m_GzFile, (char *)&origin[i], static_cast< unsigned int >( sizeof( double ) ) );
      }
    else
      {
      m_Ifstream.read( (char *)&origin[i], sizeof( double ) );
      }

    if ( m_ByteOrder == BigEndian )
      {
      ByteSwapper< double >::SwapFromSystemToBigEndian(&origin[i]);
      }
    else if ( m_ByteOrder == LittleEndian )
      {
      ByteSwapper< double >::SwapFromSystemToLittleEndian(&origin[i]);
      }

    if ( i < numberofdimension )
      {
      m_Origin[i] = origin[i];
      }
    }

  float pixval_offset;       /*  236    4                              */
  if ( m_IsCompressed )
    {
    gzread( m_Internal->m_GzFile, (char *)&pixval_offset, static_cast< unsigned int >( sizeof( float ) ) );
    }
  else
    {
    m_Ifstream.read( (char *)&pixval_offset, sizeof( float ) );
    }

  if ( m_ByteOrder == BigEndian )
    {
    ByteSwapper< float >::SwapFromSystemToBigEndian(&pixval_offset);
    }
  else if ( m_ByteOrder == LittleEndian )
    {
    ByteSwapper< float >::SwapFromSystemToLittleEndian(&pixval_offset);
    }

  float pixval_cal;          /*  240    4                              */
  if ( m_IsCompressed )
    {
    gzread( m_Internal->m_GzFile, (char *)&pixval_cal, static_cast< unsigned int >( sizeof( float ) ) );
    }
  else
    {
    m_Ifstream.read( (char *)&pixval_cal, sizeof( float ) );
    }

  if ( m_ByteOrder == BigEndian )
    {
    ByteSwapper< float >::SwapFromSystemToBigEndian(&pixval_cal);
    }
  else if ( m_ByteOrder == LittleEndian )
    {
    ByteSwapper< float >::SwapFromSystemToLittleEndian(&pixval_cal);
    }

  float user_def1;           /*  244    4  Inter-slice Gap             */
  if ( m_IsCompressed )
    {
    gzread( m_Internal->m_GzFile, (char *)&user_def1, static_cast< unsigned int >( sizeof( float ) ) );
    }
  else
    {
    m_Ifstream.read( (char *)&user_def1, sizeof( float ) );
    }

  if ( m_ByteOrder == BigEndian )
    {
    ByteSwapper< float >::SwapFromSystemToBigEndian(&user_def1);
    }
  else if ( m_ByteOrder == LittleEndian )
    {
    ByteSwapper< float >::SwapFromSystemToLittleEndian(&user_def1);
    }

  float user_def2;           /*  248    4  User defined field          */
  if ( m_IsCompressed )
    {
    gzread( m_Internal->m_GzFile, (char *)&user_def2, static_cast< unsigned int >( sizeof( float ) ) );
    }
  else
    {
    m_Ifstream.read( (char *)&user_def2, sizeof( float ) );
    }

  if ( m_ByteOrder == BigEndian )
    {
    ByteSwapper< float >::SwapFromSystemToBigEndian(&user_def2);
    }
  else if ( m_ByteOrder == LittleEndian )
    {
    ByteSwapper< float >::SwapFromSystemToLittleEndian(&user_def2);
    }

  unsigned int magic_number; /*  252    4 Magic Number                 */
  if ( m_IsCompressed )
    {
    gzread( m_Internal->m_GzFile, (char *)&magic_number, static_cast< unsigned int >( sizeof( unsigned int ) ) );
    }
  else
    {
    m_Ifstream.read( (char *)&magic_number, sizeof( unsigned int ) );
    }

  if ( m_ByteOrder == BigEndian )
    {
    ByteSwapper< unsigned int >::SwapFromSystemToBigEndian(&magic_number);
    }
  else if ( m_ByteOrder == LittleEndian )
    {
    ByteSwapper< unsigned int >::SwapFromSystemToLittleEndian(&magic_number);
    }
}

void
GiplImageIO
::SwapBytesIfNecessary(void *buffer, SizeValueType numberOfPixels)
{
  switch ( m_ComponentType )
    {
    case CHAR:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper< char >::SwapRangeFromSystemToLittleEndian(
          (char *)buffer, numberOfPixels);
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper< char >::SwapRangeFromSystemToBigEndian(
          (char *)buffer, numberOfPixels);
        }
      break;
      }
    case UCHAR:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper< unsigned char >::SwapRangeFromSystemToLittleEndian(
          (unsigned char *)buffer, numberOfPixels);
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper< unsigned char >::SwapRangeFromSystemToBigEndian(
          (unsigned char *)buffer, numberOfPixels);
        }
      break;
      }
    case SHORT:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper< short >::SwapRangeFromSystemToLittleEndian(
          (short *)buffer, numberOfPixels);
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper< short >::SwapRangeFromSystemToBigEndian(
          (short *)buffer, numberOfPixels);
        }
      break;
      }
    case USHORT:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper< unsigned short >::SwapRangeFromSystemToLittleEndian(
          (unsigned short *)buffer, numberOfPixels);
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper< unsigned short >::SwapRangeFromSystemToBigEndian(
          (unsigned short *)buffer, numberOfPixels);
        }
      break;
      }
    case FLOAT:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper< float >::SwapRangeFromSystemToLittleEndian(
          (float *)buffer, numberOfPixels);
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper< float >::SwapRangeFromSystemToBigEndian(
          (float *)buffer, numberOfPixels);
        }
      break;
      }
    case DOUBLE:
      {
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper< double >::SwapRangeFromSystemToLittleEndian(
          (double *)buffer, numberOfPixels);
        }
      else if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper< double >::SwapRangeFromSystemToBigEndian(
          (double *)buffer, numberOfPixels);
        }
      break;
      }
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Pixel Type Unknown");
      throw exception;
    }
}

void
GiplImageIO
::WriteImageInformation(void)
{
  //not possible to write a Gipl file
}

/** The write function is not implemented */
void
GiplImageIO
::Write(const void *buffer)
{
  CheckExtension( m_FileName.c_str() );

  unsigned int nDims = this->GetNumberOfDimensions();

  if ( m_IsCompressed )
    {
    m_Internal->m_GzFile = gzopen(m_FileName.c_str(), "wb");
    if ( m_Internal->m_GzFile == ITK_NULLPTR )
      {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("File cannot be write");
      throw exception;
      }
    }
  else
    {
    this->OpenFileForWriting( m_Ofstream, m_FileName );
    }

  for (unsigned int i = 0; i < 4; i++ )
    {
    unsigned short value;
    if ( i < nDims )
      {
      value = this->GetDimensions(i);
      if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper< unsigned short >::SwapFromSystemToBigEndian(&value);
        }
      else if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper< unsigned short >::SwapFromSystemToLittleEndian(&value);
        }

      if ( m_IsCompressed )
        {
        gzwrite( m_Internal->m_GzFile, (char *)&( value ), static_cast< unsigned int >( sizeof( unsigned short ) ) );
        }
      else
        {
        m_Ofstream.write( (char *)&( value ), sizeof( unsigned short ) );
        }
      }
    else
      {
      value = 1;
      if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper< unsigned short >::SwapFromSystemToBigEndian(&value);
        }
      else if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper< unsigned short >::SwapFromSystemToLittleEndian(&value);
        }
      if ( m_IsCompressed )
        {
        gzwrite( m_Internal->m_GzFile, (char *)&( value ), static_cast< unsigned int >( sizeof( unsigned short ) ) );
        }
      else
        {
        m_Ofstream.write( (char *)&value, sizeof( unsigned short ) );
        }
      }
    }

  unsigned short image_type;
  switch ( m_ComponentType )
    {
    case  CHAR:
      image_type = GIPL_CHAR; break;
    case  UCHAR:
      image_type = GIPL_U_CHAR; break;
    case  SHORT:
      image_type = GIPL_SHORT; break;
    case  USHORT:
      image_type = GIPL_U_SHORT; break;
    case  UINT:
      image_type = GIPL_U_INT; break;
    case  INT:
      image_type = GIPL_INT; break;
    case  FLOAT:
      image_type = GIPL_FLOAT; break;
    case  DOUBLE:
      image_type = GIPL_DOUBLE; break;
    default:
      itkExceptionMacro ("Invalid type: " << m_ComponentType);
    }

  if ( m_ByteOrder == BigEndian )
    {
    ByteSwapper< unsigned short >::SwapFromSystemToBigEndian( (unsigned short *)&image_type );
    }
  if ( m_ByteOrder == LittleEndian )
    {
    ByteSwapper< unsigned short >::SwapFromSystemToLittleEndian( (unsigned short *)&image_type );
    }

  if ( m_IsCompressed )
    {
    gzwrite( m_Internal->m_GzFile, (char *)&image_type, static_cast< unsigned int >( sizeof( unsigned short ) ) );
    }
  else
    {
    m_Ofstream.write( (char *)&image_type, sizeof( unsigned short ) );
    }

  /*   10   16  X,Y,Z,T pixel dimensions mm */
  for ( unsigned int i = 0; i < 4; i++ )
    {
    if ( i < nDims )
      {
      float value = static_cast< float >( m_Spacing[i] );
      if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper< float >::SwapFromSystemToBigEndian( (float *)&value );
        }
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper< float >::SwapFromSystemToLittleEndian( (float *)&value );
        }
      if ( m_IsCompressed )
        {
        gzwrite( m_Internal->m_GzFile, (char *)&value, static_cast< unsigned int >( sizeof( float ) ) );
        }
      else
        {
        m_Ofstream.write( (char *)&value, sizeof( float ) );
        }
      }
    else
      {
      float value = 1.0f;
      if ( m_ByteOrder == BigEndian )
        {
        ByteSwapper< float >::SwapFromSystemToBigEndian( (float *)&value );
        }
      if ( m_ByteOrder == LittleEndian )
        {
        ByteSwapper< float >::SwapFromSystemToLittleEndian( (float *)&value );
        }
      if ( m_IsCompressed )
        {
        gzwrite( m_Internal->m_GzFile, (char *)&value, static_cast< unsigned int >( sizeof( float ) ) );
        }
      else
        {
        m_Ofstream.write( (char *)&value, sizeof( float ) );
        }
      }
    }

  char line1[80];            /*   26   80  Patient / Text field        */

  for (unsigned int i = 0; i < 80; i++ )
    {
    line1[i] = 0; //initialize
    }

  sprintf(line1, "No Patient Information");
  for ( unsigned int i = 0; i < 80; i++ )
    {
    if ( m_IsCompressed )
      {
      gzwrite( m_Internal->m_GzFile, (char *)&line1[i], static_cast< unsigned int >( sizeof( char ) ) );
      }
    else
      {
      m_Ofstream.write( (char *)&line1[i], sizeof( char ) );
      }
    }

  float matrix[20];          /*  106   80                              */
  for ( unsigned int i = 0; i < 20; i++ )
    {
    matrix[i] = 0; //write zeros
    if ( m_IsCompressed )
      {
      gzwrite( m_Internal->m_GzFile, (char *)&matrix[i], static_cast< unsigned int >( sizeof( float ) ) );
      }
    else
      {
      m_Ofstream.write( (char *)&matrix[i], sizeof( float ) );
      }
    }

  char flag1 = 0;              /*  186    1  Orientation flag (below)    */
  if ( m_IsCompressed )
    {
    gzwrite( m_Internal->m_GzFile, (char *)&flag1, static_cast< unsigned int >( sizeof( char ) ) );
    }
  else
    {
    m_Ofstream.write( (char *)&flag1, sizeof( char ) );
    }

  char flag2 = 0;              /*  187    1                              */
  if ( m_IsCompressed )
    {
    gzwrite( m_Internal->m_GzFile, (char *)&flag2, static_cast< unsigned int >( sizeof( char ) ) );
    }
  else
    {
    m_Ofstream.write( (char *)&flag2, sizeof( char ) );
    }

  double min = 0;               /*  188    8  Minimum voxel value         */
  if ( m_IsCompressed )
    {
    gzwrite( m_Internal->m_GzFile, (char *)&min, static_cast< unsigned int >( sizeof( double ) ) );
    }
  else
    {
    m_Ofstream.write( (char *)&min, sizeof( double ) );
    }

  double max = 0;               /*  196    8  Maximum voxel value         */
  if ( m_IsCompressed )
    {
    gzwrite( m_Internal->m_GzFile, (char *)&max, static_cast< unsigned int >( sizeof( double ) ) );
    }
  else
    {
    m_Ofstream.write( (char *)&max, sizeof( double ) );
    }

  double origin[4];          /*  204   32  X,Y,Z,T offset              */
  for ( unsigned int i = 0; i < 4; i++ )
    {
    if ( i < nDims )
      {
      origin[i] = m_Origin[i];
      }
    else
      {
      origin[i] = 0;
      }

    if ( m_ByteOrder == BigEndian )
      {
      ByteSwapper< double >::SwapFromSystemToBigEndian( (double *)&origin[i] );
      }
    if ( m_ByteOrder == LittleEndian )
      {
      ByteSwapper< double >::SwapFromSystemToLittleEndian( (double *)&origin[i] );
      }

    if ( m_IsCompressed )
      {
      gzwrite( m_Internal->m_GzFile, (char *)&origin[i], static_cast< unsigned int >( sizeof( double ) ) );
      }
    else
      {
      m_Ofstream.write( (char *)&origin[i], sizeof( double ) );
      }
    }

  float pixval_offset = 0;     /*  236    4                            */
  if ( m_IsCompressed )
    {
    gzwrite( m_Internal->m_GzFile, (char *)&pixval_offset, static_cast< unsigned int >( sizeof( float ) ) );
    }
  else
    {
    m_Ofstream.write( (char *)&pixval_offset, sizeof( float ) );
    }

  float pixval_cal = 0;        /*  240    4                              */
  if ( m_IsCompressed )
    {
    gzwrite( m_Internal->m_GzFile, (char *)&pixval_cal, static_cast< unsigned int >( sizeof( float ) ) );
    }
  else
    {
    m_Ofstream.write( (char *)&pixval_cal, sizeof( float ) );
    }

  float user_def1 = 0;         /*  244    4  Inter-slice Gap             */
  if ( m_IsCompressed )
    {
    gzwrite( m_Internal->m_GzFile, (char *)&user_def1, static_cast< unsigned int >( sizeof( float ) ) );
    }
  else
    {
    m_Ofstream.write( (char *)&user_def1, sizeof( float ) );
    }

  float user_def2 = 0;         /*  248    4  User defined field          */
  if ( m_IsCompressed )
    {
    gzwrite( m_Internal->m_GzFile, (char *)&user_def2, static_cast< unsigned int >( sizeof( float ) ) );
    }
  else
    {
    m_Ofstream.write( (char *)&user_def2, sizeof( float ) );
    }

  unsigned int magic_number = GIPL_MAGIC_NUMBER; /*  252    4 Magic Number
                                                                   */
  if ( m_ByteOrder == BigEndian )
    {
    ByteSwapper< unsigned int >::SwapFromSystemToBigEndian(&magic_number);
    }
  if ( m_ByteOrder == LittleEndian )
    {
    ByteSwapper< unsigned int >::SwapFromSystemToLittleEndian(&magic_number);
    }

  if ( m_IsCompressed )
    {
    gzwrite( m_Internal->m_GzFile, (char *)&magic_number, static_cast< unsigned int >( sizeof( unsigned int ) ) );
    }
  else
    {
    m_Ofstream.write( (char *)&magic_number, sizeof( unsigned int ) );
    }

  // Actually do the writing
  //
  this->ComputeStrides();
  if ( m_FileType == ASCII )
    {
    this->WriteBufferAsASCII( m_Ofstream, buffer, this->GetComponentType(),
                              this->GetImageSizeInComponents() );
    }
  else //binary
    {
    const SizeValueType numberOfBytes      = static_cast< SizeValueType >( this->GetImageSizeInBytes() );
    const SizeValueType numberOfComponents = static_cast< SizeValueType >( this->GetImageSizeInComponents() );

    // Swap bytes if necessary
    if ( m_ByteOrder == LittleEndian )
      {
      char *tempBuffer = new char[numberOfBytes];
      memcpy(tempBuffer, buffer, numberOfBytes);
      SwapBytesIfNecessary(tempBuffer, numberOfComponents);
      if ( m_IsCompressed )
        {
        gzwrite(m_Internal->m_GzFile, tempBuffer, numberOfBytes);
        }
      else
        {
        m_Ofstream.write(tempBuffer, numberOfBytes);
        }
      delete[] tempBuffer;
      }
    else if ( m_ByteOrder == BigEndian )
      {
      char *tempBuffer = new char[numberOfBytes];
      memcpy(tempBuffer, buffer, numberOfBytes);
      SwapBytesIfNecessary(tempBuffer, numberOfComponents);
      if ( m_IsCompressed )
        {
        gzwrite(m_Internal->m_GzFile, tempBuffer, numberOfBytes);
        }
      else
        {
        m_Ofstream.write(tempBuffer, numberOfBytes);
        }
      delete[] tempBuffer;
      }
    else
      {
      if ( m_IsCompressed )
        {
        gzwrite(m_Internal->m_GzFile, const_cast< void * >( buffer ), numberOfBytes);
        }
      else
        {
        m_Ofstream.write(static_cast< const char * >( buffer ), numberOfBytes);
        }
      }
    }

  if ( m_IsCompressed )
    {
    gzclose(m_Internal->m_GzFile);
    m_Internal->m_GzFile = ITK_NULLPTR;
    }
  else
    {
    m_Ofstream.close();
    }
}

/** Print Self Method */
void GiplImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "PixelType " << m_PixelType << "\n";
}

bool GiplImageIO::CheckExtension(const char *filename)
{
  std::string fname = filename;

  if ( fname == "" )
    {
    itkDebugMacro(<< "No filename specified.");
    return false;
    }

  bool extensionFound = false;
  m_IsCompressed = false;

  std::string::size_type giplPos = fname.rfind(".gipl");
  if ( ( giplPos != std::string::npos )
       && ( giplPos == fname.length() - 5 ) )
    {
    extensionFound = true;
    }

  giplPos = fname.rfind(".gipl.gz");
  if ( ( giplPos != std::string::npos )
       && ( giplPos == fname.length() - 8 ) )
    {
    extensionFound = true;
    m_IsCompressed = true;
    }

  return extensionFound;
}
} // end namespace itk
