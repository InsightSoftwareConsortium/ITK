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
#include "itkBMPImageIO.h"
#include "itkByteSwapper.h"
#include "itksys/SystemTools.hxx"
#include <iostream>

namespace itk
{
/** Constructor */
BMPImageIO::BMPImageIO() :
  m_BitMapOffset( 0 ),
  m_FileLowerLeft( 0 ),
  m_Depth( 8 ),
  m_NumberOfColors( 0 ),
  m_ColorPaletteSize( 0 ),
  m_BMPCompression( 0 ),
  m_BMPDataSize( 0 ),
  m_ColorPalette( 0 ) // palette has no element by default
{
  this->SetNumberOfDimensions( 2 );

  m_ByteOrder = BigEndian;
  m_ComponentType = UCHAR;
  m_PixelType = SCALAR;

  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;

  this->AddSupportedWriteExtension(".bmp");
  this->AddSupportedWriteExtension(".BMP");

  this->AddSupportedReadExtension(".bmp");
  this->AddSupportedReadExtension(".BMP");
}

/** Destructor */
BMPImageIO::~BMPImageIO()
{}

bool BMPImageIO::CanReadFile(const char *filename)
{
  // First check the filename
  std::string fname = filename;

  if ( fname == "" )
    {
    itkDebugMacro(<< "No filename specified.");
    }

  bool                   extensionFound = false;
  std::string::size_type BMPPos = fname.rfind(".bmp");
  if ( ( BMPPos != std::string::npos )
       && ( BMPPos == fname.length() - 4 ) )
    {
    extensionFound = true;
    }

  BMPPos = fname.rfind(".BMP");
  if ( ( BMPPos != std::string::npos )
       && ( BMPPos == fname.length() - 4 ) )
    {
    extensionFound = true;
    }

  if ( !extensionFound )
    {
    itkDebugMacro(<< "The filename extension is not recognized");
    return false;
    }

  // Now check the content
  std::ifstream inputStream;
  try
    {
    this->OpenFileForReading( inputStream, fname );
    }
  catch( ExceptionObject & )
    {
    return false;
    }

  char magic_number1, magic_number2;
  inputStream.read( (char *)&magic_number1, sizeof( char ) );
  inputStream.read( (char *)&magic_number2, sizeof( char ) );

  if ( ( magic_number1 != 'B' ) || ( magic_number2 != 'M' ) )
    {
    std::cerr << "BMPImageIO : Magic Number Fails = " << magic_number1 << " : " << magic_number2 << std::endl;
    inputStream.close();
    return false;
    }

  long tmp;
  long infoSize;
  int  iinfoSize;  // in case we are on a 64bit machine
  int  itmp;       // in case we are on a 64bit machine

  // get the size of the file
  ::size_t sizeLong = sizeof( long );
  if ( sizeLong == 4 )
    {
    inputStream.read( (char *)&tmp, 4 );
    // skip 4 bytes
    inputStream.read( (char *)&tmp, 4 );
    // read the offset
    inputStream.read( (char *)&tmp, 4 );
    }
  else
    {
    inputStream.read( (char *)&itmp, 4 );
    // skip 4 bytes
    inputStream.read( (char *)&itmp, 4 );
    // read the offset
    inputStream.read( (char *)&itmp, 4 );
    }

  // get size of header
  if ( sizeLong == 4 )   // if we are on a 32 bit machine
    {
    inputStream.read( (char *)&infoSize, sizeof( long ) );
    ByteSwapper< long >::SwapFromSystemToLittleEndian(&infoSize);
    // error checking
    if ( ( infoSize != 40 ) && ( infoSize != 12 ) )
      {
      inputStream.close();
      return false;
      }
    }
  else    // else we are on a 64bit machine
    {
    inputStream.read( (char *)&iinfoSize, 4 );
    ByteSwapper< int >::SwapFromSystemToLittleEndian(&iinfoSize);
    infoSize = iinfoSize;

    // error checking
    if ( ( infoSize != 40 ) && ( infoSize != 12 ) )
      {
      inputStream.close();
      return false;
      }
    }

  inputStream.close();
  return true;
}

bool BMPImageIO::CanWriteFile(const char *name)
{
  std::string filename = name;

  if ( filename == "" )
    {
    itkDebugMacro(<< "No filename specified.");
    }

  bool                   extensionFound = false;
  std::string::size_type BMPPos = filename.rfind(".bmp");
  if ( ( BMPPos != std::string::npos )
       && ( BMPPos == filename.length() - 4 ) )
    {
    extensionFound = true;
    }

  BMPPos = filename.rfind(".BMP");
  if ( ( BMPPos != std::string::npos )
       && ( BMPPos == filename.length() - 4 ) )
    {
    extensionFound = true;
    }

  if ( !extensionFound )
    {
    itkDebugMacro(<< "The filename extension is not recognized");
    return false;
    }

  return true;
}

void BMPImageIO::Read(void *buffer)
{
  char *        p = static_cast< char * >( buffer );
  unsigned long l = 0;
  char *        value;

  this->OpenFileForReading( m_Ifstream, m_FileName );

  // If the file is RLE compressed
  // RLE-compressed files are lower-left
  // About the RLE compression algorithm:
  // http://msdn.microsoft.com/en-us/library/windows/desktop/dd183383%28v=vs.85%29.aspx
  if ( m_BMPCompression == 1 && (this->GetNumberOfComponents() == 3 ||
                                 this->GetIsReadAsScalarPlusPalette() )  )
    {
    value = new char[m_BMPDataSize + 1];
    m_Ifstream.seekg(m_BitMapOffset, std::ios::beg);
    m_Ifstream.read( (char *)value, m_BMPDataSize );

    SizeValueType posLine = 0;
    SizeValueType line = m_Dimensions[1] - 1;
    for ( unsigned int i = 0; i < m_BMPDataSize; i++ )
      {
      unsigned char byte1 = value[i];
      i++;
      unsigned char byte2 = value[i];
      if(byte1 == 0)
        {
        if(byte2 == 0)
          {
          // End of line
          line--;
          posLine = 0;
          continue;
          }
        else if(byte2 == 1)
          {
          // End of bitmap data
          break;
          }
        else if(byte2 == 2)
          {
          // Delta
          i++;
          unsigned char dx = value[i];
          i++;
          unsigned char dy = value[i];
          posLine += dx;
          line -= dy;
          continue;
          }
        else
          {
          // Unencoded run
          if ( !this->GetIsReadAsScalarPlusPalette() )
            {
              for ( unsigned long j = 0; j < byte2; j++ )
                {
                i++;
                RGBPixelType rgb = this->GetColorPaletteEntry(value[i]);
                l = 3 * ( line * m_Dimensions[0] + posLine );
                p[l]     = rgb.GetBlue();
                p[l + 1] = rgb.GetGreen();
                p[l + 2] = rgb.GetRed();
                posLine++;
                }
            }
            else
            {
               for ( unsigned long j = 0; j < byte2; j++ )
                 {
                 i++;
                 l = ( line * m_Dimensions[0] + posLine );
                 p[l] = value[i];
                 posLine++;
                 }
            }
          // If a run's length is odd, the it is padded with 0
          if(byte2 % 2)
            {
            i++;
            }
          }
        }
      else
        {
        // Encoded run
        if ( !this->GetIsReadAsScalarPlusPalette() )
          {
          RGBPixelType rgb = this->GetColorPaletteEntry(byte2);
          for ( unsigned long j = 0; j < byte1; j++ )
            {
            l = 3 * ( line * m_Dimensions[0] + posLine );
            p[l]     = rgb.GetBlue();
            p[l + 1] = rgb.GetGreen();
            p[l + 2] = rgb.GetRed();
            posLine++;
            }
          }
        else
          {
          for ( unsigned long j = 0; j < byte1; j++ )
            {
            l = ( line * m_Dimensions[0] + posLine );
            p[l] = byte2;
            posLine++;
            }
          }
        }
      }
    }
  else
    {
    // File is not compressed
    // Read one row at a time
    long streamRead = m_Dimensions[0] * m_Depth / 8;
    long paddedStreamRead = streamRead;
    unsigned long step = this->GetNumberOfComponents();
    if ( streamRead % 4 )
      {
      paddedStreamRead = ( ( streamRead / 4 ) + 1 ) * 4;
      }
    value = new char[paddedStreamRead + 1];

    for ( unsigned int id = 0; id < m_Dimensions[1]; id++ )
      {
      const unsigned int line_id = m_FileLowerLeft ? (m_Dimensions[1] - id - 1) : id;
      m_Ifstream.seekg(m_BitMapOffset + paddedStreamRead * line_id, std::ios::beg);
      m_Ifstream.read( (char *)value, paddedStreamRead );
      for ( long i = 0; i < streamRead; i++ )
        {
        if ( this->GetNumberOfComponents() == 1 )
          {
          p[l++] = value[i];
          }
        else
          {
          if ( m_ColorPaletteSize == 0 )
            {
            if ( this->GetNumberOfComponents() == 3 )
              {
              p[l++] = value[i + 2];
              p[l++] = value[i + 1];
              p[l++] = value[i];
              }
            if ( this->GetNumberOfComponents() == 4 )
              {
              p[l++] = value[i + 3];
              p[l++] = value[i + 2];
              p[l++] = value[i + 1];
              p[l++] = value[i];
              }
            i += step - 1;
            }
          else
            {
            RGBPixelType rgb = this->GetColorPaletteEntry(value[i]);
            p[l++] = rgb.GetBlue();
            p[l++] = rgb.GetGreen();
            p[l++] = rgb.GetRed();
            }
          }
        }
      }
    }
  delete[] value;
  m_Ifstream.close();
}

/**
 *  Read Information about the BMP file
 *  and put the cursor of the stream just before the first data pixel
 */
void BMPImageIO::ReadImageInformation()
{
  int   xsize, ysize;
  long  tmp;
  short stmp;
  long  infoSize;
  int   iinfoSize; // in case we are on a 64bit machine
  int   itmp;      // in case we are on a 64bit machine

  // Now check the content
  this->OpenFileForReading( m_Ifstream, m_FileName );

  char magic_number1, magic_number2;
  m_Ifstream.read( (char *)&magic_number1, sizeof( char ) );
  m_Ifstream.read( (char *)&magic_number2, sizeof( char ) );

  if ( ( magic_number1 != 'B' ) || ( magic_number2 != 'M' ) )
    {
    m_Ifstream.close();
    itkExceptionMacro("BMPImageIO : Magic Number Fails = " << magic_number1 << " : " << magic_number2);
    }

  // get the size of the file
  ::size_t sizeLong = sizeof( long );
  if ( sizeLong == 4 )
    {
    m_Ifstream.read( (char *)&tmp, 4 );
    // skip 4 bytes
    m_Ifstream.read( (char *)&tmp, 4 );
    // read the offset
    m_Ifstream.read( (char *)&tmp, 4 );
    m_BitMapOffset = tmp;
    ByteSwapper< long >::SwapFromSystemToLittleEndian(&m_BitMapOffset);
    }
  else
    {
    m_Ifstream.read( (char *)&itmp, 4 );
    // skip 4 bytes
    m_Ifstream.read( (char *)&itmp, 4 );
    // read the offset
    m_Ifstream.read( (char *)&itmp, 4 );
    ByteSwapper< int >::SwapFromSystemToLittleEndian(&itmp);
    m_BitMapOffset = static_cast< long >( itmp );
    }

  // get size of header
  if ( sizeLong == 4 )   // if we are on a 32 bit machine
    {
    m_Ifstream.read( (char *)&infoSize, 4 );
    ByteSwapper< long >::SwapFromSystemToLittleEndian(&infoSize);
    // error checking
    if ( ( infoSize != 40 ) && ( infoSize != 12 ) )
      {
      itkExceptionMacro(<< "Unknown file type! " << m_FileName.c_str()
                        << " is not a Windows BMP file!");
      }

    // there are two different types of BMP files
    if ( infoSize == 40 )
      {
      // now get the dimensions
      m_Ifstream.read( (char *)&xsize, 4 );
      ByteSwapper< int >::SwapFromSystemToLittleEndian(&xsize);
      m_Ifstream.read( (char *)&ysize, 4 );
      ByteSwapper< int >::SwapFromSystemToLittleEndian(&ysize);
      }
    else
      {
      m_Ifstream.read( (char *)&stmp, sizeof( short ) );
      ByteSwapper< short >::SwapFromSystemToLittleEndian(&stmp);
      xsize = stmp;
      m_Ifstream.read( (char *)&stmp, sizeof( short ) );
      ByteSwapper< short >::SwapFromSystemToLittleEndian(&stmp);
      ysize = stmp;
      }
    }
  else // else we are on a 64bit machine
    {
    m_Ifstream.read( (char *)&iinfoSize, sizeof( int ) );
    ByteSwapper< int >::SwapFromSystemToLittleEndian(&iinfoSize);

    infoSize = iinfoSize;

    // error checking
    if ( ( infoSize != 40 ) && ( infoSize != 12 ) )
      {
      itkExceptionMacro(<< "Unknown file type! " << m_FileName.c_str()
                        << " is not a Windows BMP file!");
      }

    // there are two different types of BMP files
    if ( infoSize == 40 )
      {
      // now get the dimensions
      m_Ifstream.read( (char *)&xsize, 4 );
      ByteSwapper< int >::SwapFromSystemToLittleEndian(&xsize);
      m_Ifstream.read( (char *)&ysize, 4 );
      ByteSwapper< int >::SwapFromSystemToLittleEndian(&ysize);
      }
    else
      {
      stmp = 0;
      m_Ifstream.read( (char *)&stmp, 2 );
      ByteSwapper< short >::SwapFromSystemToLittleEndian(&stmp);
      xsize = stmp;
      m_Ifstream.read( (char *)&stmp, 2 );
      ByteSwapper< short >::SwapFromSystemToLittleEndian(&stmp);
      ysize = stmp;
      }
    }

  // is corner in upper left or lower left
  if ( ysize < 0 )
    {
    ysize = -ysize;
    m_FileLowerLeft = 0;
    }
  else
    {
    m_FileLowerLeft = 1;
    }

  this->SetNumberOfDimensions(2);
  m_Dimensions[0] = xsize;
  m_Dimensions[1] = ysize;

  // ignore planes
  m_Ifstream.read( (char *)&stmp, 2 );
  // read depth
  m_Ifstream.read( (char *)&m_Depth, 2 );
  ByteSwapper< short >::SwapFromSystemToLittleEndian(&m_Depth);

  if ( ( m_Depth != 8 ) && ( m_Depth != 24 ) && ( m_Depth != 32 ) )
    {
    m_Ifstream.close();
    itkExceptionMacro("Only BMP depths of (8,24,32) are supported. Not " << m_Depth);
    }

  if ( infoSize == 40 )
    {
    if ( sizeLong == 4 )
      {
      // Compression
      m_Ifstream.read( (char *)&m_BMPCompression, 4 );
      ByteSwapper< long >::SwapFromSystemToLittleEndian(&m_BMPCompression);
      // Image Data Size
      m_Ifstream.read( (char *)&m_BMPDataSize, 4 );
      ByteSwapper< unsigned long >::SwapFromSystemToLittleEndian(&m_BMPDataSize);
      // Horizontal Resolution
      m_Ifstream.read( (char *)&tmp, 4 );
      // Vertical Resolution
      m_Ifstream.read( (char *)&tmp, 4 );
      // Number of colors
      m_Ifstream.read( (char *)&tmp, 4 );
      m_NumberOfColors = static_cast< unsigned short >( tmp );
      // Number of important colors
      m_Ifstream.read( (char *)&tmp, 4 );
      }
    else
      {
      // Compression
      m_Ifstream.read( (char *)&itmp, 4 );
      ByteSwapper< int >::SwapFromSystemToLittleEndian(&itmp);
      m_BMPCompression = static_cast< long >( itmp );
      // Image Data Size
      m_Ifstream.read( (char *)&itmp, 4 );
      ByteSwapper< int >::SwapFromSystemToLittleEndian(&itmp);
      m_BMPDataSize = static_cast< unsigned long >( itmp );
      // Horizontal Resolution
      m_Ifstream.read( (char *)&itmp, 4 );
      ByteSwapper< int >::SwapFromSystemToLittleEndian(&itmp);
      // Vertical Resolution
      m_Ifstream.read( (char *)&itmp, 4 );
      ByteSwapper< int >::SwapFromSystemToLittleEndian(&itmp);
      // Number of colors
      m_Ifstream.read( (char *)&itmp, 4 );
      ByteSwapper< int >::SwapFromSystemToLittleEndian(&itmp);
      m_NumberOfColors = static_cast< unsigned short >( itmp );
      // Number of important colors
      m_Ifstream.read( (char *)&itmp, 4 );
      }
    }

  // http://msdn.microsoft.com/en-us/library/windows/desktop/dd183376%28v=vs.85%29.aspx
  if(m_BMPCompression == 1 && !m_FileLowerLeft)
    {
    m_Ifstream.close();
    itkExceptionMacro("Compressed BMP are not supposed to be upper-left.");
    }

  // Read the color palette. Only used for 1,4 and 8 bit images.
  if ( m_Depth <= 8 )
    {
    if ( m_NumberOfColors )
      {
      m_ColorPaletteSize = ( ( 1 << m_Depth ) < m_NumberOfColors ) ? ( 1 << m_Depth ) : m_NumberOfColors;
      }
    else
      {
      m_ColorPaletteSize = ( 1 << m_Depth );
      }
    }
  else
    {
    m_ColorPaletteSize = 0;
    }
  unsigned char uctmp;
  m_ColorPalette.resize(m_ColorPaletteSize);
  for ( unsigned long i = 0; i < m_ColorPaletteSize; i++ )
    {
    RGBPixelType p;
    m_Ifstream.read( (char *)&uctmp, 1 );
    p.SetRed(uctmp);
    m_Ifstream.read( (char *)&uctmp, 1 );
    p.SetGreen(uctmp);
    m_Ifstream.read( (char *)&uctmp, 1 );
    p.SetBlue(uctmp);
    m_Ifstream.read( (char *)&tmp, 1 );
    m_ColorPalette[i] = p;
    }

  m_IsReadAsScalarPlusPalette = false;
  switch ( m_Depth )
    {
    case 1:
    case 4:
    case 8:
      {
      if ( this->GetExpandRGBPalette() )
        {
        this->SetNumberOfComponents(3);
        m_PixelType = RGB;
        }
      else
        {
        this->SetNumberOfComponents(1);
        m_PixelType = SCALAR;
        m_IsReadAsScalarPlusPalette = true;
        }
      break;
      }
    case 24:
      {
      this->SetNumberOfComponents(3);
      m_PixelType = RGB;
      break;
      }
    case 32:
      {
      this->SetNumberOfComponents(4);
      m_PixelType = RGBA;
      break;
      }
    }

  m_Ifstream.close();
}

void
BMPImageIO
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
    default:
      itkExceptionMacro(<< "Pixel Type Unknown");
    }
}

void
BMPImageIO
::Write32BitsInteger(unsigned int value)
{
  char tmp = static_cast<char>(value % 256);
  m_Ofstream.write( &tmp, sizeof( char ) );
  tmp = static_cast< char >( ( value % 65536L ) / 256 );
  m_Ofstream.write( &tmp, sizeof( char ) );
  tmp = static_cast< char >( ( value / 65536L ) % 256 );
  m_Ofstream.write( &tmp, sizeof( char ) );
  tmp = static_cast< char >( ( value / 65536L ) / 256 );
  m_Ofstream.write( &tmp, sizeof( char ) );
}

void
BMPImageIO
::Write16BitsInteger(unsigned short value)
{
  char tmp = static_cast<char>(value % 256);
  m_Ofstream.write( &tmp, sizeof( char ) );
  tmp = static_cast< char >( ( value % 65536L ) / 256 );
  m_Ofstream.write( &tmp, sizeof( char ) );
}

BMPImageIO::RGBPixelType
BMPImageIO
::GetColorPaletteEntry(const unsigned char entry) const
{
  if ( entry < m_ColorPalette.size() )
    {
    return m_ColorPalette[entry];
    }
  else
    {
    RGBPixelType p;
    p.SetRed(0);
    p.SetGreen(0);
    p.SetBlue(0);
    return p;
    }
}

void
BMPImageIO
::WriteImageInformation(void)
{}

/** The write function is not implemented */
void
BMPImageIO
::Write(const void *buffer)
{
  unsigned int nDims = this->GetNumberOfDimensions();

  if ( nDims != 2 )
    {
    itkExceptionMacro(<< "BMPImageIO cannot write images with a dimension != 2");
    }

  if ( this->GetComponentType() != UCHAR )
    {
    itkExceptionMacro(<< "BMPImageIO supports unsigned char only");
    }
  if ( ( this->m_NumberOfComponents != 1 )
       && ( this->m_NumberOfComponents != 3 )
       && ( this->m_NumberOfComponents != 4 ) )
    {
    itkExceptionMacro(<< "BMPImageIO supports 1,3 or 4 components only");
    }

  this->OpenFileForWriting( m_Ofstream, m_FileName );

  //
  //
  // A BMP file has four sections:
  //
  // * BMP Header                         14 bytes
  // * Bitmap Information (DIB header)    40 bytes (Windows V3)
  // * Color Palette
  // * Bitmap Data
  //
  // For more details:
  //
  //             http://en.wikipedia.org/wiki/BMP_file_format
  //
  //

  // Write the BMP header
  //
  // Header structure is represented by first a 14 byte field, then the bitmap
  // info header.
  //
  // The 14 byte field:
  //
  // Offset Length Description
  //
  //   0      2    Contain the string, "BM", (Hex: 42 4D)
  //   2      4    The length of the entire file.
  //   6      2    Reserved for application data. Usually zero.
  //   8      2    Reserved for application data. Usually zero.
  //  10      4    Provides an offset from the start of the file
  //               to the first byte of image sample data. This
  //               is normally 54 bytes (Hex: 36)
  //
  char tmp = 66;
  m_Ofstream.write( &tmp, sizeof( char ) );
  tmp = 77;
  m_Ofstream.write( &tmp, sizeof( char ) );

  const unsigned int bpp = this->GetNumberOfComponents();
  long               bytesPerRow = m_Dimensions[0] * bpp;
  if ( bytesPerRow % 4 )
    {
    bytesPerRow = ( ( bytesPerRow / 4 ) + 1 ) * 4;
    }
  const unsigned long paddedBytes = bytesPerRow - ( m_Dimensions[0] * bpp );

  const unsigned int rawImageDataSize = static_cast< unsigned int >( ( bytesPerRow * m_Dimensions[1] ) );
  unsigned int       fileSize = ( rawImageDataSize ) + 54;
  if ( bpp == 1 )
    {
    fileSize += 1024; // need colour LUT
    }
  this->Write32BitsInteger(fileSize);

  const unsigned short applicationReservedValue = 0;
  this->Write16BitsInteger(applicationReservedValue);
  this->Write16BitsInteger(applicationReservedValue);

  unsigned int offsetToBinaryDataStart = 54;
  if ( bpp == 1 ) // more space is needed for the LUT
    {
    offsetToBinaryDataStart += 1024;
    }
  this->Write32BitsInteger(offsetToBinaryDataStart);
  //
  // End of BMP header, 14 bytes written so far
  //

  //
  // Write the DIB header
  //
  // Offset Length Description
  //
  //  14      4    Size of the header (40 bytes)(Hex: 28)
  //
  //
  //  Color Palette
  //
  //  If the bit_count is 1, 4 or 8, the structure must be followed by a colour
  //  lookup table, with 4 bytes per entry, the first 3 of which identify the
  //  blue, green and red intensities, respectively.
  //
  //  Finally the pixel data
  //
  const unsigned int bitmapHeaderSize = 40;
  this->Write32BitsInteger(bitmapHeaderSize);

  // image width
  this->Write32BitsInteger(static_cast<unsigned int>(m_Dimensions[0]));

  // image height -ve means top to bottom
  this->Write32BitsInteger(static_cast<unsigned int>(m_Dimensions[1]));

  // Set `planes'=1 (mandatory)
  const unsigned short numberOfColorPlanes = 1;
  this->Write16BitsInteger(numberOfColorPlanes);

  // Set bits per pixel.
  unsigned short numberOfBitsPerPixel = 0;
  switch ( bpp )
    {
    case 4:
      numberOfBitsPerPixel = 32;
      break;
    case 3:
      numberOfBitsPerPixel = 24;
      break;
    case 1:
      numberOfBitsPerPixel = 8;
      break;
    default:
      itkExceptionMacro(<< "Number of components not supported.");
    }
  this->Write16BitsInteger(numberOfBitsPerPixel);

  const unsigned int compressionMethod = 0;
  this->Write32BitsInteger(compressionMethod);
  this->Write32BitsInteger(rawImageDataSize);

  // Assuming spacing is in millimeters,
  // the resolution is set here in pixel per meter.
  // The specification calls for a signed integer, but
  // here we force it to be an unsigned integer to avoid
  // dealing with directions in a subterraneous way.
  const unsigned int horizontalResolution = Math::Round< unsigned int >(1000.0 / m_Spacing[0]);
  const unsigned int verticalResolution = Math::Round< unsigned int >(1000.0 / m_Spacing[1]);

  this->Write32BitsInteger(horizontalResolution);
  this->Write32BitsInteger(verticalResolution);

  // zero here defaults to 2^n colors in the palette
  const unsigned int numberOfColorsInPalette = 0;
  this->Write32BitsInteger(numberOfColorsInPalette);

  // zero here indicates that all colors in the palette are important.
  const unsigned int numberOfImportantColorsInPalette = 0;
  this->Write32BitsInteger(numberOfImportantColorsInPalette);
  //
  // End of DIB header, 54 bytes written so far
  //

  //
  // Write down colour LUT
  //
  // only when using 1 byte per pixel
  //
  if ( bpp == 1 )
    {
    for ( unsigned int n = 0; n < 256; n++ )
      {
      char tmp2 = static_cast< unsigned char >( n );
      m_Ofstream.write( &tmp2, sizeof( char ) );
      m_Ofstream.write( &tmp2, sizeof( char ) );
      m_Ofstream.write( &tmp2, sizeof( char ) );
      m_Ofstream.write( &tmp, sizeof( char ) );
      }
    }

  //
  // Write down the raw binary pixel data
  //
  unsigned int i;
  for ( unsigned int h = 0; h < m_Dimensions[1]; h++ )
    {
    const char  paddingValue = 0;
    const char *ptr = static_cast< const char * >( buffer );
    ptr += ( m_Dimensions[1] - ( h + 1 ) ) * m_Dimensions[0] * bpp;
    if ( bpp == 1 )
      {
      for ( i = 0; i < m_Dimensions[0]; i++ )
        {
        m_Ofstream.write( ptr, sizeof( char ) );
        ptr++;
        }
      for ( i = 0; i < paddedBytes; i++ )
        {
        m_Ofstream.write( &paddingValue, sizeof( char ) );
        }
      }
    if ( bpp == 3 )
      {
      for ( i = 0; i < m_Dimensions[0]; i++ )
        {
        ptr += 2;
        m_Ofstream.write( ptr, sizeof( char ) );
        ptr--;
        m_Ofstream.write( ptr, sizeof( char ) );
        ptr--;
        m_Ofstream.write( ptr, sizeof( char ) );
        ptr += 3;
        }
      for ( i = 0; i < paddedBytes; i++ )
        {
        m_Ofstream.write( &paddingValue, sizeof( char ) );
        }
      }
    if ( bpp == 4 )
      {
      for ( i = 0; i < m_Dimensions[0]; i++ )
        {
        ptr += 3;
        m_Ofstream.write( ptr, sizeof( char ) );
        ptr--;
        m_Ofstream.write( ptr, sizeof( char ) );
        ptr--;
        m_Ofstream.write( ptr, sizeof( char ) );
        ptr--;
        m_Ofstream.write( ptr, sizeof( char ) );
        ptr += 4;
        }
      for ( i = 0; i < paddedBytes; i++ )
        {
        m_Ofstream.write( &paddingValue, sizeof( char ) );
        }
      }
    }
}

/** Print Self Method */
void BMPImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "BitMapOffset: " << m_BitMapOffset << std::endl;
  os << indent << "FileLowerLeft: " << m_FileLowerLeft << std::endl;
  os << indent << "Depth: " << m_Depth << std::endl;
  os << indent << "NumberOfColors: " << m_NumberOfColors << std::endl;
  os << indent << "ColorPaletteSize: " << m_ColorPaletteSize << std::endl;
  os << indent << "BMPCompression: " << m_BMPCompression << std::endl;
  os << indent << "DataSize: " << m_BMPDataSize << std::endl;
  if ( m_IsReadAsScalarPlusPalette )
    {
    os << "Read as Scalar Image plus palette" << "\n";
    }
  if( m_ColorPalette.size() > 0  )
    {
    os << indent << "ColorPalette:" << std::endl;
    for( unsigned int i = 0; i < m_ColorPalette.size(); ++i )
      {
      os << indent << "[" << i << "]"
         << itk::NumericTraits< PaletteType::value_type >::PrintType( m_ColorPalette[i] ) << std::endl;
      }
    }
}
} // end namespace itk
