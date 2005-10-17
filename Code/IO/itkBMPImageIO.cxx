/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBMPImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$  

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkBMPImageIO.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include <iostream>
#include <list>
#include <string>
#include <math.h>

namespace itk
{

/** Constructor */
BMPImageIO::BMPImageIO()
{
  m_ByteOrder = BigEndian;
  m_BitMapOffset = 0;
  this->SetNumberOfDimensions(2);
  m_PixelType = SCALAR;
  m_ComponentType = UCHAR;
  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;
  
  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;
  m_FileLowerLeft = 0;
  m_Depth = 8;
  m_Allow8BitBMP = true;
}


/** Destructor */
BMPImageIO::~BMPImageIO()
{
  m_Ifstream.close();
}


bool BMPImageIO::CanReadFile( const char* filename ) 
{ 
  // First check the filename extension
  std::string fname = filename;
  if ( fname == "" )
    {
    itkDebugMacro(<< "No filename specified.");
    }

  bool extensionFound = false;
  std::string::size_type BMPPos = fname.rfind(".bmp");
  if ((BMPPos != std::string::npos)
      && (BMPPos == fname.length() - 4))
    {
    extensionFound = true;
    }

  BMPPos = fname.rfind(".BMP");
  if ((BMPPos != std::string::npos)
      && (BMPPos == fname.length() - 4))
    {
    extensionFound = true;
    }

  if( !extensionFound )
    {
    itkDebugMacro(<<"The filename extension is not recognized");
    return false;
    }

  // Now check the content
  std::ifstream inputStream;
  inputStream.open( filename, std::ios::in | std::ios::binary );
  if( inputStream.fail() )
    {
    return false;
    }
 
  char magic_number1, magic_number2;
  inputStream.read((char*)&magic_number1,sizeof(char));
  inputStream.read((char*)&magic_number2,sizeof(char));

  if ((magic_number1 != 'B')||(magic_number2 != 'M'))
    {
    std::cout << "BMPImageIO : Magic Number Fails = " << magic_number1 << " : " << magic_number2 << std::endl;
    inputStream.close();
    return false;
    }

  long tmp;
  long infoSize;
  int  iinfoSize;  // in case we are on a 64bit machine
  int  itmp;       // in case we are on a 64bit machine

  // get the size of the file
  int sizeLong = sizeof(long);
  if (sizeLong == 4)
    {
    inputStream.read((char*)&tmp,4);
    // skip 4 bytes
    inputStream.read((char*)&tmp,4);
    // read the offset
    inputStream.read((char*)&tmp,4);
    }
  else
    {
    inputStream.read((char*)&itmp,4);
    // skip 4 bytes
    inputStream.read((char*)&itmp,4);
    // read the offset
    inputStream.read((char*)&itmp,4);
    }

  // get size of header
  if (sizeLong == 4)   // if we are on a 32 bit machine
    {
    inputStream.read((char*)&infoSize,sizeof(long));
    ByteSwapper<long>::SwapFromSystemToLittleEndian(&infoSize);                      
    // error checking
    if ((infoSize != 40)&&(infoSize != 12))
      {
      inputStream.close();
      return false;
      }
    }
  else    // else we are on a 64bit machine
    {
    inputStream.read((char*)&iinfoSize,sizeof(int));
    ByteSwapper<int>::SwapFromSystemToLittleEndian(&iinfoSize);
    infoSize = iinfoSize;
    
    // error checking
    if ((infoSize != 40)&&(infoSize != 12))
      {
      inputStream.close();
      return false;
      }
    }

  inputStream.close();
  return true;

}
  


bool BMPImageIO::CanWriteFile( const char * name )
{
  std::string filename = name;
  if ( filename == "" )
    {
    itkDebugMacro(<< "No filename specified.");
    }

  bool extensionFound = false;
  std::string::size_type BMPPos = filename.rfind(".bmp");
  if ((BMPPos != std::string::npos)
      && (BMPPos == filename.length() - 4))
    {
    extensionFound = true;
    }
  
  BMPPos = filename.rfind(".BMP");
  if ((BMPPos != std::string::npos)
      && (BMPPos == filename.length() - 4))
    {
    extensionFound = true;
    }

  if( !extensionFound )
    {
    itkDebugMacro(<<"The filename extension is not recognized");
    return false;
    }

  if( extensionFound )
    {
    return true;
    }
  return false;

}


 
void BMPImageIO::Read(void* buffer)
{

  char * p = static_cast<char *>(buffer);
  unsigned long l=0;
  unsigned long step = this->GetNumberOfComponents();
  long streamRead = m_Dimensions[0]*m_Depth/8;

  long paddedStreamRead = streamRead;
  if( streamRead % 4 )
    {
    paddedStreamRead = ( ( streamRead / 4 ) + 1 ) * 4;
    }

  char* value = new char[paddedStreamRead+1];
  if (m_FileLowerLeft)
    {
    for(unsigned int id=0;id<m_Dimensions[1];id++)
      {
      m_Ifstream.seekg(m_BitMapOffset + paddedStreamRead*(m_Dimensions[1] - id - 1),std::ios::beg);
      m_Ifstream.read((char *)value, paddedStreamRead);

      for(long i=0;i<streamRead;i+=step)
        {
        if(this->GetNumberOfComponents() == 1)
          {
          p[l++]=value[i];
          }
        else
          {
          p[l++]=value[i+2];
          p[l++]=value[i+1];
          p[l++]=value[i];
          }
        }
      }
    }
  else
    {
    m_Ifstream.seekg(m_BitMapOffset,std::ios::beg);
    for(unsigned int id=0;id<m_Dimensions[1];id++)
      {
      m_Ifstream.read((char *)value, streamRead);

      for(long i=0;i<streamRead;i+=step)
        { 
        if(this->GetNumberOfComponents() == 1)
          {
          p[l++]=value[i];
          }
        else
          {
          p[l++]=value[i+2];
          p[l++]=value[i+1];
          p[l++]=value[i];
          }
        }
      }
    }
  delete []value;
  m_Ifstream.close();
}

/** 
 *  Read Information about the BMP file
 *  and put the cursor of the stream just before the first data pixel
 */
void BMPImageIO::ReadImageInformation()
{

  int xsize, ysize;
  long tmp;
  short stmp;
  long infoSize;
  int  iinfoSize;  // in case we are on a 64bit machine
  int  itmp;       // in case we are on a 64bit machine

  // Now check the content
  m_Ifstream.open( m_FileName.c_str(), std::ios::in | std::ios::binary );
  if( m_Ifstream.fail() )
    {
    return;
    }
 
  char magic_number1, magic_number2;
  m_Ifstream.read((char*)&magic_number1,sizeof(char));
  m_Ifstream.read((char*)&magic_number2,sizeof(char));

  if ((magic_number1 != 'B')||(magic_number2 != 'M'))
    {
    std::cout << "BMPImageIO : Magic Number Fails = " << magic_number1 << " : " << magic_number2 << std::endl;
    m_Ifstream.close();
    return;
    }

  // get the size of the file
  int sizeLong = sizeof(long);
  if (sizeLong == 4)
    {
    m_Ifstream.read((char*)&tmp,4);
    // skip 4 bytes
    m_Ifstream.read((char*)&tmp,4);
    // read the offset
    m_Ifstream.read((char*)&tmp,4);
    m_BitMapOffset = tmp;
    ByteSwapper<long>::SwapFromSystemToLittleEndian(&m_BitMapOffset);
    }
  else
    {
    m_Ifstream.read((char*)&itmp,4);
    // skip 4 bytes
    m_Ifstream.read((char*)&itmp,4);
    // read the offset
    m_Ifstream.read((char*)&itmp,4);
    ByteSwapper<int>::SwapFromSystemToLittleEndian(&itmp);
    m_BitMapOffset = static_cast<long>(itmp);
    }

  // get size of header
  if (sizeLong == 4)   // if we are on a 32 bit machine
    {
    m_Ifstream.read((char*)&infoSize,sizeof(long));
    ByteSwapper<long>::SwapFromSystemToLittleEndian(&infoSize);
                       
    // error checking
    if ((infoSize != 40)&&(infoSize != 12))
      {
      itkExceptionMacro(<<"Unknown file type! " << m_FileName.c_str() 
                    <<" is not a Windows BMP file!");
      m_Ifstream.close();
      return;
      }
  
    // there are two different types of BMP files
    if (infoSize == 40)
      {
      // now get the dimensions
      m_Ifstream.read((char*)&xsize,sizeof(long));
      ByteSwapper<int>::SwapFromSystemToLittleEndian(&xsize);
      m_Ifstream.read((char*)&ysize,sizeof(long));
      ByteSwapper<int>::SwapFromSystemToLittleEndian(&ysize);
      }
    else
      {
      m_Ifstream.read((char*)&stmp,sizeof(short));
      ByteSwapper<short>::SwapFromSystemToLittleEndian(&stmp);
      xsize = stmp;
      m_Ifstream.read((char*)&stmp,sizeof(short));
      ByteSwapper<short>::SwapFromSystemToLittleEndian(&stmp);
      ysize = stmp;
      }
    }
  else // else we are on a 64bit machine
    {
    m_Ifstream.read((char*)&iinfoSize,sizeof(int));
    ByteSwapper<int>::SwapFromSystemToLittleEndian(&iinfoSize);
    
    infoSize = iinfoSize;
    
    // error checking
    if ((infoSize != 40)&&(infoSize != 12))
      {
      itkExceptionMacro(<<"Unknown file type! " << m_FileName.c_str() 
                    <<" is not a Windows BMP file!");
      m_Ifstream.close();
      return;
      }
  
    // there are two different types of BMP files
    if (infoSize == 40)
      {
      // now get the dimensions
      m_Ifstream.read((char*)&xsize,sizeof(int));
      ByteSwapper<int>::SwapFromSystemToLittleEndian(&xsize);
      m_Ifstream.read((char*)&ysize,sizeof(int));
      ByteSwapper<int>::SwapFromSystemToLittleEndian(&ysize);
      }
    else
      {
      stmp =0;
      m_Ifstream.read((char*)&xsize,sizeof(short));
      ByteSwapper<short>::SwapFromSystemToLittleEndian(&stmp);
      xsize = stmp;
      m_Ifstream.read((char*)&xsize,sizeof(short));
      ByteSwapper<short>::SwapFromSystemToLittleEndian(&stmp);
      ysize = stmp;
      }
    }
  
  this->SetNumberOfDimensions(2);
  m_Dimensions[0] = xsize;
  m_Dimensions[1] = ysize;
  
  // is corner in upper left or lower left
  if (ysize < 0)
    {
    ysize = ysize*-1;
    m_FileLowerLeft = 0;
    }
  else
    {
    m_FileLowerLeft = 1;
    }
    
  // ignore planes
  m_Ifstream.read((char*)&stmp,sizeof(short));
  // read depth
  m_Ifstream.read((char*)&m_Depth,sizeof(short));
  ByteSwapper<short>::SwapFromSystemToLittleEndian(&m_Depth);

  if ((m_Depth != 8)&&(m_Depth != 24))
    {
    std::cout <<"Only BMP depths of (8,24) are supported. Not " << m_Depth << std::endl;
    m_Ifstream.close();
    return;
    }
  
  // skip over rest of info for long format
  if (infoSize == 40)
    {
    m_Ifstream.read((char*)&tmp,4);
    m_Ifstream.read((char*)&tmp,4);
    m_Ifstream.read((char*)&tmp,4);
    m_Ifstream.read((char*)&tmp,4);
    m_Ifstream.read((char*)&tmp,4);
    }

  if ((m_Depth == 8) && m_Allow8BitBMP)
    {
    this->SetNumberOfComponents(1);
    }
  else
    {
    this->SetNumberOfComponents(3);
    }

}


void 
BMPImageIO
::SwapBytesIfNecessary( void* buffer, unsigned long numberOfPixels )
{
  switch(m_ComponentType)
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
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Pixel Type Unknown");
      throw exception;
    }
}


void 
BMPImageIO
::WriteImageInformation(void)
{
  
}


/** The write function is not implemented */
void 
BMPImageIO
::Write( const void* buffer) 
{
 
  unsigned int nDims = this->GetNumberOfDimensions();

  if(nDims != 2)
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("BMPImageIO cannot write images with a dimension != 2");
    throw exception;
    }

  if(this->GetComponentType() != UCHAR)
    {
    itkExceptionMacro(<<"BMPImageIO supports unsigned char only");
    }
  if( (this->m_NumberOfComponents != 1) && (this->m_NumberOfComponents !=3) )
    {
    itkExceptionMacro(<<"BMPImageIO supports 1 or 3 components only");
    }
  

#ifdef __sgi
  // Create the file. This is required on some older sgi's
  std::ofstream tFile(m_FileName.c_str(),std::ios::out);
  tFile.close();                    
#endif

  m_Ofstream.open(m_FileName.c_str(), std::ios::binary | std::ios::out);
  if( m_Ofstream.fail() )
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("File cannot be write");
    throw exception;
    }

  // Write the header  
  // spit out the BMP header
  // Header structure is represented by first a 14 byte field, then the bitmap 
  // info header. 
  // 
  // The 14 byte field:
  // first 2 bytes contain the string, "BM", and whose next 4 bytes contain
  // the length of the entire file.  The next 4 bytes must be 0. The final
  // 4 bytes provides an offset from the start of the file to the first byte
  // of image sample data. If the bit_count is 1, 4 or 8, the structure must 
  // be followed by a colour lookup table, with 4 bytes per entry, the first 
  // 3 of which identify the blue, green and red intensities, respectively.
  //
  char tmp = 66;
  m_Ofstream.write(&tmp,sizeof(char));
  tmp = 77;
  m_Ofstream.write(&tmp,sizeof(char));

  unsigned int bpp = this->GetNumberOfComponents(); 
  long bytesPerRow = m_Dimensions[0]*bpp;  
  if ( bytesPerRow % 4 )
    {
    bytesPerRow = ( ( bytesPerRow / 4 ) + 1 ) * 4;
    }
  unsigned long paddedBytes = bytesPerRow - (m_Dimensions[0]*bpp);

  long temp = (long)( bytesPerRow * m_Dimensions[1]) + 54L;
  if( bpp == 1 ) 
    {
    temp += 1024; // need colour LUT
    }

  tmp = temp%256;
  m_Ofstream.write(&tmp,sizeof(char));
  tmp = (temp%65536L)/256;
  m_Ofstream.write(&tmp,sizeof(char));
  tmp = temp/65536L;
  m_Ofstream.write(&tmp,sizeof(char));
  tmp =0;

  int row;
  for(row = 0; row < 5; row++)
    {
    m_Ofstream.write(&tmp,sizeof(char));
    }

  tmp=54;
  m_Ofstream.write(&tmp,sizeof(char));
  if( bpp == 1 ) 
    {
    tmp=4;
    m_Ofstream.write(&tmp,sizeof(char));
    }
  else if(bpp==3)
    {
    tmp=0;
    m_Ofstream.write(&tmp,sizeof(char));
    }
  tmp =0;
  m_Ofstream.write(&tmp,sizeof(char));
  m_Ofstream.write(&tmp,sizeof(char));
  
  // info header, 14 bytes written so far
  tmp = 40;  // bitmap header size
  m_Ofstream.write(&tmp,sizeof(char));
  tmp =0; 
  m_Ofstream.write(&tmp,sizeof(char));
  m_Ofstream.write(&tmp,sizeof(char));
  m_Ofstream.write(&tmp,sizeof(char));
  
  // image width
  tmp = m_Dimensions[0]%256;
  m_Ofstream.write(&tmp,sizeof(char));
  tmp = m_Dimensions[0]/256;
  m_Ofstream.write(&tmp,sizeof(char));
  tmp =0;
  m_Ofstream.write(&tmp,sizeof(char));
  m_Ofstream.write(&tmp,sizeof(char));
  
  // image height -ve means top to bottom
  tmp = m_Dimensions[1]%256;
  m_Ofstream.write(&tmp,sizeof(char));
  tmp = m_Dimensions[1]/256;
  m_Ofstream.write(&tmp,sizeof(char));
  tmp =0;
  m_Ofstream.write(&tmp,sizeof(char));
  m_Ofstream.write(&tmp,sizeof(char));
  
  // Set `planes'=1 (mandatory)
  tmp =1;
  m_Ofstream.write(&tmp,sizeof(char));
  tmp =0;
  m_Ofstream.write(&tmp,sizeof(char));

  // Set bits per pel.  
  if( bpp == 3 ) 
    {
    tmp=24;
    }
  else if( bpp == 1)
    {
    tmp=8;
    }
  else
    {
    itkExceptionMacro(<< "Number of components not supported.") ;
    }
  m_Ofstream.write(&tmp,sizeof(char));
  tmp =0;
  for(row = 0; row < 25; row++)
    {
    m_Ofstream.write(&tmp,sizeof(char));
    }

  // spit out colour LUT
  if (bpp == 1)
    {
    for (unsigned int n=0; n < 256; n++)
      { 
      char tmp2 = static_cast< char >( n );
      m_Ofstream.write(&tmp2,sizeof(char));
      m_Ofstream.write(&tmp2,sizeof(char));
      m_Ofstream.write(&tmp2,sizeof(char));
      m_Ofstream.write(&tmp,sizeof(char));
      }
    }

  unsigned int i;
  for (unsigned int h = 0; h < m_Dimensions[1]; h++)
    {  
      const char paddingValue = 0;
      const char * ptr = static_cast<const char*>(buffer);
      ptr += (m_Dimensions[1]-(h+1))*m_Dimensions[0]*bpp;
      if (bpp == 1)
        {
        for (i = 0; i < m_Dimensions[0]; i++)
          {
          m_Ofstream.write(ptr,sizeof(char));
          ptr++;
          }
        for (i = 0; i < paddedBytes; i++)
          {
          m_Ofstream.write(&paddingValue,sizeof(char));
          }
        }
      if (bpp == 3)
        {
        for (i = 0; i < m_Dimensions[0]; i++)
          {
           ptr+=2;
           m_Ofstream.write(ptr,sizeof(char));
           ptr--;
           m_Ofstream.write(ptr,sizeof(char));
           ptr--;
           m_Ofstream.write(ptr,sizeof(char));
           ptr+=3;
          }
        for (i = 0; i < paddedBytes; i++)
          {
          m_Ofstream.write(&paddingValue,sizeof(char));
          }
        }
      if (bpp == 4)
        {
        for (i = 0; i < m_Dimensions[0]; i++)
          {
           ptr+=2;
           m_Ofstream.write(ptr,sizeof(char));
           ptr--;
           m_Ofstream.write(ptr,sizeof(char));
           ptr--;
           m_Ofstream.write(ptr,sizeof(char));
           ptr+=4;
          }
        for (i = 0; i < paddedBytes; i++)
          {
          m_Ofstream.write(&paddingValue,sizeof(char));
          }
        }
      }
}

/** Print Self Method */
void BMPImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Depth " << m_Depth << "\n";
  os << indent << "FileLowerLeft " << m_FileLowerLeft << "\n";
  os << indent << "BitMapOffset " << m_BitMapOffset << "\n";
  if(m_Allow8BitBMP)
    {
    os << indent << "m_Allow8BitBMP : True" << "\n";
    }
  else
    {
    os << indent << "m_Allow8BitBMP : False" << "\n";
    }
}



} // end namespace itk
