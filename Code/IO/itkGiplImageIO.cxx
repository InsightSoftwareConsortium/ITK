/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGiplImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$  

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkGiplImageIO.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include <iostream>
#include <list>
#include <string>
#include <math.h>

namespace itk
{

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

/** Constructor */
GiplImageIO::GiplImageIO()
{
  m_ByteOrder = BigEndian;
}


/** Destructor */
GiplImageIO::~GiplImageIO()
{
  m_Ifstream.close();
}


bool GiplImageIO::CanReadFile( const char* filename ) 
{ 
  std::ifstream inputStream;
  inputStream.open( filename, std::ios::in | std::ios::binary );
  if( inputStream.fail() )
  {
    return false;
  }
  
  inputStream.seekg(252);
  unsigned int magic_number;
  inputStream.read((char*)&magic_number,sizeof(unsigned int));
  
  if(m_ByteOrder == BigEndian)
  {
    ByteSwapper<unsigned int>::SwapFromSystemToBigEndian(&magic_number);
  }
  else if (m_ByteOrder == LittleEndian)
  {
    ByteSwapper<unsigned int>::SwapFromSystemToLittleEndian(&magic_number);
  }

  if(magic_number == GIPL_MAGIC_NUMBER)
  {
     inputStream.close();
     return true;
  }
  
  inputStream.close();
  return false;
}
  


bool GiplImageIO::CanWriteFile(const char*)
{
  return true;
}


const std::type_info& GiplImageIO::GetPixelType() const
{
  switch(m_PixelType)
    {
    case SHORT:
      return typeid(short);
    case USHORT:
      return typeid(unsigned short);
    case CHAR:
      return typeid(char);
    case UCHAR:
      return typeid(unsigned char);
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Pixel Type Unknown");
      throw exception;
    }
}

unsigned int GiplImageIO::GetComponentSize() const
{
  switch(m_PixelType)
    {
    case SHORT:
      return sizeof(short);
    case USHORT:
      return sizeof(unsigned short);
    case CHAR:
      return sizeof(char);
    case UCHAR:
      return sizeof(unsigned char);
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Pixel Type Unknown");
      throw exception;
    }
  return 1;
}
 
 
void GiplImageIO::Read(void* buffer)
{
  unsigned int dimensions = this->GetNumberOfDimensions();
  unsigned int numberOfPixels = 1;
  for( unsigned int dim=0; dim< dimensions; dim++ )
    {
    numberOfPixels *= m_Dimensions[ dim ];
    }

  char * p = static_cast<char *>(buffer);
  m_Ifstream.read( p, this->GetImageSizeInBytes() );
  bool success = !m_Ifstream.bad();
  m_Ifstream.close();
  if( !success )
    {
    itkExceptionMacro("Error reading image data.");
    }

  SwapBytesIfNecessary( buffer, numberOfPixels );
}

/** 
 *  Read Information about the Gipl file
 *  and put the cursor of the stream just before the first data pixel
 */
void GiplImageIO::ReadImageInformation()
{
  unsigned int i;

  m_Ifstream.open(m_FileName.c_str(), std::ios::in | std::ios::binary );
  if( m_Ifstream.fail() )
  {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("File cannot be read");
    throw exception;
  }

  unsigned short dims[4];

  unsigned int numberofdimension = 0;
  for(i=0;i<4;i++)
  {
    dims[i]=0;
  }

  for(i=0;i<4;i++)
  {
    
    m_Ifstream.read((char*)&dims[i],sizeof(unsigned short));
    if(m_ByteOrder == BigEndian)
    {
      ByteSwapper<unsigned short>::SwapFromSystemToBigEndian(&dims[i]);
    }
    else if (m_ByteOrder == LittleEndian)
    {
      ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&dims[i]);
    }

    
    if( dims[i] > 0 )
    {
      if (i<3)
      {
        numberofdimension++; 
      }
      else if( dims[i] > 1)
      {
        numberofdimension++; 
      }
    }
    else
    {
      break;
    }
  }

  this->SetNumberOfDimensions( numberofdimension );

  for(i=0;i<numberofdimension;i++)
  {
    m_Dimensions[i]=dims[i];
  }

  unsigned short   image_type;
  m_Ifstream.read((char*)&image_type,sizeof(unsigned short));


  if(m_ByteOrder == BigEndian)
  {
    ByteSwapper<unsigned short>::SwapFromSystemToBigEndian(&image_type);
  }


  switch(image_type)
  {
    case  GIPL_BINARY : m_ComponentType = UCHAR; m_PixelType = UCHAR;break;
    case  GIPL_CHAR :  m_ComponentType = CHAR; m_PixelType = CHAR;break;
    case  GIPL_U_CHAR :  m_ComponentType = UCHAR; m_PixelType = UCHAR;break;
    case  GIPL_SHORT :  m_ComponentType = SHORT; m_PixelType = SHORT;break;
    case  GIPL_U_SHORT :  m_ComponentType = USHORT; m_PixelType = USHORT;break;
    case  GIPL_U_INT :  m_ComponentType = UINT; m_PixelType = UINT;break;
    case  GIPL_INT :  m_ComponentType = INT; m_PixelType = INT;break;
    case  GIPL_FLOAT :  m_ComponentType = FLOAT; m_PixelType = FLOAT;break;
    case  GIPL_DOUBLE :  m_ComponentType = DOUBLE; m_PixelType = DOUBLE;break;
  }


  float   pixdim[4];         /*   10   16  X,Y,Z,T pixel dimensions mm */
  for(i=0;i<4;i++)
  {
    m_Ifstream.read((char*)&pixdim[i],sizeof(float));
    if(m_ByteOrder == BigEndian)
    {
      ByteSwapper<float>::SwapFromSystemToBigEndian(&pixdim[i]);
    }
    else if (m_ByteOrder == LittleEndian)
    {
      ByteSwapper<float>::SwapFromSystemToLittleEndian(&pixdim[i]);
    }
    
    if(i<numberofdimension)
    {
      m_Spacing[i]=pixdim[i];
    }    
  }
      
  char    line1[80];         /*   26   80  Patient / Text field        */
  for(i=0;i<80;i++)
  {
    m_Ifstream.read((char*)&line1[i],sizeof(char));
  }

  
  float   matrix[20];        /*  106   80                              */ 
  for(i=0;i<20;i++)
  {
    m_Ifstream.read((char*)&matrix[i],sizeof(float));
    
    if(m_ByteOrder == BigEndian)
    {
      ByteSwapper<float>::SwapFromSystemToBigEndian(&matrix[i]);
    }
    else if (m_ByteOrder == LittleEndian)
    {
      ByteSwapper<float>::SwapFromSystemToLittleEndian(&matrix[i]);
    } 
  }

  char    flag1;             /*  186    1  Orientation flag (below)    */
  m_Ifstream.read((char*)&flag1,sizeof(char));
  if(m_ByteOrder == BigEndian)
  {
    ByteSwapper<char>::SwapFromSystemToBigEndian(&flag1);
  }
  else if (m_ByteOrder == LittleEndian)
  {
    ByteSwapper<char>::SwapFromSystemToLittleEndian(&flag1);
  }
  
  char    flag2;             /*  187    1                              */
  m_Ifstream.read((char*)&flag2,sizeof(char));

  
  if(m_ByteOrder == BigEndian)
  {
    ByteSwapper<char>::SwapFromSystemToBigEndian(&flag2); 
  }
  else if (m_ByteOrder == LittleEndian)
  {
    ByteSwapper<char>::SwapFromSystemToLittleEndian(&flag2); 
  }
 
  double  min;               /*  188    8  Minimum voxel value         */
  m_Ifstream.read((char*)&min,sizeof(double));

  double  max;               /*  196    8  Maximum voxel value         */
  m_Ifstream.read((char*)&max,sizeof(double));
 
  double  origin[4];         /*  204   32  X,Y,Z,T offset              */
  for(i=0;i<4;i++)
  {
    m_Ifstream.read((char*)&origin[i],sizeof(double));
  }

  float   pixval_offset;     /*  236    4                              */
  m_Ifstream.read((char*)&pixval_offset,sizeof(float));

  if(m_ByteOrder == BigEndian)
  {  
    ByteSwapper<float>::SwapFromSystemToBigEndian(&pixval_offset);
  }
  else if (m_ByteOrder == LittleEndian)
  {   
    ByteSwapper<float>::SwapFromSystemToLittleEndian(&pixval_offset); 
  }

  float   pixval_cal;        /*  240    4                              */
  m_Ifstream.read((char*)&pixval_cal,sizeof(float));
    
  if(m_ByteOrder == BigEndian)
  {  
    ByteSwapper<float>::SwapFromSystemToBigEndian(&pixval_cal);
  }
  else if (m_ByteOrder == LittleEndian)
  {   
    ByteSwapper<float>::SwapFromSystemToLittleEndian(&pixval_cal);
  }

  float   user_def1;         /*  244    4  Inter-slice Gap             */
  m_Ifstream.read((char*)&user_def1,sizeof(float));
  

  if(m_ByteOrder == BigEndian)
  {  
    ByteSwapper<float>::SwapFromSystemToBigEndian(&user_def1);
  }
  else if (m_ByteOrder == LittleEndian)
  {   
    ByteSwapper<float>::SwapFromSystemToLittleEndian(&user_def1); 
  }

       
  float   user_def2;         /*  248    4  User defined field          */
  m_Ifstream.read((char*)&user_def2,sizeof(float));
  

  if(m_ByteOrder == BigEndian)
  {  
    ByteSwapper<float>::SwapFromSystemToBigEndian(&user_def2);
  }
  else if (m_ByteOrder == LittleEndian)
  {   
    ByteSwapper<float>::SwapFromSystemToLittleEndian(&user_def2); 
  }
     
  unsigned int magic_number; /*  252    4 Magic Number                 */
  m_Ifstream.read((char*)&magic_number,sizeof(unsigned int));
  

  if(m_ByteOrder == BigEndian)
  {  
    ByteSwapper<unsigned int>::SwapFromSystemToBigEndian(&magic_number);
  }
  else if (m_ByteOrder == LittleEndian)
  {   
    ByteSwapper<unsigned int>::SwapFromSystemToLittleEndian(&magic_number); 
  }
  
}


void 
GiplImageIO
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
::Write( const void* buffer) 
{
  unsigned int nDims = this->GetNumberOfDimensions();

  m_Ofstream.open(m_FileName.c_str(), std::ios::binary | std::ios::out);
  if( m_Ofstream.fail() )
  {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("File cannot be write");
    throw exception;
  }

 

  unsigned int i;
  for(i=0;i<4;i++)
  {
    unsigned short value;
    if(i<nDims)
    {
      value = this->GetDimensions(i);
      if(m_ByteOrder == BigEndian)
      {
        ByteSwapper<unsigned short>::SwapFromSystemToBigEndian(&value);
      }
      else if (m_ByteOrder == LittleEndian)
      {
        ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&value);
      }
      m_Ofstream.write((char*)&(value),sizeof(unsigned short));
    }

    else
    {
      value = 0;
      if(m_ByteOrder == BigEndian)
      {
        ByteSwapper<unsigned short>::SwapFromSystemToBigEndian(&value);
      }
      else if (m_ByteOrder == LittleEndian)
      {
        ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&value);
      }
      m_Ofstream.write((char*)&value,sizeof(unsigned short));;
    }
  }


  unsigned short   image_type;
  switch(m_PixelType)
  {
    case  CHAR : image_type = GIPL_CHAR;break;
    case  UCHAR :  image_type = GIPL_U_CHAR;break;
    case  SHORT :  image_type = GIPL_SHORT;break;
    case  USHORT :  image_type = GIPL_U_SHORT;break;
    case  UINT :  image_type = GIPL_U_INT;break;
    case  INT :  image_type = GIPL_INT;break;
    case  FLOAT :  image_type = GIPL_FLOAT;break;
    case  DOUBLE : image_type = GIPL_DOUBLE;break;
    default:
      itkExceptionMacro ("Invalid type: " << m_PixelType );
  }

  if(m_ByteOrder == BigEndian)
  {
    ByteSwapper<unsigned short>::SwapFromSystemToBigEndian((unsigned short*)&image_type);
  }
  if(m_ByteOrder == LittleEndian)
  {
    ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian((unsigned short *)&image_type);
  }
 
  m_Ofstream.write((char*)&image_type,sizeof(unsigned short));

  /*   10   16  X,Y,Z,T pixel dimensions mm */
  
  for(i=0;i<4;i++)
  {
    if(i<nDims)
    {
      float value = m_Spacing[i];
      if(m_ByteOrder == BigEndian)
      {
        ByteSwapper<float>::SwapFromSystemToBigEndian((float *)&value);
      }
      if(m_ByteOrder == LittleEndian)
      {
        ByteSwapper<float>::SwapFromSystemToLittleEndian((float *)&value);
      }
      m_Ofstream.write((char*)&value,sizeof(float));
    }
    else
    {
      float value = 0;
      if(m_ByteOrder == BigEndian)
      {
        ByteSwapper<float>::SwapFromSystemToBigEndian((float*)&value);
      }
      if(m_ByteOrder == LittleEndian)
      {
        ByteSwapper<float>::SwapFromSystemToLittleEndian((float *)&value);
      }
      
      m_Ofstream.write((char*)&value,sizeof(float));;
    }

  }

  char    line1[80];         /*   26   80  Patient / Text field        */
  sprintf(line1,"No Patient Information");
  for(i=0;i<80;i++)
  {
    m_Ofstream.write((char*)&line1[i],sizeof(char));
  }

  float   matrix[20];        /*  106   80                              */
  for(i=0;i<20;i++)
  {
    m_Ofstream.write((char*)&matrix[i],sizeof(float));
  }

  char    flag1=0;             /*  186    1  Orientation flag (below)    */
  m_Ofstream.write((char*)&flag1,sizeof(char));
  

  char    flag2=0;             /*  187    1                              */
  m_Ofstream.write((char*)&flag2,sizeof(char));
   
  double  min=0;               /*  188    8  Minimum voxel value         */
  m_Ofstream.write((char*)&min,sizeof(double));

 
  double  max=0;               /*  196    8  Maximum voxel value         */
  m_Ofstream.write((char*)&max,sizeof(double));
 
        
  double  origin[4];         /*  204   32  X,Y,Z,T offset              */
  for(i=0;i<4;i++)
  {
    origin[i]=0;
    m_Ofstream.write((char*)&origin[i],sizeof(double));
  }

  float   pixval_offset=0;     /*  236    4                            */
  m_Ofstream.write((char*)&pixval_offset,sizeof(float));
  
  float   pixval_cal=0;        /*  240    4                              */
  m_Ofstream.write((char*)&pixval_cal,sizeof(float));
  
  float   user_def1=0;         /*  244    4  Inter-slice Gap             */
  m_Ofstream.write((char*)&user_def1,sizeof(float));
       
  float   user_def2=0;         /*  248    4  User defined field          */
  m_Ofstream.write((char*)&user_def2,sizeof(float));
     
  unsigned int magic_number = GIPL_MAGIC_NUMBER; /*  252    4 Magic Number                 */
  if(m_ByteOrder == BigEndian)
  {
    ByteSwapper<unsigned int>::SwapFromSystemToBigEndian(&magic_number);
  }
  if(m_ByteOrder == LittleEndian)
  {
    ByteSwapper<unsigned int>::SwapFromSystemToLittleEndian(&magic_number);
  }
  m_Ofstream.write((char*)&magic_number,sizeof(unsigned int));
  


  // Actually do the writing
  //
  this->ComputeStrides();
  if ( m_FileType == ASCII )
  {
    this->WriteBufferAsASCII(m_Ofstream, buffer, this->GetComponentType(),
                           this->GetImageSizeInComponents());
  }
  else //binary
  {
    const unsigned long numberOfBytes      = this->GetImageSizeInBytes();
    const unsigned long numberOfComponents = this->GetImageSizeInComponents();

    // Swap bytes if necessary
    if ( m_ByteOrder == LittleEndian )
    {
      char * tempBuffer = new char[ numberOfBytes ];
      memcpy( tempBuffer, buffer , numberOfBytes );
      SwapBytesIfNecessary(tempBuffer, numberOfComponents );
      m_Ofstream.write( tempBuffer, numberOfBytes );
      delete [] tempBuffer;
    }
    else if ( m_ByteOrder == BigEndian )
    {
      char * tempBuffer = new char[ numberOfBytes ];
      memcpy( tempBuffer, buffer , numberOfBytes );
      SwapBytesIfNecessary(tempBuffer, numberOfComponents );
      m_Ofstream.write( tempBuffer, numberOfBytes );
      delete [] tempBuffer;
    }
    else
    {
      m_Ofstream.write(static_cast<const char*>(buffer), numberOfBytes );
    }

  }

  m_Ofstream.close();
}

/** Print Self Method */
void GiplImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "PixelType " << m_PixelType << "\n";
}



} // end namespace itk
