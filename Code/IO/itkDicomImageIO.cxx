
/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDicomImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$  

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkDicomImageIO.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include <iostream>
#include <list>

#include <math.h>
#include <stdio.h>
#include <string.h>

// Convert a subtag into an unsigned int.
static inline unsigned int
BytePairToUnsigned(unsigned char* bytePair)
{ return ((unsigned int) (bytePair[1]) << 8)  + bytePair[0]; }

namespace itk
{

/** Constructor */
DicomImageIO::DicomImageIO()
{
  this->SetNumberOfDimensions(2);
  m_PixelType  = UCHAR;
  m_ByteOrder = LittleEndian;
}


/** Destructor */
DicomImageIO::~DicomImageIO()
{
}


/** This function returns true if the given tag (char *) is equal
 *  to (first subtag + second subtag)
 *  else return false */
bool DicomImageIO
::IfEqual(unsigned char * tag,int tagvalue1,int tagvalue2) const
{
  return (BytePairToUnsigned(&tag[0]) == tagvalue1 &&
          BytePairToUnsigned(&tag[2]) == tagvalue2);
}

/** This function puts the cursor of inputStream on the first byte
 *  after the subtag of the begining of the data pixels. i is the number
 *  of bytes already read in the stream.  Return true if the subtag is
 *  found, false otherwise.  */
bool DicomImageIO::GoToTheEndOfHeader(std::ifstream & inputStream,
                                      long int& i,Tag & tagcurrent) const
{
  unsigned char current[4]; 
  bool test;
  bool again;
  unsigned char c; 
  //take the first tag
  test=false;
  if(!inputStream.eof())
  {
    inputStream >> c;
    current[0]=c;
    if(!inputStream.eof())
    {
      inputStream >> c;
      current[1]=c;
      if(!inputStream.eof())
      {
        inputStream >> c;
        current[2]=c;
        if(!inputStream.eof())
        {
          inputStream >> c;
          current[3]=c;
          test=true;
        }
      }
    }
  }
  i=i+4;
  if (test==false)
  {
    return false;
  }
  again=true;
  while(again)
  {
    // (0x7EF0, 0x0010): Tag before data pixel
    if (!DicomImageIO::IfEqual(current,0x7FE0,0x0010))
    {
      //continue
      current[0]=current[1];
      current[1]=current[2];
      current[2]=current[3];
      if(!inputStream.eof())
      {
        inputStream >> c;
        current[3]=c;
        i++;
      }
      else
      {
        return (false);
      }
    }
    else
    {   
      tagcurrent.Subtag1[0]=current[0];
      tagcurrent.Subtag1[1]=current[1];
      tagcurrent.Subtag2[0]=current[2];
      tagcurrent.Subtag2[1]=current[3];
      tagcurrent.count=i;
      again=false;
    } 
  }
  return (true);
}

/** This function puts the cursor of the given stream on the first byte
 *  after the tag (subtagref1, subtagref2). i is the number of bytes
 *  already read in the Stream.  Return true if tag is found, false
 *  otherwise.
 */
bool DicomImageIO::GoToTag(std::ifstream & inputStream,int subtagref1,
                           int subtagref2,long int& i,long int& max,
                           Tag & tagcurrent) const
{
  unsigned char current[4]; 
  bool test;
  bool again;
  unsigned char c; 
  //take the first tag
  test=false;
  if(!inputStream.eof())
  {
    inputStream >> c;
    current[0]=c;
    if(!inputStream.eof())
    {
      inputStream >> c;
      current[1]=c;
      if(!inputStream.eof())
      {
        inputStream >> c;
        current[2]=c;
        if(!inputStream.eof())
        {
          inputStream >> c;
          current[3]=c;
          test=true;
        }  
      }
    }
  }
  i=i+4;
  if (test==false)
  {
    return false;
  }
  again=true;
  while(again)
  {
    unsigned int pos = inputStream.tellg();
    //0xAAAA and 0xBBBB >> Subtag1 And Subtag2 
    if (!DicomImageIO::IfEqual(current,subtagref1,subtagref2))
    {
      //continue
      current[0]=current[1];
      current[1]=current[2];
      current[2]=current[3];
      // if(i>=max) //if the program is reading outside the header
      if (false)
      {
        return (false);
      }
      else  
      {
        inputStream >> c;
        if (inputStream.eof()) 
        {
          return false;
        }
        current[3]=c;
        i++;
      }
    }
    else
    {   
      tagcurrent.Subtag1[0]=current[0];
      tagcurrent.Subtag1[1]=current[1];
      tagcurrent.Subtag2[0]=current[2];
      tagcurrent.Subtag2[1]=current[3];
      tagcurrent.count=i;
      again=false;
    } 
  }
  return (true);
}

/** This function tries to build a list of Tags for all required subtags
 *  return true >> all subtags are found 
 *  return false >> else
 */
bool DicomImageIO::
CheckTagTable(std::ifstream& inputStream, std::list<Tag>& TableOfTags) const
{
  long int i;
  long int max;
  Tag tagcurrent;
  i=0;
  //0x7FE0 and 0x0010 is the last tag before data pixel
  if(!DicomImageIO::GoToTheEndOfHeader(inputStream,i,tagcurrent))
  {
    return (false);
  }
  else
  {
    TableOfTags.push_back(tagcurrent);
    max=i;
    //start the reading of the Stream from the begining
    inputStream.seekg(0);
    i=0;
  }

#if 0

  ////0x0020 and 0x0032 >> tag before patient position
  if(!DicomImageIO::GoToTag(inputStream,0x0020,0x0032,i,max,tagcurrent))
  {
    return false;
  }
  else
  {
    std::cout << "Tagcurrent: ";
    TableOfTags.push_back(tagcurrent);
  }

#endif

  ////0x0028 and 0x0010 >> tag before the number of rows 
  if(!DicomImageIO::GoToTag(inputStream,0x0028,0x0010,i,max,tagcurrent))
  {
    return false;
  }
  else
  {
    TableOfTags.push_back(tagcurrent);
  }
  
  //0x0028 and 0x0011 >> tag before the number of columns
  if(!DicomImageIO::GoToTag(inputStream,0x0028,0x0011,i,max,tagcurrent))
  {
    return false;
  }
  else
  {
    TableOfTags.push_back(tagcurrent);
  }

  //0x0028 and 0x0030 >> tag before each value of spacing
  if(!DicomImageIO::GoToTag(inputStream,0x0028,0x0030,i,max,tagcurrent))
  {
    //if there is no information about the spacing, we set 'count' to 0
    tagcurrent.Subtag1[1]=0x28;
    tagcurrent.Subtag1[0]=0x00;
    tagcurrent.Subtag2[1]=0x30;
    tagcurrent.Subtag2[0]=0x00;
    tagcurrent.count=0;  //
    TableOfTags.push_back(tagcurrent);
    //start the reading of the Stream from the begining
    inputStream.seekg(0);
    i=0;
  }
  else
  {
    TableOfTags.push_back(tagcurrent);
  }

  //0x0028 and 0x0100 >> tag before the number of bits allocated
  if(!DicomImageIO::GoToTag(inputStream,0x0028,0x0100,i,max,tagcurrent))
  {
    return false;
  }
  else
  {
    TableOfTags.push_back(tagcurrent);
  }
  
  //0x0028 and 0x0103 >> tag before the place of sample representation
  if(!DicomImageIO::GoToTag(inputStream,0x0028,0x0103,i,max,tagcurrent))
  {
    return false;
  }
  else
  {
    TableOfTags.push_back(tagcurrent);
  }

  return(true);

}

bool DicomImageIO::CanReadFile( const char* filename ) 
{ 

  std::ifstream inputStream;
  std::list <Tag> TableOfTags;
  inputStream.open( filename, std::ios::in | std::ios::binary );
  if( inputStream.fail() )
  {
    return false;
  }
  if(DicomImageIO::CheckTagTable(inputStream,TableOfTags))
  {
    if(TableOfTags.size()==6)
    {
      return true;
    }
  }
  return false;
}
  


bool DicomImageIO::CanWriteFile(const char*)
{
  //not possible to write a dicom file
  return false;
}



const std::type_info& DicomImageIO::GetPixelType() const
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

unsigned int DicomImageIO::GetComponentSize() const
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
 
 
void DicomImageIO::Read(void* buffer)
{
  unsigned int dimensions = this->GetNumberOfDimensions();
  unsigned int numberOfPixels = 1;
  for( unsigned int dim=0; dim< dimensions; dim++ )
    {
    numberOfPixels *= m_Dimensions[ dim ];
    }

  std::ifstream inFile;
  
  inFile.open(m_FileName.c_str(), std::ios::in | std::ios::binary );
  if( !inFile )
  {
    ExceptionObject exception(__FILE__, __LINE__);
    std::string msg = "File \"" + m_FileName + "\" cannot be read.";
    exception.SetDescription(msg.c_str());
    throw exception;
  }

  inFile.seekg(m_InputPosition);
  if( !inFile )
  {
    ExceptionObject exception(__FILE__, __LINE__);
    itk::OStringStream msg;
    msg << "Cannot seek to position " << m_InputPosition
        << " in file \"" << m_FileName.c_str() << "\".";
    exception.SetDescription(msg.str().c_str());
    throw exception;
  }

  char * p = static_cast<char *>(buffer);
  inFile.read( p, this->GetImageSizeInBytes() );
  bool success = !inFile.bad();
  inFile.close();
  if( !success )
  {
    itkExceptionMacro("Error reading image data.");
  }

  SwapBytesIfNecessary( buffer, numberOfPixels );
}

/** 
 *  Read Information about the dicom file
 *  and put the cursor of the stream just before the first data pixel
 */
void DicomImageIO::ReadImageInformation()
{
  long int i,j,max;
  long int len;
  double temp;
  unsigned char bytePair[2];
  unsigned char c;
  unsigned int rows;
  unsigned int columns;
  unsigned int allocatedbits;
  unsigned int representation;
  char chain [4];
  char * value;
  char * spac1value,* spac2value;
  Tag tagcurrent;

  std::ifstream inFile;
  inFile.open(m_FileName.c_str(), std::ios::in | std::ios::binary );
  if( !inFile )
  {
    ExceptionObject exception(__FILE__, __LINE__);
    std::string msg = "File \"" + m_FileName + "\" cannot be read.";
    exception.SetDescription(msg.c_str());
    throw exception;
  }

  this->SetNumberOfDimensions( 2 );
  i=0;
  //0x7FE0 and 0x0010 is the last tag before data pixel
  if(DicomImageIO::GoToTheEndOfHeader(inFile,i,tagcurrent))
  {
    max=i;
    //start the reading of the Stream from the begining
    inFile.seekg(0);
    i=0;
  }

#if 0

  ////0x0020 and 0x0032 >> tag before value of Origin
  m_Origin.clear();
  m_Origin.insert(m_Origin.end(), 3, 0.0);  // Default for origin.
  if(DicomImageIO::GoToTag(inFile, 0x0020, 0x0032, i, max, tagcurrent))
  {
    for(i=0;i<4;i++)
    {
      inFile >> chain[i];
    }
    if((chain[0]=='D') && (chain[1]=='S'))
    {
      // The VR (Value Representation) is explicitly "DS"
      if (chain[2]==0 && chain[3] !=0 )
      {
        // But the length is not specified 
        len=0; 
        c=chain[3];
        i=inFile.tellg();
        while(c>44)
        {
          len++;
          inFile >> c;
        }
        //we put the cursor back to read data spacing
        inFile.seekg(i-len+8);
      }
      else
      {
        // The length is specified using 2 bytes.
        len=chain[2]+chain[3]*256;
      }
    }
    else
    {
      // The VR is not specified and the length is, using 4 bytes.
      len=chain[0]+chain[1]*256+chain[2]*256*256+chain[3]*256*256*256;
    }

    if (len > 0) {
      char dummy;
      inFile
        >> m_Origin[0] >> dummy
        >> m_Origin[1] >> dummy
        >> m_Origin[2];
    }
  }
  else
  {
    // If tag not found then go back to beginning of file to look for
    // next tag.  (We aren't using the fact that tags are required to
    // be in order.)
    inFile.seekg(0);
    i=0;
  }

#endif // 0

  ////0x0028 and 0x0010 >> tag before the number of rows 
  if(DicomImageIO::GoToTag(inFile, 0x0028, 0x0010, i, max, tagcurrent))
  { 
    for(i=0;i<4;i++)
    {
      inFile >> chain [i];
    }
    if(strcmp("US20",chain)||strcmp("2000",chain))
    {
      inFile >> bytePair[0] >> bytePair[1];
      m_Dimensions[1] = BytePairToUnsigned(bytePair);
    }
    else
    {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("error_dicom_rows");
      throw exception;  
    }
  }
  else
  {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("error_dicom_rows");
    throw exception;  
  }

  ////0x0028 and 0x0011 >> tag before the number of columns 
  if(DicomImageIO::GoToTag(inFile,0x0028,0x0011,
                           i,max,tagcurrent))
   {
    for(i=0;i<4;i++)
    {
      inFile >> chain [i];
    }
    if(strcmp("US20",chain)||strcmp("2000",chain))
    {
      inFile >> bytePair[0] >> bytePair[1];
      m_Dimensions[0] = BytePairToUnsigned(bytePair);
    }
    else
    {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("error_dicom_columns");
      throw exception;  
    }
  }
  else
  {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("error_dicom_columns");
    throw exception;  
  }

  ////0x0028 and 0x0030 >> tag before value of spacing
  m_Spacing[0]=1;  // Default values
  m_Spacing[1]=1;
  if(DicomImageIO::GoToTag(inFile, 0x0028, 0x0030, i, max, tagcurrent))
  {
    for(i=0;i<4;i++)
    {
      inFile >> chain[i];
    }
    if((chain[0]=='D') && (chain[1]=='S'))
    {
      // The VR (Value Representation) is explicitly "DS"
      if (chain[2]==0 && chain[3] !=0 )
      {
        // But the length is not specified 
        len=0; 
        c=chain[3];
        i=inFile.tellg();
        while(c>44)
        {
          len++;
          inFile >> c;
        }
        //we put the cursor back to read data spacing
        inFile.seekg(i-len+8);
      }
      else
      {
        // The length is specified using 2 bytes.
        len=chain[2]+chain[3]*256;
      }
    }
    else
    {
      // The VR is not specified and the length is, using 4 bytes.
      len=chain[0]+chain[1]*256+chain[2]*256*256+chain[3]*256*256*256;
    }
    if (len > 0) {
      char dummy;
      inFile >> m_Spacing[0] >> dummy >> m_Spacing[1];
    }
  }
  else
  {
    // If tag not found then go back to beginning of file to look for
    // next tag.  (We aren't using the fact that tags are required to
    // be in order.)
    inFile.seekg(0);
    i=0;
  }

  ////0x0028 and 0x0100 >> tag before the number of allocated bits
  if(DicomImageIO::GoToTag(inFile,0x0028,0x0100,i,max,tagcurrent))
  {
    for(i=0;i<4;i++)
    {
      inFile >> chain [i];
    }
    if(strcmp("US20",chain)||strcmp("2000",chain))
    {
      inFile >> bytePair[0] >> bytePair[1];
      allocatedbits = BytePairToUnsigned(bytePair);
    }
    else
    {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("error_dicom_allocated_bits");
      throw exception;  
    }
  }
  else
  {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("error_dicom_allocated_bits");
    throw exception;  
  }

  ////0x0028 and 0x0103 >> tag before the number of pixel representation
  if(DicomImageIO::GoToTag(inFile,0x0028,0x0103,i,max,tagcurrent))
  {
    for(i=0;i<4;i++)
    {
      inFile >> chain [i];
    }
    if(strcmp("US20",chain)||strcmp("2000",chain))
    {
      inFile >> bytePair[0] >> bytePair[1];
      representation = BytePairToUnsigned(bytePair);
    }
    else
    {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("error_dicom_representation_bits");
      throw exception;  
    }
  }
  else
  {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("error_dicom_representation_bits");
    throw exception;  
  }

  m_ByteOrder=LittleEndian; 

  if ( allocatedbits == 8 && representation == 0)
  {      
    m_ComponentType = UCHAR;
    m_PixelType = UCHAR;
  }
  else
  {
    if( allocatedbits == 8 && representation == 1)
    {
      m_ComponentType = CHAR;
      m_PixelType = CHAR;
    }
    else
    {
      if( allocatedbits == 16 && representation == 0)
      {
        m_ComponentType = USHORT;
        m_PixelType = USHORT;
      }
      else
      {
        if( allocatedbits == 16 && representation == 1)
        {
          m_ComponentType = SHORT;
          m_PixelType = SHORT; 
        }
        else
        {
          ExceptionObject exception(__FILE__, __LINE__);
          exception.SetDescription("error_dicom_PixelType_ComponentType)");
          throw exception;  
        }
      }  
    }
  }
  //then go to the end of the header, at the begining of the data
  if(!DicomImageIO::GoToTag(inFile,0x7FE0,0x0010,i,max,tagcurrent))
  {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("error_dicom_no_end_of_the_header");
    throw exception;  
  }
  inFile >> chain[0];
  inFile >> chain[1];
  if (chain[0]=='O' && chain[1]=='W')
  {
    j=4;
  }
  else
  {
    j=2;
  }
  for(i=0;i<j;i++)
  {
    inFile >> c;
  }
  
  m_InputPosition = inFile.tellg();
  itk::Indent* in = itk::Indent::New();
  this->PrintSelf(std::cout, *in);
}


void 
DicomImageIO
::SwapBytesIfNecessary( void* buffer, unsigned long numberOfPixels )
{
  switch(m_PixelType)
    {
  case CHAR:
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
    case UCHAR:
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


/** The write function is not implemented */
void 
DicomImageIO
::WriteImageInformation(void)
{
  //not possible to write a dicom file  
}

/** The write function is not implemented */
void 
DicomImageIO
::Write( const void*) 
{
  //not possible to write a dicom file
}

/** Print Self Method */
void DicomImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Spacing: ( ";
  for (unsigned int i=0; i < m_NumberOfDimensions; i++)
  {
    os << m_Spacing[i] << " ";
  }
  os << " )\n";
  os << indent << "Origin: ( ";
  for (unsigned int i=0; i < m_Origin.size(); i++)
  {
    os << m_Origin[i] << " ";
  }
  os << " )" << std::endl;
  
}



} // end namespace itk
