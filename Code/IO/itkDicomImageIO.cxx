
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
#include <string.h>
#include <math.h>

// Note: Balise is a French word used in this case to refer to half of
// a tag.

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


/** This function return true if the given tag (char *) is equal
 *  to (first balise + the second balise)
 *  else return false */
bool DicomImageIO::IfEqual(unsigned char * tag,int tagvalue1,int tagvalue2) const
{
  if((tag[0]+tag[1]*256==tagvalue1) && (tag[2]+tag[3]*256==tagvalue2))
  {
    return (true);
  }
  else
  {
    return (false);
  }
}

/** This function put the cursor of the given Stream on the first byte
 *  after the balise of the begining of the Datapixel. i is the number of bytes allready red
 *  in the Stream. return true >> balise found
 *                 return flase >> else */
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
    //0x7EF0 and 0x0010 >> Balise before data pixel
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
      tagcurrent.Balise1[0]=current[0];
      tagcurrent.Balise1[1]=current[1];
      tagcurrent.Balise2[0]=current[2];
      tagcurrent.Balise2[1]=current[3];
      tagcurrent.compt=i;
      again=false;
    } 
  }
  return (true);
}

/** This function put the cursor of the given stream on the first byte
 *  after the tag (baliseref1, baliseref2). i is the number of bytes
 *  already read in the Stream.  Return true if tag is found, false
 *  otherwise.
 */
bool DicomImageIO::GoToTag(std::ifstream & inputStream,int baliseref1,
                           int baliseref2,long int& i,long int& max,
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
    //0xAAAA and 0xBBBB >> Balise1 And Balise2 
    if (!DicomImageIO::IfEqual(current,baliseref1,baliseref2))
    {
      //continue
      current[0]=current[1];
      current[1]=current[2];
      current[2]=current[3];
      if(i>=max) //if the program is reading outside the header
      {
        return (false);
      }
      else  
      {
        inputStream >> c;
        current[3]=c;
        i++;
      }
    }
    else
    {   
      tagcurrent.Balise1[0]=current[0];
      tagcurrent.Balise1[1]=current[1];
      tagcurrent.Balise2[0]=current[2];
      tagcurrent.Balise2[1]=current[3];
      tagcurrent.compt=i;
      again=false;
    } 
  }
  return (true);
}

/** This function try to build a list of Tags for all required balises
 *  return true >> all balises are found 
 *  return flase >> else
 */
bool DicomImageIO::CheckTagTable(std::ifstream & inputStream,std::list <Tag> &TableOfTags) const
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

  //0x0028 and 0x0030 >> tag before each the value of spacing
  if(!DicomImageIO::GoToTag(inputStream,0x0028,0x0030,i,max,tagcurrent))
  {
    //if there is no information about the spacing, we set the value compt to 0
    tagcurrent.Balise1[1]=0x28;
    tagcurrent.Balise1[0]=0x00;
    tagcurrent.Balise2[1]=0x30;
    tagcurrent.Balise2[0]=0x0;
    tagcurrent.compt=0;  //
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
  unsigned char c;
  unsigned int rows;
  unsigned int columns;
  unsigned int allocatedbits;
  unsigned int representation;
  char chaine [4];
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

  ////0x0028 and 0x0010 >> tag before the number of rows 
  if(DicomImageIO::GoToTag(inFile,0x0028,0x0010,
                                             i,max,tagcurrent))
  { 
    for(i=0;i<4;i++)
    {
      inFile >> chaine [i];
    }
    if(strcmp("US20",chaine)||strcmp("2000",chaine))
    {
      rows=0;
      for(i=0;i<2;i++)
      {
        inFile >> c;
        // rows=rows+c*(unsigned int)pow((double)256,(double)i);
        rows += c << (i * 8);
      }
      m_Dimensions[1]=rows;
      std::cout << m_Dimensions[1] << std::endl;
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
      inFile >> chaine [i];
    }
    if(strcmp("US20",chaine)||strcmp("2000",chaine))
    {
      columns=0;
      for(i=0;i<2;i++)
      {
        inFile >> c;
        // columns=columns+c*(unsigned int)pow((double)256,(double)i);
        columns += c << (i * 8);
      }
      m_Dimensions[0]=columns;
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

  ////0x0028 and 0x0030 >> tag before value of spacing
  if(DicomImageIO::GoToTag( inFile,0x0028,0x0030,
                                             i,max,tagcurrent))
  {
    for(i=0;i<4;i++)
    {
      inFile >> chaine [i];
    }
    if((chaine[0]=='D') && (chaine[1]=='S'))
    {
      //if the length is not specified 
      if (chaine[2]==0 && chaine[3] !=0 )
      {
       len=0; 
       c=chaine[3];
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
       //if the length is specified on 2 bytes
      len=chaine[2]+chaine[3]*256;
      }
    }
    else
    {
      //if the length is specified on 4 bytes
      len=chaine[0]+chaine[1]*256+chaine[2]*256*256+chaine[3]*256*256*256;
    }
    // we build a table of char 
    value = (char *) malloc (sizeof(char)*len);
    j = 0;
    for(i=0;i<len;i++)
    {
      inFile >> c;
      value[i]=c;
      if(value[i]=='\\')
      {
        j=i;
      }
    }
    spac1value=(char*)malloc(sizeof(char)*(j));
    spac2value=(char*)malloc(sizeof(char)*(len-j-1));
    for(i=0;i<len;i++)
    {
      if(i<j)
      {
        //we put data in spac1value[]
        spac1value[i]=value[i];
      }
      if(i>j)
      {
        //we put data in spac2value[]
        if(i<len-1)
        {
          spac2value[i-j-1]=value[i];
        }
        if(i==len-1) // if the value of the length is = n or = n-1  ???
        {
          if(value[i]>=44)  // 44 is '-' / if lower it can be a tag detected
          {
            spac2value[i-j-1]=value[i];
          }
          else
          {
            //one byte was red too many
            i=inFile.tellg();
            inFile.seekg(--i);
          }
        }
      }
    }
    temp=atof(spac1value);
    m_Spacing[0]=temp;
    temp=atof(spac2value);
    m_Spacing[1]=temp;
  }
  else
  {
    //if no data about spacing exits >> then set the value to 1
    m_Spacing[0]=1;
    m_Spacing[1]=1;
    inFile.seekg(0);
    i=0;
  }
  ////0x0028 and 0x0100 >> tag before the number of allocated bits
  if(DicomImageIO::GoToTag(inFile,0x0028,0x0100,i,max,tagcurrent))
  {
    for(i=0;i<4;i++)
    {
      inFile >> chaine [i];
    }
    if(strcmp("US20",chaine)||strcmp("2000",chaine))
    {
      allocatedbits=0;
      for(i=0;i<2;i++)
      {
        inFile >> c;
        allocatedbits=allocatedbits+c*(unsigned int)pow((double)256,(double)i);
      }
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
      inFile >> chaine [i];
    }
    if(strcmp("US20",chaine)||strcmp("2000",chaine))
    {
      representation=0;
      for(i=0;i<2;i++)
      {
        inFile >> c;
        representation=representation+c*(unsigned int)pow((double)256,(double)i);
      }
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
  inFile >> chaine[0];
  inFile >> chaine[1];
  if (chaine[0]=='O' && chaine[1]=='W')
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
  os << indent << "PixelType " << m_PixelType << "\n";
}



} // end namespace itk
