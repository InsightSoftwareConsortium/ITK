/*=========================================================================
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBrains2MaskImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  This software is distributed WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the above copyright notices for more information.

  =========================================================================*/

#include "itkBrains2MaskImageIO.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkIOCommon.h"
#include "itkMetaDataObject.h"
#include <stdio.h>
#include <zlib.h>

static const unsigned char DEF_WHITE_MASK=255;
namespace itk
{
#define Brains2_MASKFILE_WHITE  0
#define Brains2_MASKFILE_BLACK  1
#define Brains2_MASKFILE_GRAY  2


template <class TPixel> 
class Brains2MaskMappingFunction {
public:
  unsigned int Evaluate(const TPixel *pixel) ;
};
template <class TPixel>
unsigned int
Brains2MaskMappingFunction<TPixel>::
Evaluate(const TPixel *pixel)
{
  return *pixel == 0 ? 0 : 1;
}
    

// Default constructor
Brains2MaskImageIO::Brains2MaskImageIO()
{
  //by default, only have 3 dimensions
  this->SetNumberOfDimensions(3);
  m_PixelType         = UCHAR;
  //The file byte order
  m_MachineByteOrder  = ( ByteSwapper<int>::SystemIsBigEndian() == true ) ? 
    LittleEndian : BigEndian ;
}

Brains2MaskImageIO::~Brains2MaskImageIO()
{
  //Purposefully left blank
}

void Brains2MaskImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "PixelType " << m_PixelType << "\n";
}

bool Brains2MaskImageIO::CanWriteFile(const char * FileNameToWrite)
{
  m_FileName=FileNameToWrite;
  if( ( m_FileName != "" ) &&
      ( m_FileName.find(".mask") < m_FileName.length() )  // Mask name Given
    )
    {
    return true;
    }
  return false;
}

const std::type_info& Brains2MaskImageIO::GetPixelType() const
{
  return typeid(unsigned char);
}

unsigned int Brains2MaskImageIO::GetComponentSize() const
{
  return sizeof(unsigned char);
}

//The function that is used to read the octree stream to an octree.
itk::OctreeNodeBranch * Brains2MaskImageIO::
readOctree (std::ifstream & octreestream,
            const ImageIOBase::ByteOrder machineByteOrder,
            const ImageIOBase::ByteOrder fileByteOrder)
{
  //Read in the color to set
  unsigned short int colorCode;
  octreestream.read((char *)&colorCode, sizeof (unsigned short int));
  if (machineByteOrder != fileByteOrder)
    {
    if ( machineByteOrder == LittleEndian )
      {
      ByteSwapper<unsigned short int>::
        SwapFromSystemToBigEndian( &colorCode );
      }
    else if ( machineByteOrder == BigEndian )
      {
      ByteSwapper<unsigned short int>::
        SwapFromSystemToLittleEndian( &colorCode );
      }
    }
  //Create child array of nodes.
  itk::OctreeNodeBranch *CurrentNodeBranch = new itk::OctreeNodeBranch(m_Octree);
  //7766554433221100  ChildID
  //1111110000000000  Bit
  //5432109876543210  Numbers
  for (unsigned int i = ZERO; i <= SEVEN; i++)
    {
    OctreeNode *curnode = 
      CurrentNodeBranch->GetLeaf(static_cast<enum LeafIdentifier>(i));

    switch ((colorCode >> (i << 1)) & 3)  //(colorCode/pow(2,i*2) ) & 00000011b
      {
      case Brains2_MASKFILE_WHITE: // 0
        curnode->SetColor(Brains2_MASKFILE_WHITE);
        break;
      case Brains2_MASKFILE_BLACK: // 1
        curnode->SetColor(Brains2_MASKFILE_BLACK);
        break;
      case Brains2_MASKFILE_GRAY:  // 2
        //NOTE recursive call on all children to set them.
        curnode->SetBranch(
          readOctree(octreestream,machineByteOrder,
                     fileByteOrder)
          );
        break;
      }
    }
  return CurrentNodeBranch;
}

void Brains2MaskImageIO::Read(void* buffer)
{
  std::ifstream   local_InputStream;
  { //Just fast forward throuth the file header
  itk::Brains2IPLHeaderInfo DummyHeader;
  local_InputStream.open( this->m_FileName.c_str(), std::ios::in | std::ios::binary );
  if( local_InputStream.fail() )
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("File cannot be read");
    throw exception;
    }
  DummyHeader.ReadBrains2Header(local_InputStream);
  }
  //Actually start reading the octree
  unsigned int octreeHdr[6];
  //Need to gobble up the end of line character here and move one more byte.
  local_InputStream.ignore();
  local_InputStream.read((char *)octreeHdr,6*sizeof(unsigned int));
  if(this->m_ByteOrder != this->m_MachineByteOrder)
    {
    if ( m_MachineByteOrder == LittleEndian )
      {
      ByteSwapper<unsigned int>::
        SwapRangeFromSystemToBigEndian( octreeHdr,6 );
      }
    else
      {
      ByteSwapper<unsigned int>::
        SwapRangeFromSystemToLittleEndian( octreeHdr,6 );
      }
    }
  Octree<unsigned char,2,Brains2MaskMappingFunction<unsigned char> >::Pointer octree = 
    Octree<unsigned char,2,Brains2MaskMappingFunction<unsigned char> >::New();
  octree->SetDepth(octreeHdr[0]);
  octree->SetWidth(octreeHdr[1]);
  octree->SetTrueDims(octreeHdr[2],octreeHdr[3],octreeHdr[4]);
  this->m_Octree = octree;
  switch (octreeHdr[5])
    {
    case Brains2_MASKFILE_WHITE:
      //NOTE: THIS ALMOST NEVER HAPPENS!! All white image
      octree->SetColor(DEF_WHITE_MASK);
      break;
    case Brains2_MASKFILE_BLACK:
      //NOTE: THIS ALMOST NEVER HAPPENS!! All black image
      octree->SetColor(0);
      break;
    case Brains2_MASKFILE_GRAY:
      octree->SetTree(readOctree(local_InputStream,this->m_MachineByteOrder,this->m_ByteOrder ));
    }
  local_InputStream.close();
  //DEBUG: Now just convert the octree into an image for returning!!!
  //DEBUG:  This is written for 3D octreees only right now
  char * const p = static_cast<char *>(buffer);
  for(unsigned int k=0; k< this->m_Dimensions[2]; k++)
    {
    const unsigned int slice_offset=k*this->m_Dimensions[1]*this->m_Dimensions[0];
    for(unsigned int j=0; j< this->m_Dimensions[1]; j++)
      {
      const unsigned int sliceandrowoffset=slice_offset+j*this->m_Dimensions[0];
      for(unsigned int i=0; i< this->m_Dimensions[0]; i++)
        {
        unsigned int val =  octree->GetValue(i,this->m_Dimensions[1]-1-j,k);
        if(val != 0)
          p[sliceandrowoffset+i]= DEF_WHITE_MASK;
        else
          p[sliceandrowoffset+i]= 0;
        }
      }
    }
  return;
}
// This method will only test if the header looks like an
// Brains2Mask Header.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool Brains2MaskImageIO::CanReadFile( const char* FileNameToRead )
{
  m_FileName=FileNameToRead;
  std::ifstream   local_InputStream;
  local_InputStream.open( FileNameToRead, std::ios::in | std::ios::binary );
  if( local_InputStream.fail() )
    {
    return false;
    }
  try
    {
    this->m_IPLHeaderInfo.ClearHeader();
    this->m_IPLHeaderInfo.ReadBrains2Header(local_InputStream);
    }
  catch (itk::ExceptionObject & itkNotUsed(e))
    {
    return false;
    }

  local_InputStream.close();
  if(this->m_IPLHeaderInfo.DoesKeyExist("MASK_HEADER_BEGIN")==false)
    {
    return false;
    }
  this->m_ByteOrder=(this->m_IPLHeaderInfo.getString("BYTE_ORDER:")
                     =="LITTLE_ENDIAN") ? LittleEndian : BigEndian;
  this->m_MachineByteOrder = 
    (ByteSwapper<int>::SystemIsBigEndian() == true ) ? 
    BigEndian : LittleEndian ;

  //this->m_IPLHeaderInfo.PrintSelf(std::cout);
  const int TempNumDims=this->m_IPLHeaderInfo.getInt("MASK_NUM_DIMS:");
  this->SetNumberOfDimensions(TempNumDims);
  //NOTE: Brains2MaskImage dim[0] are the number of dims, and dim[1..7] are the
  // actual dims.
  m_Dimensions[ 0 ] = this->m_IPLHeaderInfo.getInt("MASK_X_SIZE:");
  m_Dimensions[ 1 ] = this->m_IPLHeaderInfo.getInt("MASK_Y_SIZE:");
  m_Dimensions[ 2 ] = this->m_IPLHeaderInfo.getInt("MASK_Z_SIZE:");
  m_Spacing[ 0 ]  = this->m_IPLHeaderInfo.getFloat("MASK_X_RESOLUTION:");
  m_Spacing[ 1 ]  = this->m_IPLHeaderInfo.getFloat("MASK_Y_RESOLUTION:");
  m_Spacing[ 2 ]  = this->m_IPLHeaderInfo.getFloat("MASK_Z_RESOLUTION:");

  m_ComponentType = CHAR;
  return true;
}

void Brains2MaskImageIO::ReadImageInformation()
{
  this->CanReadFile(this->m_FileName.c_str());
}

// cut the gordian knot, just do the header in one
// long printf
static const char mask_header_format[] =
"IPL_HEADER_BEGIN\n"
"PATIENT_ID: %s\n"
"SCAN_ID: %s\n"
"FILENAME: %s\n"
"DATE: %s\n"
"CREATOR: %s\n"
"PROGRAM: %s\n"
"MODULE: %s\n"
"VERSION: %s\n"
"NAME: %s\n"
"BYTE_ORDER: BIG_ENDIAN\n"
"MASK_HEADER_BEGIN\n"
"MASK_NUM_DIMS: %d\n"
"MASK_X_SIZE: %d\n"
"MASK_X_RESOLUTION: %f\n"
"MASK_Y_SIZE: %d\n"
"MASK_Y_RESOLUTION: %f\n"
"MASK_Z_SIZE: %d\n"
"MASK_Z_RESOLUTION: %f\n"
"MASK_THRESHOLD: %f\n"
"MASK_NAME: %d\n"
"MASK_ACQ_PLANE: CORONAL\n"
"MASK_HEADER_END\n"
"IPL_HEADER_END\n";

/**
   *
   */
void
Brains2MaskImageIO
::WriteImageInformation(void)
{
  return;
}
static bool
writeOctree (OctreeNode *branch,std::ofstream &output)
{
  unsigned i;
  unsigned short  colorCode = 0;

  for (i = 0; i < 8; i++)
    {
    OctreeNode &subnode = 
      branch->GetChild(static_cast<enum LeafIdentifier>(i));
    if (subnode.IsNodeColored())
      {
      if(subnode.GetColor() == Brains2_MASKFILE_BLACK)
        {
        colorCode |= Brains2_MASKFILE_BLACK << (i << 1);
        }
      else
        {
        colorCode |= Brains2_MASKFILE_WHITE << (i << 1);
        }
      }
    else
      {
      colorCode |= Brains2_MASKFILE_GRAY << (i << 1);
      }
    }
  itk::ByteSwapper<unsigned short>::SwapFromSystemToBigEndian(&colorCode);
  output.write((const char *)&colorCode,sizeof(colorCode));
  for (i = 0; i < 8; i++)
    {
    OctreeNode &subnode = 
      branch->GetChild(static_cast<enum LeafIdentifier>(i));
    if (!subnode.IsNodeColored())
      {
      writeOctree (&subnode, output);
      }
    }
  return true;
}


/**
   *
   */
void
Brains2MaskImageIO
::Write( const void* buffer)
{
  if(this->m_FileName == "") 
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("Error in OctreeCreation");
    throw exception;
    }
  std::ofstream output(this->m_FileName.c_str());
  if(output.fail())
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("Error in OctreeCreation");
    throw exception;
    }
  unsigned xsize = this->GetDimensions(0);
  unsigned ysize = this->GetDimensions(1);
  unsigned zsize = this->GetDimensions(2);

  itk::MetaDataDictionary &thisDic=this->GetMetaDataDictionary();
  std::string temp;
  std::string patient_id;
  if(itk::ExposeMetaData<std::string>(thisDic,ITK_PatientID,temp))
    {
    patient_id = temp;
    }
  // to do -- add more header crap
  //Write the image Information before writing data
  char buf[16384];
  sprintf(buf,mask_header_format,
          patient_id.c_str(),
          "",                 // scan_id
          "",                 // file_name
          "",                 // date
          "",                 // creator
          "",                 // program
          "",                 // module
          "",                 // version
          this->m_FileName.c_str(),   // name
          3,                  // num_dims
          xsize,              // xsize
          1.0,                // x_res
          ysize,              // ysize
          1.0,                // y_res
          zsize,              // zsize
          1.0,                // z_res
          0.0,                // threshold
          -1                 // mask_name
    );
  output.write(buf,strlen(buf));
  unsigned octreeHdr[6];
  OctreeNode *tree;
  OctreeBase::Pointer octBasePtr;
  if(m_PixelType == CHAR)
    {
    octBasePtr =  Octree<char,2,Brains2MaskMappingFunction<char> >::New();
    }
  else if(m_PixelType == UCHAR)
    {
    octBasePtr =  Octree<unsigned char,2,Brains2MaskMappingFunction<unsigned char> >::New();
    }
  else if(m_PixelType == SHORT)
    {
    octBasePtr =  Octree<short,2,Brains2MaskMappingFunction<short> >::New();
    }
  else if(m_PixelType == USHORT)
    {
    octBasePtr =  Octree<unsigned short,2,Brains2MaskMappingFunction<unsigned short> >::New();
    }
  else if(m_PixelType == INT)
    {
    octBasePtr =  Octree<int,2,Brains2MaskMappingFunction<int> >::New();
    }
  else if(m_PixelType == UINT)
    {
    octBasePtr =  Octree<unsigned int,2,Brains2MaskMappingFunction<unsigned int> >::New();
    }
  else if(m_PixelType == LONG)
    {
    octBasePtr =  Octree<long,2,Brains2MaskMappingFunction<long> >::New();
    }
  else if(m_PixelType == ULONG)
    {
    octBasePtr =  Octree<unsigned long,2,Brains2MaskMappingFunction<unsigned long> >::New();
    }
  else if(m_PixelType == FLOAT)
    {
    octBasePtr =  Octree<float,2,Brains2MaskMappingFunction<float> >::New();
    }
  else if(m_PixelType == DOUBLE)
    {
    octBasePtr =  Octree<double,2,Brains2MaskMappingFunction<double> >::New();
    }
  else
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("Pixel Type Unknown");
    throw exception;
    }
  octBasePtr->BuildFromBuffer(buffer,xsize,ysize,zsize);
  tree = octBasePtr->GetTree();

  octreeHdr[0] = octBasePtr->GetDepth();
  octreeHdr[1] = octBasePtr->GetWidth();
  octreeHdr[2] = xsize;
  octreeHdr[3] = ysize;
  octreeHdr[4] = zsize;
  if(tree->IsNodeColored())
    {
    octreeHdr[5] = tree->GetColor();
    }
  else
    {
    octreeHdr[5] = Brains2_MASKFILE_GRAY;
    }
  itk::ByteSwapper<unsigned>::SwapRangeFromSystemToBigEndian(octreeHdr,
                                                             6);
  output.write((const char *)octreeHdr,sizeof(unsigned)*6);

  if(!tree->IsNodeColored())
    {
    writeOctree(tree,output);
    }
  output.close();
}
} // end namespace itk
