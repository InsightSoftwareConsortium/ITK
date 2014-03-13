/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    $RCSfile: itkAnalyzeObjectLabelMapImageIO.cxx,v $
Language:  C++
Date:      $Date: 2007/03/29 18:39:28 $
Version:   $Revision: 1.32 $

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkAnalyzeObjectLabelMapImageIO.h"
#include "itkIOCommon.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkMetaDataObject.h"
#include "itkSpatialOrientationAdapter.h"
#include <itksys/SystemTools.hxx>
#include <vnl/vnl_math.h>
#include "itk_zlib.h"
#include <stdio.h>
#include <stdlib.h>
#include <vector>

namespace itk
{
// Streaming not yet supported, so use the default base class to return the LargestPossibleRegion
#if _USE_STREAMABLE_REGION_FOR_AOLM
ImageIORegion AnalyzeObjectLabelMapImageIO
::GenerateStreamableReadRegionFromRequestedRegion( const ImageIORegion & requestedRegion ) const
{
  std::cout << "AnalyzeObjectLabelMapImageIO::GenerateStreamableReadRegionFromRequestedRegion() " << std::endl;
  std::cout << "RequestedRegion = " << requestedRegion << std::endl;

  return requestedRegion;
}

#endif

AnalyzeObjectLabelMapImageIO::AnalyzeObjectLabelMapImageIO()
{
  // Nothing to do during initialization.
}

AnalyzeObjectLabelMapImageIO::~AnalyzeObjectLabelMapImageIO()
{
}

void AnalyzeObjectLabelMapImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

bool AnalyzeObjectLabelMapImageIO::CanWriteFile(const char * FileNameToWrite)
{
  if(FileNameToWrite == 0 || *FileNameToWrite == '\0' )
    {
    itkDebugMacro(<< "No filename specified.");
    return false;
    }
  // This assumes that the final '.' in a file name is the delimiter
  // for the file's extension type
  std::string filename = FileNameToWrite;
  const std::string fileExt = itksys::SystemTools::GetFilenameLastExtension(filename);
  if( fileExt != ".obj" )
    {
    return false;
    }

  return true;
}

void AnalyzeObjectLabelMapImageIO::Read(void* buffer)
{
  this->m_InputFileStream.open(m_FileName.c_str(), std::ios::binary | std::ios::in);
  this->m_InputFileStream.seekg( m_LocationOfFile);
  if( !this->m_InputFileStream.is_open() )
    {
    itkDebugMacro(<< "Error: Could not open " << m_FileName.c_str() );
    exit(-1);
    }
  // TODO: Image spacing needs fixing.  Will need to look to see if a
  //      .nii, .nii.gz, or a .hdr file
  //      exists for the same .obj file.
  //      If so, then read in the spacing for those images.

  // When this function decods the run length encoded raw data into an unsigned char volume
  // store the values into this structure.
  struct RunLengthStruct
  {
    unsigned char voxel_count;
    unsigned char voxel_value;
  };
  typedef struct RunLengthStruct RunLengthElement;
  RunLengthElement RunLengthArray[NumberOfRunLengthElementsPerRead];

  // The file consists of unsigned character pairs which represents the encoding of the data
  // The character pairs have the form of length, tag value.  Note also that the data in
  // Analyze object files are run length encoded a plane at a time.

  int            index = 0;
  int            voxel_count_sum = 0;
  unsigned char *tobuf = (unsigned char *)buffer;

  // The following commented out code is a starting attempt at streaming.

  //    ImageIORegion regionToRead = this->GetIORegion();
  // ImageIORegion::SizeType size = regionToRead.GetSize();
  // ImageIORegion::IndexType start = regionToRead.GetIndex();
  // const int DimensionSize = regionToRead.GetImageDimension();
  // int dimensions[4] = {1};
  // int startPoint[4] = {1};
  // for(int i = 0; i<4; i ++)
  // {
  //  dimensions[i] = regionToRead.GetSize(i);
  //  startPoint[i] = regionToRead.GetIndex(i);
  // }
  //  int xDim = 1;
  //  int yDim = 1;
  //  int zDim = 1;
  //  switch (regionToRead.GetImageDimension())
  //  {
  //    case 3:
  //      zDim = regionToRead.GetSize(2);
  //    case 2:
  //      yDim = regionToRead.GetSize(1);
  //    case 1:
  //      xDim = regionToRead.GetSize(0);
  //      break;
  //    default:
  //      break;
  //  }

  /*const int numberOfBytesToRead = 2 * startPoint[0] * startPoint[1] * startPoint[2];
      int *cornersOfRegion;
      cornersOfRegion = new int(numberOfBytesToRead);
      for(int i = 1; i <zDim+1; i++)
      {
        for(int j = 1; j<(2 * startPoint[0] * startPoint[1] * startPoint[2]); j++)
        {
          if((j-1)%2 != 2 && j!=1)
          {
            cornersOfRegion[j] = (startPoint[0] * startPoint[1] * startPoint[2] + xDim * (j/2) + startPoint[1] + (j-1)/2) * i;
          }
          else if(j%2 != 2)
          {
            cornersOfRegion[j] = (startPoint[0] * startPoint[1] * startPoint[2] + xDim * (j/2)) * i;
          }
          else
          {
            cornersOfRegion[0] = startPoint[0] * startPoint[1] * startPoint[2];
          }
        }
      }
      std::cout<<"Can not deal with the image size right now"<<std::endl;*/

  int VolumeSize = 1;
  unsigned dim = this->GetNumberOfDimensions();
  switch(dim)
    {
    case 4:
      VolumeSize *= this->GetDimensions(3);
    case 3:
      VolumeSize *= this->GetDimensions(2);
    case 2:
      VolumeSize *= this->GetDimensions(1);
    case 1:
      VolumeSize *= this->GetDimensions(0);
      break;
    default:
      itkExceptionMacro(<< "Dimensions " << dim << " > maximum dimension 4");
    }

  // You can uncomment the following commented out code to have a file written with the exact values that are read in.

  //      std::ofstream myfile;
  //      myfile.open("VoxelInformationReader.txt", myfile.app);
  while( !this->m_InputFileStream.read(reinterpret_cast<char *>(RunLengthArray), sizeof(RunLengthElement)
                                       * NumberOfRunLengthElementsPerRead).eof() )
    {
    for( int i = 0; i < NumberOfRunLengthElementsPerRead; i++ )
      {
      //           myfile<< "Assigning: " << (int)RunLengthArray[i].voxel_count
      //             << " voxels of label " << (int)RunLengthArray[i].voxel_value
      //             << std::endl;
      if( RunLengthArray[i].voxel_count == 0 )
        {
        itkDebugMacro(
          << "Inside AnaylzeObjectLabelMap Invalid Length " << (int)RunLengthArray[i].voxel_count << std::endl);
        exit(-1);
        }
      for( int j = 0; j < RunLengthArray[i].voxel_count; j++ )
        {
        tobuf[index] = RunLengthArray[i].voxel_value;
        index++;
        }
      voxel_count_sum += RunLengthArray[i].voxel_count;
      //          myfile <<"index = "<<index
      //            << " voxel_count_sum= " << voxel_count_sum
      //            << " Volume size = "<<VolumeSize<<std::endl;
      if( index > VolumeSize )
        {
        itkDebugMacro(<< "BREAK!\n");
        exit(-1);
        }
      }
    }

  //      myfile.close();

  if( index != VolumeSize )
    {
    itkDebugMacro(<< "Warning: Error decoding run-length encoding." << std::endl);
    if( index < VolumeSize )
      {
      itkDebugMacro(<< "Warning: file underrun." << std::endl);
      }
    else
      {
      itkDebugMacro(<< "Warning: file overrun." << std::endl);
      }
    exit(-1);
    }

  this->m_InputFileStream.close();

// The following commented code will run through all of the values and write them to a file.

//    std::ofstream check;
//    check.open("CheckBuffer.txt");
//    for(int i=0; i<VolumeSize;i++)
//    {
//      check<<(int)tobuf[i]<<std::endl;
//    }
//    check.close();

}

bool AnalyzeObjectLabelMapImageIO::CanReadFile( const char* FileNameToRead )
{
  itkDebugMacro(<< "I am in Can Read File for AnalyzeObjectLabelMapImageIO" << std::endl);
  if(FileNameToRead == 0 || *FileNameToRead == '\0' )
    {
    itkDebugMacro(<< "No filename specified.");
    return false;
    }
  // check for existence.
  if(!itksys::SystemTools::FileExists(FileNameToRead,true))
    {
    return false;
    }

  // This assumes that the final '.' in a file name is the delimiter
  // for the file's extension type
  std::string filename = FileNameToRead;
  const std::string fileExt = itksys::SystemTools::GetFilenameLastExtension(filename);
  if( fileExt != ".obj" )
    {
    return false;
    }

  return true;
}

void AnalyzeObjectLabelMapImageIO::ReadImageInformation()
{
  m_ComponentType = CHAR;
  m_PixelType = SCALAR;
  // Opening the file
  std::ifstream inputFileStream;
  inputFileStream.open(m_FileName.c_str(), std::ios::binary | std::ios::in);
  if( !inputFileStream.is_open() )
    {
    itkDebugMacro(<< "Error: Could not open: " << m_FileName.c_str() << std::endl);
    exit(-1);
    }

  // Reading the header, which contains the version number, the size, and the
  // number of objects
  bool NeedByteSwap = false;

  int header[6] = {1};
  if(  inputFileStream.read(reinterpret_cast<char *>(header), sizeof(int) * 5).fail() )
    {
    itkDebugMacro(<< "Error: Could not read header of " << m_FileName.c_str() << std::endl);
    exit(-1);
    }
  // Do byte swapping if necessary.
  if( header[0] == -1913442047 || header[0] == 1323699456 )   // Byte swapping needed (Number is byte swapped number of
                                                              // VERSION7)
    {
    NeedByteSwap = true;
    // NOTE: All analyze object maps should be big endian on disk in order to be valid
    //      The following function calls will swap the bytes when on little endian machines.
    itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(header[0]) );
    itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(header[1]) );
    itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(header[2]) );
    itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(header[3]) );
    itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(header[4]) );
    }

  bool NeedBlendFactor = false;
  if( header[0] == VERSION7 )
    {
    if( ( inputFileStream.read(reinterpret_cast<char *>(&(header[5]) ), sizeof(int) * 1) ).fail() )
      {
      itkDebugMacro(<< "Error: Could not read header of " << m_FileName.c_str() << std::endl);
      exit(-1);
      }

    if( NeedByteSwap )
      {
      itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(header[5]) );
      }
    NeedBlendFactor = true;
    }
  // Now the file pointer is pointing to the image region

  if( header[5] > 1 )
    {
    this->SetNumberOfDimensions(4);
    }
  else if( header[3] > 1 )
    {
    this->SetNumberOfDimensions(3);
    }
  else if( header[2] > 1 )
    {
    this->SetNumberOfDimensions(2);
    }
  else
    {
    this->SetNumberOfDimensions(1);
    }

  switch( this->GetNumberOfDimensions() )
    {
    case 4:
    {
    this->SetDimensions(3, header[5]);
    this->SetSpacing(3, 1);
    }
    case 3:
    {
    this->SetDimensions(2, header[3]);
    this->SetSpacing(2, 1);
    }
    case 2:
    {
    this->SetDimensions(1, header[2]);
    this->SetSpacing(1, 1);
    }
    case 1:
    {
    this->SetDimensions(0, header[1]);
    this->SetSpacing(0, 1);
    }
    default:
    {
    }
    break;
    }

  std::vector<double> dirx(this->GetNumberOfDimensions(), 0), diry(this->GetNumberOfDimensions(), 0), dirz(
    this->GetNumberOfDimensions(), 0);

  switch( this->GetNumberOfDimensions() )
    {
    case 4:
    {
    m_Origin[3] = 0;
    }
    case 3:
    {
    dirx[2] = 0;
    diry[2] = 0;
    dirz[2] = 1;

    dirz[1] = 0;

    dirz[0] = 0;

    m_Origin[2] = 0;
    }
    case 2:
    {
    dirx[1] = 0;
    diry[1] = 1;
    diry[0] = 0;

    m_Origin[1] = 0;
    }
    case 1:
    {
    dirx[0] = 1;
    m_Origin[0] = 0;

    }
    break;
    default:
    {
    itkDebugMacro(<< "Error:  Setting the steps has an error" << std::endl);
    }
    break;
    }

  this->SetDirection(0, dirx);
  if( this->GetNumberOfDimensions() > 1 )
    {
    this->SetDirection(1, diry);
    }

  if( this->GetNumberOfDimensions() > 2 )
    {
    this->SetDirection(2, dirz);
    }

  // Error checking the number of objects in the object file
  if( (header[4] < 1) || (header[4] > 256) )
    {
    itkDebugMacro(<< "Error: Invalid number of object files.\n");
    inputFileStream.close();
    exit(-1);
    }

  /*std::ofstream myfile;
  myfile.open("ReadFromFilePointer35.txt", myfile.app);*/
  itk::AnalyzeObjectEntryArrayType my_reference;
  (my_reference).resize(header[4]);
  for( int i = 0; i < header[4]; i++ )
    {
    // Allocating a object to be created
    (my_reference)[i] = AnalyzeObjectEntry::New();
    (my_reference)[i]->ReadFromFilePointer( inputFileStream, NeedByteSwap, NeedBlendFactor);
    // (*my_reference)[i]->Print(myfile);
    }
  // myfile.close();
  m_LocationOfFile =  inputFileStream.tellg();
  inputFileStream.close();
  // Now fill out the MetaData
  MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
  EncapsulateMetaData<std::string>(thisDic, ITK_OnDiskStorageTypeName, std::string(typeid(unsigned char).name() ) );
  EncapsulateMetaData<itk::AnalyzeObjectEntryArrayType>(thisDic, ANALYZE_OBJECT_LABEL_MAP_ENTRY_ARRAY, my_reference);
}

/**
 *
 */
void
AnalyzeObjectLabelMapImageIO
::WriteImageInformation(void)
{
  itkDebugMacro(<< "I am in the writeimageinformaton" << std::endl);
  std::string tempfilename = this->GetFileName();
  // Opening the file
  std::ofstream outputFileStream;
  outputFileStream.open(tempfilename.c_str(), std::ios::binary | std::ios::out | std::ios::trunc);
  if( !outputFileStream.is_open() )
    {
    itkDebugMacro(<< "Error: Could not open " << tempfilename.c_str() << std::endl);
    exit(-1);
    }
  itk::AnalyzeObjectEntryArrayType my_reference;

  int header[6] = {1, 1, 1, 1, 1, 1};
  header[0] = VERSION7;

  switch( this->GetNumberOfDimensions() )
    {
    case 4:
    {
    header[5] = this->GetDimensions(3);
    }
    case 3:
    {
    header[3] = this->GetDimensions(2);
    }
    case 2:
    {
    header[2] = this->GetDimensions(1);
    }
    case 1:
    {
    header[1] = this->GetDimensions(0);
    }
    break;
    default:
      itkDebugMacro(<< "There is an error in writing the header for the Dimensions");
      exit(-1);
    }

  bool MetaDataCheck = itk::ExposeMetaData<itk::AnalyzeObjectEntryArrayType>(
    this->GetMetaDataDictionary(), ANALYZE_OBJECT_LABEL_MAP_ENTRY_ARRAY, my_reference);
  if( MetaDataCheck )
    {
    header[4] = my_reference.size();
    }
  else
    {
    header[4] = 256;
    }

  // All object maps are written in BigEndian format as required by the AnalyzeObjectMap documentation.
  bool NeedByteSwap = itk::ByteSwapper<int>::SystemIsLittleEndian();
  if( NeedByteSwap )
    {
    itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(header[0]) );
    itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(header[1]) );
    itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(header[2]) );
    itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(header[3]) );
    itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(header[4]) );
    itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(header[5]) );
    }

  // Writing the header, which contains the version number, the size, and the
  // number of objects
  if( outputFileStream.write(reinterpret_cast<char *>(header), sizeof(int) * 6).fail() )
    {
    itkDebugMacro(<< "Error: Could not write header of " << tempfilename.c_str() << std::endl);
    exit(-1);
    }

  // Error checking the number of objects in the object file
  if( my_reference.size() > 256 )
    {
    itkDebugMacro(<< "Error: Invalid number of object files.\n" );
    outputFileStream.close();
    }

  if( MetaDataCheck )
    {
    // Since the NumberOfObjects does not reflect the background, the background will be included
    for( unsigned int i = 0; i < my_reference.size(); i++ )
      {
      // Using a temporary so that the object file is always written in BIG_ENDIAN mode but does
      // not affect the current object itself
      AnalyzeObjectEntry *ObjectWrite = my_reference[i];
      if( NeedByteSwap == true )
        {
        ObjectWrite->SwapObjectEndedness();
        }
      ObjectWrite->Write(outputFileStream);

      }
    }
  else
    {
    for( unsigned int i = 0; i < 256; i++ )
      {
      // Using a temporary so that the object file is always written in BIG_ENDIAN mode but does
      // not affect the current object itself
      AnalyzeObjectEntry::Pointer ObjectWrite = AnalyzeObjectEntry::New();
      ObjectWrite->SetName("Blank Object");
      if( NeedByteSwap == true )
        {
        ObjectWrite->SwapObjectEndedness();
        }
      ObjectWrite->Write(outputFileStream);

      }
    }

  outputFileStream.close();
}

/**
 *
 */
void
AnalyzeObjectLabelMapImageIO

::Write( const void* buffer)
{
  if( this->GetComponentType() != UCHAR )
    {
    std::cerr << "Error: The pixel type needs to be an unsigned char." << std::endl;
    exit(-1);
    }
  this->WriteImageInformation();
  std::string tempfilename = this->GetFileName();
  // Opening the file
  std::ofstream outputFileStream;
  outputFileStream.open(tempfilename.c_str(), std::ios::binary | std::ios::out | std::ios::app);
  if( !outputFileStream.is_open() )
    {
    itkDebugMacro(<< "Error: Could not open " << tempfilename.c_str() << std::endl);
    exit(-1);
    }
  itk::AnalyzeObjectEntryArrayType my_reference;
  itk::ExposeMetaData<itk::AnalyzeObjectEntryArrayType>(
    this->GetMetaDataDictionary(), ANALYZE_OBJECT_LABEL_MAP_ENTRY_ARRAY, my_reference);

  // Encoding the run length encoded raw data into an unsigned char volume
  int VolumeSize;
  if( this->GetNumberOfDimensions() > 3 )
    {
    VolumeSize = this->GetDimensions(0) * this->GetDimensions(1) * this->GetDimensions(2) * this->GetDimensions(3);
    }
  else if( this->GetNumberOfDimensions() == 3 )
    {
    VolumeSize = this->GetDimensions(0) * this->GetDimensions(1) * this->GetDimensions(2);
    }
  else if( this->GetNumberOfDimensions() == 2 )
    {
    VolumeSize = this->GetDimensions(0) * this->GetDimensions(1);
    }
  else
    {
    VolumeSize = this->GetDimensions(0);
    }
  int PlaneSize;
  if( this->GetNumberOfDimensions() > 1 )
    {
    PlaneSize = this->GetDimensions(0) * this->GetDimensions(1);
    }
  else
    {
    PlaneSize = this->GetDimensions(0);
    }
  int           bufferindex = 0;
  int           planeindex = 0;
  int           runlength = 0;
  unsigned char CurrentObjIndex = 0;
  const  int    buffer_size = 16384; // NOTE: This is probably overkill, but it will work
  unsigned char bufferObjectMap[buffer_size] = {0};

  unsigned char *bufferChar = (unsigned char *)buffer;
  for( int i = 0; i < VolumeSize; i++ )
    {
    if( runlength == 0 )
      {
      CurrentObjIndex = bufferChar[i];
      runlength = 1;
      }
    else
      {
      if( CurrentObjIndex == bufferChar[i] )
        {
        runlength++;
        if( runlength == 255 )
          {
          bufferObjectMap[bufferindex] = runlength;
          bufferObjectMap[bufferindex + 1] = CurrentObjIndex;
          bufferindex += 2;
          runlength = 0;
          }
        }
      else
        {
        bufferObjectMap[bufferindex] = runlength;
        bufferObjectMap[bufferindex + 1] = CurrentObjIndex;
        bufferindex += 2;
        CurrentObjIndex = bufferChar[i];
        runlength = 1;
        }
      }

    planeindex++;
    if( planeindex == PlaneSize )
      {
      // Just finished with a plane of data, so encode it
      planeindex = 0;
      if( runlength != 0 )
        {
        bufferObjectMap[bufferindex] = runlength;
        bufferObjectMap[bufferindex + 1] = CurrentObjIndex;
        bufferindex += 2;
        runlength = 0;
        }
      }
    if( bufferindex == buffer_size )
      {
      // buffer full
      if( outputFileStream.write(reinterpret_cast<char *>(bufferObjectMap), buffer_size).fail() )
        {
        itkDebugMacro(<< "error: could not write buffer" << std::endl);
        exit(-1);
        }
      bufferindex = 0;
      }

    }
  if( bufferindex != 0 )
    {
    if( runlength != 0 )
      {
      bufferObjectMap[bufferindex] = runlength;
      bufferObjectMap[bufferindex + 1] = CurrentObjIndex;
      bufferindex += 2;
      }
    if( outputFileStream.write(reinterpret_cast<char *>(bufferObjectMap), bufferindex).fail() )
      {
      itkDebugMacro(<< "error: could not write buffer" << std::endl);
      exit(-1);
      }
    }
}

} // end namespace itk
