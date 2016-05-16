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

#include "itksys/SystemTools.hxx"
#include "itkIPLCommonImageIO.h"
#include "itkByteSwapper.h"
#include "itkSpatialOrientationAdapter.h"
#include "itkDirectory.h"
#include "itkMetaDataObject.h"
#include <iostream>
#include <fstream>
#include <string.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <assert.h>
#include <vector>

//From uiig library "The University of Iowa Imaging Group-UIIG"

namespace itk
{
// Default constructor
IPLCommonImageIO::IPLCommonImageIO()
{
  m_SystemByteOrder =
    ByteSwapper< int >::SystemIsBigEndian() ?
    ImageIOBase::BigEndian :
    ImageIOBase::LittleEndian;
  m_ImageHeader = ITK_NULLPTR;
  m_FilenameList = new IPLFileNameList;
  this->SetComponentType(ImageIOBase::SHORT);
}

IPLCommonImageIO::~IPLCommonImageIO()
{
  delete m_ImageHeader;
  delete m_FilenameList;
}

void IPLCommonImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

bool IPLCommonImageIO::CanWriteFile(const char *)
{
  return false;
}

unsigned int IPLCommonImageIO::GetComponentSize() const
{
  return sizeof( short int );
}

void IPLCommonImageIO::Read(void *buffer)
{
  short int *                   img_buffer = (short int *)buffer;
  IPLFileNameList::IteratorType it = m_FilenameList->begin();
  IPLFileNameList::IteratorType itend = m_FilenameList->end();

  for (; it != itend; it++ )
    {
    std::string   curfilename = ( *it )->GetImageFileName();
    std::ifstream f;
    this->OpenFileForReading( f, curfilename );

    f.seekg ( ( *it )->GetSliceOffset(), std::ios::beg );
    if ( !this->ReadBufferAsBinary( f, img_buffer, m_FilenameList->GetXDim() * m_FilenameList->GetYDim()
                                    * sizeof( short int ) ) )
      {
      f.close();
      RAISE_EXCEPTION();
      }
    f.close();
    // ByteSwapper::SwapRangeFromSystemToBigEndian is set up based on
    // the FILE endian-ness, not as the name would lead you to believe.
    // So, on LittleEndian systems, SwapFromSystemToBigEndian will swap.
    // On BigEndian systems, SwapFromSystemToBigEndian will do nothing.
    itk::ByteSwapper< short int >::SwapRangeFromSystemToBigEndian( img_buffer,
                                                                   m_FilenameList->GetXDim() * m_FilenameList->GetYDim() );
    img_buffer += m_FilenameList->GetXDim() * m_FilenameList->GetYDim();
    }
}

GEImageHeader * IPLCommonImageIO::ReadHeader(const char *)
{
  //
  // must be redefined in a child class
  //
  return ITK_NULLPTR;
}

bool IPLCommonImageIO::CanReadFile(const char *)
{
  //
  // must be redefined in child class or you'll never read anything ;-)
  //
  return false;
}

void IPLCommonImageIO::ReadImageInformation()
{
  std::string FileNameToRead = this->GetFileName();
  //
  // GE images are stored in separate files per slice.
  //char imagePath[IOCommon::ITK_MAXPATHLEN+1];
  //TODO -- use std::string instead of C strings
  char        imageMask[IOCommon::ITK_MAXPATHLEN + 1];
  char        imagePath[IOCommon::ITK_MAXPATHLEN + 1];
  std::string _imagePath =
    itksys::SystemTools::CollapseFullPath( FileNameToRead.c_str() );

  FileNameToRead = _imagePath;

  this->m_ImageHeader = this->ReadHeader( FileNameToRead.c_str() );
  //
  // if anything fails in the header read, just let
  // exceptions propagate up.

  bool isCT = false;
  std::string modality = m_ImageHeader->modality;
  if( modality == "CT" )
    {
    isCT = true;
    }

  AddElementToList(m_ImageHeader->filename,
                   m_ImageHeader->sliceLocation,
                   m_ImageHeader->offset,
                   m_ImageHeader->imageXsize,
                   m_ImageHeader->imageYsize,
                   m_ImageHeader->imageXres,
                   m_ImageHeader->imageYres,
                   m_ImageHeader->seriesNumber,
                   (isCT)?m_ImageHeader->examNumber:
                   m_ImageHeader->echoNumber);  // If CT use examNumber, otherwise use echoNumber.

  // Add header info to metadictionary

  itk::MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
  std::string               classname( this->GetNameOfClass() );
  itk::EncapsulateMetaData< std::string >(thisDic, ITK_InputFilterName, classname);
  itk::EncapsulateMetaData< std::string >( thisDic, ITK_OnDiskStorageTypeName, std::string("SHORT") );
  itk::EncapsulateMetaData< short int >(thisDic, ITK_OnDiskBitPerPixel, (short int)16);

  //
  // has to be set before setting dir cosines,
  // otherwise the vector doesn't get allocated
  this->SetNumberOfDimensions(3);

  itk::EncapsulateMetaData< std::string >( thisDic, ITK_PatientID, std::string(m_ImageHeader->patientId) );
  itk::EncapsulateMetaData< std::string >( thisDic, ITK_ExperimentDate, std::string(m_ImageHeader->date) );

  if ( _imagePath == "" )
    {
    RAISE_EXCEPTION();
    }
  strncpy( imagePath, _imagePath.c_str(), sizeof( imagePath ) );
  imagePath[IOCommon::ITK_MAXPATHLEN] = '\0';
  strncpy( imageMask, imagePath, sizeof( imageMask ) );
  imageMask[IOCommon::ITK_MAXPATHLEN] = '\0';

  char *lastslash = strrchr(imagePath, '/');
  if ( lastslash == ITK_NULLPTR )
    {
    strcpy(imagePath, ".");
    }
  else
    {
    *lastslash = '\0';
    }
  itk::Directory::Pointer Dir = itk::Directory::New();
  if ( Dir->Load(imagePath) == 0 )
    {
    RAISE_EXCEPTION();
    }
  std::vector< std::string >::size_type i;
  std::vector< std::string >::size_type numfiles;

  GEImageHeader *curImageHeader;

  for ( i = 0, numfiles = Dir->GetNumberOfFiles(); i < numfiles; i++ )
    {
    const char *curFname =  Dir->GetFile(static_cast<unsigned int>( i ) );
    char        fullPath[IOCommon::ITK_MAXPATHLEN + 1];
    sprintf(fullPath, "%s/%s", imagePath, curFname);

    if ( curFname == ITK_NULLPTR )
      {
      break;
      }
    else if ( FileNameToRead == fullPath ) //strcmp(curFname,FileNameToRead) ==
                                           // 0)
      {
      continue;
      }
    try
      {
      curImageHeader = this->ReadHeader(fullPath);
      }
    catch ( itk::ExceptionObject & )
      {
      // ReadGE4XHeader throws an exception on any error.
      // So if, for example we run into a subdirectory, it would
      // throw an exception, and we'd just want to skip it.
      continue;
      }
    if( (((isCT)?curImageHeader->examNumber:curImageHeader->echoNumber)
        == m_FilenameList->GetKey2()) &&
        (curImageHeader->seriesNumber == m_FilenameList->GetKey1()))
      {
      AddElementToList(curImageHeader->filename,
                       curImageHeader->sliceLocation,
                       curImageHeader->offset,
                       curImageHeader->imageXsize,
                       curImageHeader->imageYsize,
                       curImageHeader->imageXres,
                       curImageHeader->imageYres,
                       curImageHeader->seriesNumber,
                       (isCT)?curImageHeader->examNumber:
                       curImageHeader->echoNumber);  // If CT use examNumber, otherwise use echoNumber.
      }
    delete curImageHeader;
    }

  //sort image list
  m_FilenameList->sortImageList();

  //
  //
  // set the image properties
  this->SetDimensions(0, m_ImageHeader->imageXsize);
  this->SetDimensions(1, m_ImageHeader->imageYsize);
  this->SetDimensions( 2, static_cast<unsigned int>( m_FilenameList->NumFiles() ) );
  this->SetSpacing(0, m_ImageHeader->imageXres);
  this->SetSpacing(1, m_ImageHeader->imageYres);
  this->SetSpacing(2, m_ImageHeader->sliceThickness + m_ImageHeader->sliceGap);

  //
  // set direction cosines
  typedef SpatialOrientationAdapter OrientAdapterType;
  SpatialOrientationAdapter::DirectionType dir =
    OrientAdapterType().ToDirectionCosines(m_ImageHeader->coordinateOrientation);
  std::vector< double > dirx(3, 0), diry(3, 0), dirz(3, 0);
  dirx[0] = dir[0][0];
  dirx[1] = dir[1][0];
  dirx[2] = dir[2][0];
  diry[0] = dir[0][1];
  diry[1] = dir[1][1];
  diry[2] = dir[2][1];
  dirz[0] = dir[0][2];
  dirz[1] = dir[1][2];
  dirz[2] = dir[2][2];

  this->SetDirection(0, dirx);
  this->SetDirection(1, diry);
  this->SetDirection(2, dirz);

  this->ModifyImageInformation();
}

void IPLCommonImageIO::SortImageListByNameAscend()
{
  m_FilenameList->SetSortOrder(IPLFileNameList::SortByNameAscend);
}

void IPLCommonImageIO::SortImageListByNameDescend()
{
  m_FilenameList->SetSortOrder(IPLFileNameList::SortByNameDescend);
}

/**
 *
 */
void
IPLCommonImageIO
::WriteImageInformation(void)
{
  RAISE_EXCEPTION();
}

/**
 *
 */
void
IPLCommonImageIO
::Write(const void *)
{
  RAISE_EXCEPTION();
}

int
IPLCommonImageIO
::GetStringAt(std::ifstream & f,
              std::streamoff Offset,
              char *buf,
              size_t amount, bool throw_exception)
{
  f.seekg(Offset, std::ios::beg);
  if ( f.fail() )
    {
    if ( throw_exception )
      {
      RAISE_EXCEPTION();
      }
    else
      {
      return -1;
      }
    }
  if ( !this->ReadBufferAsBinary(f, (void *)buf, amount) )
    {
    if ( throw_exception )
      {
      RAISE_EXCEPTION();
      }
    else
      {
      return -1;
      }
    }
  return 0;
}

int IPLCommonImageIO
::GetIntAt(std::ifstream & f, std::streamoff Offset, int *ip,
           bool throw_exception)
{
  int tmp;

  if ( this->GetStringAt(f, Offset, (char *)&tmp, sizeof( int ),
                         throw_exception) == 0 )
    {
    *ip = this->hdr2Int( (char *)&tmp );
    }
  else
    {
    *ip = 0;
    }
  return 0;
}

int IPLCommonImageIO
::GetShortAt(std::ifstream & f, std::streamoff Offset, short *ip,
             bool throw_exception)
{
  short tmp;

  if ( this->GetStringAt(f, Offset, (char *)&tmp, sizeof( short ),
                         throw_exception) == 0 )
    {
    *ip = this->hdr2Short( (char *)&tmp );
    }
  else
    {
    *ip = 0;
    }
  return 0;
}

int IPLCommonImageIO
::GetFloatAt(std::ifstream & f, std::streamoff Offset, float *ip,
             bool throw_exception)
{
  float tmp;

  if ( this->GetStringAt(f, Offset, (char *)&tmp, sizeof( float ),
                         throw_exception) == 0 )
    {
    *ip = this->hdr2Float( (char *)&tmp );
    }
  else
    {
    *ip = 0.0f;
    }
  return 0;
}

int IPLCommonImageIO
::GetDoubleAt(std::ifstream & f, std::streamoff Offset, double *ip,
              bool throw_exception)
{
  double tmp;

  if ( this->GetStringAt(f, Offset, (char *)&tmp, sizeof( double ),
                         throw_exception) == 0 )
    {
    *ip = this->hdr2Double( (char *)&tmp );
    }
  else
    {
    *ip = 0.0;
    }
  return 0;
}

short IPLCommonImageIO::hdr2Short(char *hdr)
{
  short shortValue;

  memcpy ( &shortValue, hdr, sizeof( short ) );
  ByteSwapper< short int >::SwapFromSystemToBigEndian (&shortValue);
  return ( shortValue );
}

int IPLCommonImageIO
::hdr2Int(char *hdr)
{
  int intValue;

  memcpy ( &intValue, hdr, sizeof( int ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian (&intValue);
  return ( intValue );
}

float IPLCommonImageIO
::hdr2Float(char *hdr)
{
  float floatValue;

  memcpy (&floatValue, hdr, 4);
  ByteSwapper< float >::SwapFromSystemToBigEndian (&floatValue);

  return ( floatValue );
}

double IPLCommonImageIO
::hdr2Double(char *hdr)
{
  double doubleValue;

  memcpy ( &doubleValue, hdr, sizeof( double ) );
  ByteSwapper< double >::SwapFromSystemToBigEndian (&doubleValue);

  return ( doubleValue );
}

int IPLCommonImageIO
::AddElementToList(char const *const filename,
                   const float sliceLocation,
                   const int offset,
                   const int XDim,
                   const int YDim,
                   const float XRes,
                   const float YRes,
                   const int Key1,
                   const int Key2)
{
  if ( m_FilenameList->NumFiles() == 0 )
    {
    m_FilenameList->SetXDim(XDim);
    m_FilenameList->SetYDim(YDim);
    m_FilenameList->SetXRes(XRes);
    m_FilenameList->SetYRes(YRes);
    m_FilenameList->SetKey1(Key1);
    m_FilenameList->SetKey2(Key2);
    }
  else if ( XDim != m_FilenameList->GetXDim() || YDim != m_FilenameList->GetYDim() )
    {
    return 0;
    }
  else if( itk::Math::NotAlmostEquals( XRes, m_FilenameList->GetXRes() ) || itk::Math::NotAlmostEquals( YRes, m_FilenameList->GetYRes() )  )
    {
    return 0;
    }
  else if ( m_FilenameList->GetKey1() != Key1 ||  m_FilenameList->GetKey2() != Key2 )
    {
    return 1;  //It is OK for keys to not match,  Just don't add.
    }
  m_FilenameList->AddElementToList(filename, sliceLocation,
                                   offset, XDim, YDim, XRes, YRes, 0, Key1, Key2);
  return 1;
}

void IPLCommonImageIO
::sortImageListAscend()
{
  m_FilenameList->sortImageListAscend();
}

void IPLCommonImageIO
::sortImageListDescend()
{
  m_FilenameList->sortImageListDescend();
}

int IPLCommonImageIO
::statTimeToAscii(void *clock, char *timeString,int len)
{

  time_t tclock = (time_t)*( (int *)clock );
  const char * const asciiTime = ctime (&tclock);

  strncpy (timeString, asciiTime, len);
  timeString[len-1] = '\0';
  char *newline;
  if((newline = strrchr(timeString,'\n')) != ITK_NULLPTR ||
     (newline = strrchr(timeString,'\r')))
    {
    *newline = '\0';
    }
  return 1;
}
} // end namespace itk
