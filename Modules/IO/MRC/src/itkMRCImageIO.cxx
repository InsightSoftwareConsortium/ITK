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
#include "itkMRCImageIO.h"
#include "itkMRCImageIOPrivate.h"

#include "itkMetaDataObject.h"
#include "itkIOCommon.h"
#include "itkByteSwapper.h"

#include <fstream>

#include "itksys/SystemTools.hxx"


namespace itk
{
const char *MRCImageIO::m_MetaDataHeaderName = "MRCHeader";

MRCImageIO::MRCImageIO()
  : StreamingImageIOBase()
{
  this->SetNumberOfComponents(1);
  this->SetNumberOfDimensions(3);
  this->SetFileTypeToBinary();

  this->AddSupportedReadExtension(".mrc");
  this->AddSupportedReadExtension(".rec");

  this->AddSupportedWriteExtension(".mrc");
  this->AddSupportedWriteExtension(".rec");
}

void MRCImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

bool MRCImageIO::CanReadFile(const char *filename)
{
  std::string fname = filename;

  if ( fname != ""
       && ( fname.find(".mrc") < fname.length()
            || fname.find(".rec") < fname.length() ) )
    {
    return true;
    }

  std::ifstream file;

  try
    {
    // this may throw an expection, but we just return false
    this->OpenFileForReading( file, fname );
    }
  catch ( ExceptionObject & )
    {
    return false;
    }

  itkDebugMacro(<< "Reading Magic numbers " << filename);
  char map[4];
  char stamp[4];

  // special offset to magic number
  file.seekg(208);

  if ( !this->ReadBufferAsBinary(file, static_cast< void * >( &map ), 4)
       || !this->ReadBufferAsBinary(file, static_cast< void * >( &stamp ), 4) )
    {
    return false;
    }

  // check the magic number
  if ( strncmp(map, magicMAP, 4) != 0 )
    {
    return false;
    }

  return true;
}

MRCImageIO::SizeType MRCImageIO::GetHeaderSize(void) const
{
  if ( m_MRCHeader.IsNull() )
    {
    itkExceptionMacro(<< "Must read info first");
    }

  return m_MRCHeader->GetExtendedHeaderSize() + m_MRCHeader->GetHeaderSize();
}

void MRCImageIO::ReadImageInformation(void)
{
  std::ifstream file;

  this->InternalReadImageInformation(file);

  if ( m_MRCHeader->IsOriginalHeaderBigEndian() )
    {
    this->SetByteOrderToBigEndian();
    }
  else
    {
    this->SetByteOrderToLittleEndian();
    }

  const MRCHeaderObject::Header & header = m_MRCHeader->GetHeader();

  // fixed types defined by header
  switch  ( header.mode )
    {
    case MRCHeaderObject::MRCHEADER_MODE_UINT8:
      {
      // todo: the format is unclear weather this is signed or
      // unsigned, it would be best to check the min and max in the
      // header to see what makes since
      this->SetComponentType(UCHAR);
      this->SetNumberOfComponents(1);
      this->SetPixelType(SCALAR);
      break;
      }
    case MRCHeaderObject::MRCHEADER_MODE_IN16:
      {
      this->SetComponentType(SHORT);
      this->SetNumberOfComponents(1);
      this->SetPixelType(SCALAR);
      break;
      }
    case MRCHeaderObject::MRCHEADER_MODE_FLOAT:
      {
      this->SetComponentType(FLOAT);
      this->SetNumberOfComponents(1);
      this->SetPixelType(SCALAR);
      break;
      }
    case MRCHeaderObject::MRCHEADER_MODE_COMPLEX_INT16:
      {
      // ITK does not support short complex well
      // but if the program has gotten this far we can just write it out
      this->SetComponentType(SHORT);
      this->SetNumberOfComponents(2);
      this->SetPixelType(COMPLEX);
      break;
      }
    case MRCHeaderObject::MRCHEADER_MODE_COMPLEX_FLOAT:
      {
      this->SetComponentType(FLOAT);
      this->SetNumberOfComponents(2);
      this->SetPixelType(COMPLEX);
      break;
      }
    case MRCHeaderObject::MRCHEADER_MODE_UINT16:
      {
      this->SetComponentType(USHORT);
      this->SetNumberOfComponents(1);
      this->SetPixelType(SCALAR);
      break;
      }
    case MRCHeaderObject::MRCHEADER_MODE_RGB_BYTE:
      {
      this->SetComponentType(UCHAR);
      this->SetNumberOfComponents(3);
      this->SetPixelType(RGB);
      break;
      }
    default:
      {
      itkExceptionMacro(<< "Unrecognized mode");
      }
    }

  if ( header.xlen == 0.0f
       && header.ylen == 0.0f
       && header.zlen == 0.0f )
    {
    // if the spacing was not set in the header then this is the
    // default
    m_Spacing[0] = 1.0;
    m_Spacing[1] = 1.0;
    m_Spacing[2] = 1.0;
    }
  else
    {
    m_Spacing[0] = header.xlen / float(header.mx);
    m_Spacing[1] = header.ylen / float(header.my);
    m_Spacing[2] = header.zlen / float(header.mz);
    }

  // copy the origin
  m_Origin[0] = header.xorg;
  m_Origin[1] = header.yorg;
  m_Origin[2] = header.zorg;

  // copy the size of the dimensions
  m_Dimensions[0] = header.nx;
  m_Dimensions[1] = header.ny;
  m_Dimensions[2] = header.nz;

  // add the MRCHeader object into the metadata dictionary to that it
  // can be accessed
  MetaDataDictionary & thisDic = this->GetMetaDataDictionary();

  std::string classname( this->GetNameOfClass() );
  EncapsulateMetaData< std::string >(thisDic, ITK_InputFilterName, classname);
  EncapsulateMetaData< MRCHeaderObject::ConstPointer >( thisDic, m_MetaDataHeaderName,
                                                        MRCHeaderObject::ConstPointer(m_MRCHeader) );
}

// methods to load the data into the MRCHeader member variable
void MRCImageIO::InternalReadImageInformation(std::ifstream & file)
{
  char *buffer = ITK_NULLPTR;

  try
    {
    m_MRCHeader = MRCHeaderObject::New();

    itkDebugMacro(<< "Reading Information ");

    this->OpenFileForReading( file, m_FileName );

    buffer = new char[m_MRCHeader->GetHeaderSize()];
    if ( !this->ReadBufferAsBinary( file, static_cast< void * >( buffer ), m_MRCHeader->GetHeaderSize() ) )
      {
      itkExceptionMacro(<< "Header Read failed: Wanted "
                        << m_MRCHeader->GetHeaderSize()
                        << " bytes, but read "
                        << file.gcount() << " bytes.");
      }

    // convert the raw buffer into the header
    if ( !m_MRCHeader->SetHeader( reinterpret_cast< const MRCHeaderObject::Header * >( buffer ) ) )
      {
      itkExceptionMacro(<< "Unrecognized header");
      }

    delete[] buffer;

    buffer = new char[m_MRCHeader->GetExtendedHeaderSize()];
    if ( !this->ReadBufferAsBinary( file, static_cast< void * >( buffer ),  m_MRCHeader->GetExtendedHeaderSize() ) )
      {
      itkExceptionMacro(<< "Extended Header Read failed.");
      }

    m_MRCHeader->SetExtendedHeader(buffer);
    }
  catch ( ... )
    {
    // clean up dynamic allocation
    delete[] buffer;
    buffer = ITK_NULLPTR;
    throw;
    }

  delete[] buffer;
}

void MRCImageIO
::Read(void *buffer)
{
  std::ifstream file;

  if ( this->RequestedToStream() )
    {
    // open and stream read
    this->OpenFileForReading( file, m_FileName );

    this->StreamReadBufferAsBinary(file, buffer);
    }

  else
    {
    // open the file
    this->OpenFileForReading( file, m_FileName );

    // seek base the header
    std::streampos dataPos = static_cast< std::streampos >( this->GetHeaderSize() );
    file.seekg(dataPos, std::ios::beg);

    if ( file.fail() )
      {
      itkExceptionMacro(<< "Failed seeking to data position");
      }

    // read the image
    this->ReadBufferAsBinary( file, buffer, this->GetImageSizeInBytes() );
    }

  int size = this->GetComponentSize();
  switch ( size )
    {
    case 1:
      break;
    case 2:
      this->GetByteOrder() == BigEndian ?
      ByteSwapper< uint16_t >::SwapRangeFromSystemToBigEndian( (uint16_t *)buffer, this->GetImageSizeInComponents() ) :
      ByteSwapper< uint16_t >::SwapRangeFromSystemToLittleEndian( (uint16_t *)buffer,
                                                                  this->GetImageSizeInComponents() );
      break;
    case 4:
      this->GetByteOrder() == BigEndian ?
      ByteSwapper< uint32_t >::SwapRangeFromSystemToBigEndian( (uint32_t *)buffer, this->GetImageSizeInComponents() ) :
      ByteSwapper< uint32_t >::SwapRangeFromSystemToLittleEndian( (uint32_t *)buffer,
                                                                  this->GetImageSizeInComponents() );
      break;
    default:
      itkExceptionMacro(<< "Unknown component size");
    }
}

bool MRCImageIO::CanWriteFile(const char *fname)
{
  std::string filename = fname;

  if ( filename.length() > 4
       && ( filename.find(".mrc") == filename.length() - 4
            || filename.find(".rec") == filename.length() - 4 ) )
    {
    return true;
    }
  return false;
}

template< typename TPixelType >
void MRCImageIO::UpdateHeaderWithMinMaxMean(const TPixelType *bufferBegin)
{
  typedef const TPixelType *ConstPixelPointer;

  ConstPixelPointer bufferEnd = bufferBegin + m_IORegion.GetNumberOfPixels();

  // this could be replaced with std::min_element and
  // std::max_element, but that is slightly less efficient
  std::pair< ConstPixelPointer, ConstPixelPointer > mm =
    itk::min_max_element(bufferBegin, bufferEnd);

  double mean = std::accumulate( bufferBegin, bufferEnd, double(0.0) )
    / std::distance(bufferBegin, bufferEnd);

  m_MRCHeader->m_Header.amin = float(*mm.first);
  m_MRCHeader->m_Header.amax = float(*mm.second);
  m_MRCHeader->m_Header.amean = float(mean);
}

void MRCImageIO::UpdateHeaderFromImageIO(void)
{
  MRCHeaderObject::Header header;

  memset( &header, 0, sizeof( MRCHeaderObject::Header ) );

  itkAssertOrThrowMacro (this->GetNumberOfDimensions() != 0, "Invalid Dimension for Writting");
  if ( this->GetNumberOfDimensions() > 3 )
    {
    itkExceptionMacro(<< "MRC Writer can not write more than 3-dimensional images");
    }

  // magic number
  memcpy(header.cmap, magicMAP, 4);

  if ( ByteSwapper< void * >::SystemIsBigEndian() )
    {
    header.stamp[0] = 17;
    }
  else
    {
    header.stamp[0] = 68;
    }

  header.alpha = 90;
  header.beta = 90;
  header.gamma = 90;

  header.mapc = MRCHeaderObject::MRCHEADER_MAP_X;
  header.mapr = MRCHeaderObject::MRCHEADER_MAP_Y;
  header.maps = MRCHeaderObject::MRCHEADER_MAP_Z;

  header.mx = header.nx = m_Dimensions[0];
  header.my = header.ny = (  this->GetNumberOfDimensions() >= 2 ) ? m_Dimensions[1] : 1;
  header.mz = header.nz = (  this->GetNumberOfDimensions() >= 3 ) ? m_Dimensions[2] : 1;

  header.mode = -1;
  if ( this->GetNumberOfComponents() == 1 )
    {
    if ( this->GetComponentType() == UCHAR )
      {
      header.mode = MRCHeaderObject::MRCHEADER_MODE_UINT8;
      }
    else if ( this->GetComponentType() == SHORT )
      {
      header.mode = MRCHeaderObject::MRCHEADER_MODE_IN16;
      }
    else if ( this->GetComponentType() == FLOAT )
      {
      header.mode = MRCHeaderObject::MRCHEADER_MODE_FLOAT;
      }
    else if ( this->GetComponentType() == USHORT )
      {
      header.mode = MRCHeaderObject::MRCHEADER_MODE_UINT16;
      }
    }
  else if ( this->GetNumberOfComponents() == 2 &&
            this->GetPixelType() == COMPLEX )
    {
    if ( this->GetComponentType() == FLOAT )
      {
      header.mode = MRCHeaderObject::MRCHEADER_MODE_COMPLEX_FLOAT;
      }
    // ITK does not support short complex well
    // but if we have gotten this far, it's done
    else if ( this->GetComponentType() == SHORT )
      {
      header.mode = MRCHeaderObject::MRCHEADER_MODE_COMPLEX_INT16;
      }
    }
  else if (  this->GetNumberOfComponents() == 3
             && this->GetComponentType() == UCHAR )
    {
    header.mode = MRCHeaderObject::MRCHEADER_MODE_RGB_BYTE;
    }

  if ( header.mode == -1 )
    {
    itkExceptionMacro(<< "Unsupported pixel type: " << this->GetPixelTypeAsString( this->GetPixelType() )
                      << " " << this->GetComponentTypeAsString(
                        this->GetComponentType() )
                      << std::endl
                      <<
                      "Supported pixel types include unsigned byte, unsigned short, signed short, float, rgb unsigned char, float complex"
                      );
    }

  header.nxstart = 0;
  header.nystart = 0;
  header.nzstart = 0;

  header.xlen = m_Spacing[0] * float(header.mx);
  header.ylen = ( this->GetNumberOfDimensions() >= 2 ) ? m_Spacing[1] * float(header.my) : 1;
  header.zlen = ( this->GetNumberOfDimensions() >= 3 ) ? m_Spacing[2] * float(header.mz) : 1;

  header.xorg = m_Origin[0];
  header.yorg = ( this->GetNumberOfDimensions() >= 2 ) ? m_Origin[1] : 0;
  header.zorg = ( this->GetNumberOfDimensions() >= 3 ) ? m_Origin[2] : 0;

  // the SetHeader method is used to set the all the internal variable
  // of the header object correctly, and the data is verified
  m_MRCHeader = MRCHeaderObject::New();
  if ( !m_MRCHeader->SetHeader(&header) )
    {
    itkExceptionMacro(<< "Unexpected error setting header");
    }
}

void MRCImageIO::WriteImageInformation(const void *buffer)
{
  this->UpdateHeaderFromImageIO();

  this->UpdateHeaderWithMinMaxMean(buffer);

  std::ofstream file;

  this->OpenFileForWriting( file, m_FileName, true );

  // write the header
  const MRCHeaderObject::Header & header = m_MRCHeader->GetHeader();
  file.write(static_cast< const char * >( (const void *)&( header ) ), 1024);
}

void MRCImageIO
::UpdateHeaderWithMinMaxMean(const void *bufferBegin)
{
  // fixed types defined by header
  const MRCHeaderObject::Header & header = m_MRCHeader->GetHeader();

  switch ( header.mode )
    {
    case 0:
      {
      // scalar unsigned char
      this->UpdateHeaderWithMinMaxMean( static_cast< const unsigned char * >( bufferBegin ) );
      break;
      }
    case 1:
      {
      // scalar short
      this->UpdateHeaderWithMinMaxMean( static_cast< const short * >( bufferBegin ) );
      break;
      }
    case 2:
      {
      // scalar float
      this->UpdateHeaderWithMinMaxMean( static_cast< const float * >( bufferBegin ) );
      break;
      }
    case 3:
      {
      // complex short

      // What is the best way to map complex to float?
      // just set resonable values
      m_MRCHeader->m_Header.amin = -1.0f;
      m_MRCHeader->m_Header.amax = 1.0f;
      m_MRCHeader->m_Header.amean = 0.0f;
      break;
      }
    case 4:
      {
      // complex float

      // What is the best way to map complex to float?
      // just set resonable values
      m_MRCHeader->m_Header.amin = -1.0f;
      m_MRCHeader->m_Header.amax = 1.0f;
      m_MRCHeader->m_Header.amean = 0.0f;
      break;
      }
    case 6:
      {
      // scalar unsigned short
      this->UpdateHeaderWithMinMaxMean( static_cast< const unsigned short * >( bufferBegin ) );
      break;
      }
    case 16:
      {
      // RGB of unsigned char

      // just set resonable values
      m_MRCHeader->m_Header.amin = 0.0f;
      m_MRCHeader->m_Header.amax = 255.0f;
      m_MRCHeader->m_Header.amean = 127.5f;
      break;
      }
    default:
      {
      itkExceptionMacro(<< "Unrecognized mode");
      }
    }
}

void MRCImageIO
::Write(const void *buffer)
{
  if ( this->RequestedToStream() )
    {
    // we assume that GetActualNumberOfSplitsForWriting is called before
    // this methods and it will remove the file if a new header needs to
    // be written
    if ( !itksys::SystemTools::FileExists( m_FileName.c_str() ) )
      {
      this->WriteImageInformation(buffer);

      std::ofstream file;
      // open and stream write
      this->OpenFileForWriting( file, m_FileName, false );

      // write one byte at the end of the file to allocate (this is a
      // nifty trick which should not write the entire size of the file
      // just allocate it, if the system supports sparse files)
      std::streampos seekPos = this->GetImageSizeInBytes() + this->GetHeaderSize() - 1;
      file.seekp(seekPos, std::ios::cur);
      file.write("\0", 1);
      file.seekp(0);
      }
    else
      {
      if ( m_MRCHeader.IsNull() )
        {
        // need to determin the size of the header in the file by
        // reading the header into m_MRCHeader

        // we assume that GetActualNumberOfSplitsForWriting is called
        // to verify that the header and file is compatible with the
        // region to be written, but this call will still overwrite
        // the internal m_MRCHeader variable

        std::ifstream file;
        this->InternalReadImageInformation(file);
        }

      // TODO:
      // how are we suppose to update min, max mean? we could be
      // overwriting data, and we don't know what's already there if
      // we were pasting
      }

    std::ofstream file;
    // open and stream write
    this->OpenFileForWriting( file, m_FileName, false );

    this->StreamWriteBufferAsBinary(file, buffer);
    }

  else
    {
    // this will truncate file and write header
    this->WriteImageInformation(buffer);

    std::ofstream file;
    // open the file
    this->OpenFileForWriting( file, m_FileName, false);

    // seek pass the header
    std::streampos dataPos = static_cast< std::streampos >( this->GetHeaderSize() );
    file.seekp(dataPos, std::ios::beg);

    if ( file.fail() )
      {
      itkExceptionMacro(<< "Failed seeking to data position");
      }

    // read the image
    if ( !this->WriteBufferAsBinary( file, buffer, this->GetImageSizeInBytes() ) )
      {
      itkExceptionMacro(<< "Could not write file: " << m_FileName);
      }
    }
}

} // namespace itk
