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
#include "itkVoxBoCUBImageIO.h"
#include "itkIOCommon.h"
#include "itkExceptionObject.h"
#include "itkMetaDataObject.h"
#include "itkByteSwapper.h"
#include "itksys/SystemTools.hxx"
#include <iostream>
#include <sstream>
#include <list>
#include <string>
#include <math.h>
#include <time.h>

#include "itk_zlib.h"
#include "itkSpatialOrientationAdapter.h"

namespace itk
{
/**
 *
 * \class GenericCUBFileAdaptor
 *
 * \brief Reader and Writer for the VoxBo file format.
 *
 * A generic reader and writer object for VoxBo files. Basically it
 * provides uniform access to gzip and normal files
 *
 * \author Burstein, Pablo D.; Yushkevich, Paul; Gee, James C.
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://insight-journal.org/midas/handle.php?handle=1926/303
 *
 */
class GenericCUBFileAdaptor
{
public:
  GenericCUBFileAdaptor() {}
  virtual ~GenericCUBFileAdaptor() {}

  typedef ImageIOBase::SizeType SizeType;

  virtual unsigned char ReadByte() = 0;

  virtual void ReadData(void *data, SizeType bytes) = 0;

  virtual void WriteData(const void *data, SizeType bytes) = 0;

  std::string ReadHeader()
  {
    // Read everything up to the \f symbol
    std::ostringstream oss;
    unsigned char             byte = ReadByte();

    while ( byte != '\f' )
      {
      oss << byte;
      byte = ReadByte();
      }

    // Read the next byte
    unsigned char term = ReadByte();
    if ( term == '\r' )
      {
      term = ReadByte();
      }
    // Throw exception if term is not there
    if ( term != '\n' )
      {
      ExceptionObject exception;
      exception.SetDescription("Header is not terminated by newline.");
      throw exception;
      }

    // Return the header string
    return oss.str();
  }
};

/**
 * A reader for gzip files
 */

class CompressedCUBFileAdaptor:public GenericCUBFileAdaptor
{
public:
  CompressedCUBFileAdaptor(const char *file, const char *mode)
  {
    m_GzFile = gzopen(file, mode);
    if ( m_GzFile == ITK_NULLPTR )
      {
      ExceptionObject exception;
      exception.SetDescription("File cannot be accessed");
      throw exception;
      }
  }

  virtual ~CompressedCUBFileAdaptor()
  {
    if ( m_GzFile )
      {
      gzflush(m_GzFile, Z_FINISH);
      gzclose(m_GzFile);
      }
  }

  virtual unsigned char ReadByte() ITK_OVERRIDE
  {
    int byte = gzgetc(m_GzFile);

    if ( byte < 0 )
      {
      std::ostringstream oss;
      oss << "Error reading byte from file at position: " << gztell(m_GzFile);
      ExceptionObject exception;
      exception.SetDescription( oss.str().c_str() );
      throw exception;
      }
    return static_cast< unsigned char >( byte );
  }

  virtual void ReadData(void *data, SizeType bytes) ITK_OVERRIDE
  {
    if ( m_GzFile == ITK_NULLPTR )
      {
      ExceptionObject exception;
      exception.SetDescription("File cannot be read");
      throw exception;
      }

    unsigned int numberOfBytesToRead = Math::CastWithRangeCheck< unsigned int, SizeType >(bytes);
    SizeType     bread = gzread(m_GzFile, data, numberOfBytesToRead);
    if ( bread != bytes )
      {
      std::ostringstream oss;
      oss << "File size does not match header: "
          << bytes << " bytes requested but only "
          << bread << " bytes available!" << std::endl
          << "At file position " << gztell(m_GzFile);
      ExceptionObject exception;
      exception.SetDescription( oss.str().c_str() );
      throw exception;
      }
  }

  virtual void WriteData(const void *data, SizeType bytes) ITK_OVERRIDE
  {
    if ( m_GzFile == ITK_NULLPTR )
      {
      ExceptionObject exception;
      exception.SetDescription("File cannot be written");
      throw exception;
      }

    unsigned int numberOfBytesToWrite = Math::CastWithRangeCheck< unsigned int, SizeType >(bytes);
    SizeType     bwritten = gzwrite(m_GzFile, const_cast< void * >( data ), numberOfBytesToWrite);
    if ( bwritten != bytes )
      {
      ExceptionObject exception;
      exception.SetDescription("Could not write all bytes to file");
      std::cout << "Could not write all bytes to file" << std::endl;
      throw exception;
      }
    gzflush(m_GzFile, Z_SYNC_FLUSH);
  }

private:
  gzFile m_GzFile;
};

/**
 * A reader for non-gzip files
 */
class DirectCUBFileAdaptor:public GenericCUBFileAdaptor
{
public:
  DirectCUBFileAdaptor(const char *file, const char *mode)
  {
    m_File = fopen(file, mode);
    if ( !m_File )
      {
      ExceptionObject exception;
      exception.SetDescription("File cannot be read");
      throw exception;
      }
  }

  ~DirectCUBFileAdaptor()
  {
    if ( m_File )
      {
      fclose(m_File);
      }
  }

  virtual unsigned char ReadByte() ITK_OVERRIDE
  {
    int byte = fgetc(m_File);

    if ( byte == EOF )
      {
      std::ostringstream oss;
      oss << "Error reading byte from file at position: " << ::ftell(m_File);
      ExceptionObject exception;
      exception.SetDescription( oss.str().c_str() );
      throw exception;
      }
    return static_cast< unsigned char >( byte );
  }

  virtual void ReadData(void *data, SizeType bytes) ITK_OVERRIDE
  {
    if ( m_File == ITK_NULLPTR )
      {
      ExceptionObject exception;
      exception.SetDescription("File cannot be read");
      throw exception;
      }

    const SizeValueType numberOfBytesToRead =  Math::CastWithRangeCheck< SizeValueType, SizeType >(bytes);
    SizeType     bread = fread(data, NumericTraits< SizeValueType >::OneValue(), numberOfBytesToRead, m_File);
    if ( bread != bytes )
      {
      std::ostringstream oss;
      oss << "File size does not match header: "
          << bytes << " bytes requested but only "
          << bread << " bytes available!" << std::endl
          << "At file position " << ftell(m_File);
      ExceptionObject exception;
      exception.SetDescription( oss.str().c_str() );
      throw exception;
      }
  }

  virtual void WriteData(const void *data, SizeType bytes) ITK_OVERRIDE
  {
    if ( m_File == ITK_NULLPTR )
      {
      ExceptionObject exception;
      exception.SetDescription("File cannot be written");
      throw exception;
      }

    const SizeValueType numberOfBytesToWrite =  Math::CastWithRangeCheck< SizeValueType, SizeType >(bytes);
    SizeType     bwritten = fwrite(data, NumericTraits< SizeValueType >::OneValue(), numberOfBytesToWrite, m_File);
    if ( bwritten != bytes )
      {
      ExceptionObject exception;
      exception.SetDescription("Could not write all bytes to file");
      throw exception;
      }
  }

private:
  FILE *m_File;
};

/**
 * \class VoxBoCUBImageIOSwapHelper
 *
 * \brief A swap helper class, used to perform swapping for any input
 * data type.
 *
 */
template< typename TPixel >
class VoxBoCUBImageIOSwapHelper
{
public:
  typedef ImageIOBase::ByteOrder      ByteOrder;
  typedef ImageIOBase::BufferSizeType BufferSizeType;

  static void SwapIfNecessary(
    void *buffer, BufferSizeType numberOfBytes, ByteOrder dataByteOrder)
  {
    if ( dataByteOrder == ImageIOBase::LittleEndian )
      {
      ByteSwapper< TPixel >::SwapRangeFromSystemToLittleEndian(
        (TPixel *)buffer, numberOfBytes / sizeof( TPixel ) );
      }
    else if ( dataByteOrder == ImageIOBase::BigEndian )
      {
      ByteSwapper< TPixel >::SwapRangeFromSystemToBigEndian(
        (TPixel *)buffer, numberOfBytes / sizeof( TPixel ) );
      }
  }
};

// Strings
const char *VoxBoCUBImageIO:: m_VB_IDENTIFIER_SYSTEM = "VB98";
const char *VoxBoCUBImageIO:: m_VB_IDENTIFIER_FILETYPE = "CUB1";
const char *VoxBoCUBImageIO:: m_VB_DIMENSIONS = "VoxDims(XYZ)";
const char *VoxBoCUBImageIO:: m_VB_SPACING = "VoxSizes(XYZ)";
const char *VoxBoCUBImageIO:: m_VB_ORIGIN = "Origin(XYZ)";
const char *VoxBoCUBImageIO:: m_VB_DATATYPE = "DataType";
const char *VoxBoCUBImageIO:: m_VB_BYTEORDER = "Byteorder";
const char *VoxBoCUBImageIO:: m_VB_ORIENTATION = "Orientation";
const char *VoxBoCUBImageIO:: m_VB_BYTEORDER_MSB = "msbfirst";
const char *VoxBoCUBImageIO:: m_VB_BYTEORDER_LSB = "lsbfirst";
const char *VoxBoCUBImageIO:: m_VB_DATATYPE_BYTE = "Byte";
const char *VoxBoCUBImageIO:: m_VB_DATATYPE_INT = "Integer";
const char *VoxBoCUBImageIO:: m_VB_DATATYPE_FLOAT = "Float";
const char *VoxBoCUBImageIO:: m_VB_DATATYPE_DOUBLE = "Double";

/** Constructor */
VoxBoCUBImageIO::VoxBoCUBImageIO()
{
  InitializeOrientationMap();
  m_ByteOrder = BigEndian;
  m_Reader = ITK_NULLPTR;
  m_Writer = ITK_NULLPTR;
}

/** Destructor */
VoxBoCUBImageIO::~VoxBoCUBImageIO()
{
  delete m_Reader;
  delete m_Writer;
}

GenericCUBFileAdaptor *
VoxBoCUBImageIO::CreateReader(const char *filename)
{
  try
    {
    bool compressed;
    if ( CheckExtension(filename, compressed) )
      {
      if ( compressed )
        {
        return new CompressedCUBFileAdaptor(filename, "rb");
        }
      else
        {
        return new DirectCUBFileAdaptor(filename, "rb");
        }
      }
    else
      {
      return ITK_NULLPTR;
      }
    }
  catch ( ... )
    {
    return ITK_NULLPTR;
    }
}

GenericCUBFileAdaptor *
VoxBoCUBImageIO::CreateWriter(const char *filename)
{
  try
    {
    bool compressed;
    if ( CheckExtension(filename, compressed) )
      {
      if ( compressed )
        {
        return new CompressedCUBFileAdaptor(filename, "wb");
        }
      else
        {
        return new DirectCUBFileAdaptor(filename, "wb");
        }
      }
    else
      {
      return ITK_NULLPTR;
      }
    }
  catch ( ... )
    {
    return ITK_NULLPTR;
    }
}

bool VoxBoCUBImageIO::CanReadFile(const char *filename)
{
  // First check if the file can be read
  GenericCUBFileAdaptor *reader = CreateReader(filename);

  if ( reader == ITK_NULLPTR )
    {
    itkDebugMacro(<< "The file is not a valid CUB file");
    return false;
    }

  // Now check the content
  bool iscub = true;
  try
    {
    // Get the header
    std::istringstream iss( reader->ReadHeader() );

    // Read the first two words
    std::string word;

    // Read the first line from the file
    iss >> word;
    if ( word != m_VB_IDENTIFIER_SYSTEM )
      {
      iscub = false;
      }

    // Read the second line
    iss >> word;
    if ( word != m_VB_IDENTIFIER_FILETYPE )
      {
      iscub = false;
      }
    }
  catch ( ... )
    {
    iscub = false;
    }

  delete reader;
  return iscub;
}

bool VoxBoCUBImageIO::CanWriteFile(const char *name)
{
  bool compressed;

  return CheckExtension(name, compressed);
}

void VoxBoCUBImageIO::Read(void *buffer)
{
  if ( m_Reader == ITK_NULLPTR )
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("File cannot be read");
    throw exception;
    }

  BufferSizeType numberOfBytesToRead =
    Math::CastWithRangeCheck< BufferSizeType, SizeType >( this->GetImageSizeInBytes() );
  m_Reader->ReadData(buffer, numberOfBytesToRead);
  this->SwapBytesIfNecessary(buffer, numberOfBytesToRead);
}

/**
 *  Read Information about the VoxBoCUB file
 *  and put the cursor of the stream just before the first data pixel
 */
void VoxBoCUBImageIO::ReadImageInformation()
{
  // Make sure there is no other reader
  delete m_Reader;

  // Create a reader
  m_Reader = CreateReader( m_FileName.c_str() );
  if ( m_Reader == ITK_NULLPTR )
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("File cannot be read");
    throw exception;
    }

  // Set the number of dimensions to three
  SetNumberOfDimensions(3);

  // Read the file header
  std::istringstream issHeader( m_Reader->ReadHeader() );

  // Read every string in the header. Parse the strings that are special
  while ( issHeader.good() )
    {
    // Read a line from the stream
    char linebuffer[512];
    issHeader.getline(linebuffer, 512);

    // Get the key string
    std::istringstream iss(linebuffer);
    std::string        key;

    // Read the key and strip the colon from it
    iss >> key;

    const std::string::size_type keysize = key.size();

    if ( ( keysize > 0 ) && ( key[key.size() - 1] == ':' ) )
      {
      // Strip the colon off the key
      key = key.substr(0, key.size() - 1);

      // Check if this is a relevant key
      if ( key == m_VB_DIMENSIONS )
        {
        iss >> m_Dimensions[0];
        iss >> m_Dimensions[1];
        iss >> m_Dimensions[2];
        }

      else if ( key == m_VB_SPACING )
        {
        iss >> m_Spacing[0];
        iss >> m_Spacing[1];
        iss >> m_Spacing[2];
        }

      else if ( key == m_VB_ORIGIN )
        {
        double ox, oy, oz;
        iss >> ox; iss >> oy; iss >> oz;
        m_Origin[0] = -ox * m_Spacing[0];
        m_Origin[1] = -oy * m_Spacing[1];
        m_Origin[2] = -oz * m_Spacing[2];
        }

      else if ( key == m_VB_DATATYPE )
        {
        std::string type;
        iss >> type;
        m_PixelType = SCALAR;
        if ( type == m_VB_DATATYPE_BYTE )
          {
          m_ComponentType = UCHAR;
          }
        else if ( type == m_VB_DATATYPE_INT )
          {
          m_ComponentType = USHORT;
          }
        else if ( type == m_VB_DATATYPE_FLOAT )
          {
          m_ComponentType = FLOAT;
          }
        else if ( type == m_VB_DATATYPE_DOUBLE )
          {
          m_ComponentType = DOUBLE;
          }
        }

      else if ( key == m_VB_BYTEORDER )
        {
        std::string type;
        iss >> type;
        if ( type == m_VB_BYTEORDER_MSB )
          {
          SetByteOrderToBigEndian();
          }
        else if ( type == m_VB_BYTEORDER_LSB )
          {
          SetByteOrderToLittleEndian();
          }
        else
          {
          ExceptionObject exception(__FILE__, __LINE__);
          exception.SetDescription("Unknown byte order constant");
          throw exception;
          }
        }

      else if ( key == m_VB_ORIENTATION )
        {
        std::string code;
        iss >> code;

        // Set the orientation code in the data dictionary
        OrientationMap::const_iterator it = m_OrientationMap.find(code);
        if ( it != m_OrientationMap.end() )
          {
          //NOTE:  The itk::ImageIOBase direction is a std::vector<std::vector > >, and threeDDirection is a 3x3 matrix
          itk::SpatialOrientationAdapter soAdaptor;
          itk::SpatialOrientationAdapter::DirectionType threeDDirection=soAdaptor.ToDirectionCosines(it->second);
          this->m_Direction[0][0]=threeDDirection[0][0];
          this->m_Direction[0][1]=threeDDirection[0][1];
          this->m_Direction[0][2]=threeDDirection[0][2];
          this->m_Direction[1][0]=threeDDirection[1][0];
          this->m_Direction[1][1]=threeDDirection[1][1];
          this->m_Direction[1][2]=threeDDirection[1][2];
          this->m_Direction[2][0]=threeDDirection[2][0];
          this->m_Direction[2][1]=threeDDirection[2][1];
          this->m_Direction[2][2]=threeDDirection[2][2];
          }
        }

      else
        {
        // Encode the right hand side of the string in the meta-data dic
        std::string               word;
        std::ostringstream oss;
        while ( iss >> word )
          {
          if ( oss.str().size() )
            {
            oss << " ";
            }
          oss << word;
          }
        MetaDataDictionary & dic = this->GetMetaDataDictionary();
        EncapsulateMetaData< std::string >( dic, key, oss.str() );
        }
      }
    }
}

void
VoxBoCUBImageIO
::WriteImageInformation(void)
{
  if ( m_Writer == ITK_NULLPTR )
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("File cannot be read");
    throw exception;
    }

  // Check that the number of dimensions is correct
  if ( GetNumberOfDimensions() != 3 )
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("Unsupported number of dimensions");
    throw exception;
    }

  // Put together a header
  std::ostringstream header;

  // Write the identifiers
  header << m_VB_IDENTIFIER_SYSTEM << std::endl;
  header << m_VB_IDENTIFIER_FILETYPE << std::endl;

  // Write the data type
  switch ( m_ComponentType )
    {
    case CHAR:
    case UCHAR:
      header << m_VB_DATATYPE << ":\t" << m_VB_DATATYPE_BYTE << std::endl;
      break;
    case SHORT:
    case USHORT:
      header << m_VB_DATATYPE << ":\t" << m_VB_DATATYPE_INT << std::endl;
      break;
    case FLOAT:
      header << m_VB_DATATYPE << ":\t" << m_VB_DATATYPE_FLOAT << std::endl;
      break;
    case DOUBLE:
      header << m_VB_DATATYPE << ":\t" << m_VB_DATATYPE_DOUBLE << std::endl;
      break;
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Unsupported pixel component type");
      throw exception;
    }

  // Write the image dimensions
  header << m_VB_DIMENSIONS << ":\t"
         << m_Dimensions[0] << "\t"
         << m_Dimensions[1] << "\t"
         << m_Dimensions[2] << std::endl;

  // Write the spacing
  header << m_VB_SPACING << ":\t"
         << m_Spacing[0] << "\t"
         << m_Spacing[1] << "\t"
         << m_Spacing[2] << std::endl;

  // Write the origin (have to convert to bytes)

  double x = -m_Origin[0] / m_Spacing[0];
  double y = -m_Origin[1] / m_Spacing[1];
  double z = -m_Origin[2] / m_Spacing[2];
  header << m_VB_ORIGIN << ":\t"
         << ( ( x >= 0 ) ? (int)( x + .5 ) : (int)( x - .5 ) ) << "\t"
         << ( ( y >= 0 ) ? (int)( y + .5 ) : (int)( y - .5 ) ) << "\t"
         << ( ( z >= 0 ) ? (int)( z + .5 ) : (int)( z - .5 ) ) << std::endl;

  // Write the byte order
  header << m_VB_BYTEORDER << ":\t"
         << ( ( ByteSwapper< short >::SystemIsBigEndian() ) ? m_VB_BYTEORDER_MSB : m_VB_BYTEORDER_LSB ) << std::endl;

  // Write the orientation code
  //NOTE:  The itk::ImageIOBase direction is a std::vector<std::vector > >, and threeDDirection is a 3x3 matrix
  itk::SpatialOrientationAdapter soAdaptor;
  itk::SpatialOrientationAdapter::DirectionType threeDDirection;
  threeDDirection[0][0]=this->m_Direction[0][0];
  threeDDirection[0][1]=this->m_Direction[0][1];
  threeDDirection[0][2]=this->m_Direction[0][2];
  threeDDirection[1][0]=this->m_Direction[1][0];
  threeDDirection[1][1]=this->m_Direction[1][1];
  threeDDirection[1][2]=this->m_Direction[1][2];
  threeDDirection[2][0]=this->m_Direction[2][0];
  threeDDirection[2][1]=this->m_Direction[2][1];
  threeDDirection[2][2]=this->m_Direction[2][2];
  OrientationFlags     oflag = soAdaptor.FromDirectionCosines(threeDDirection);
    {
    InverseOrientationMap::const_iterator it = m_InverseOrientationMap.find(oflag);
    if ( it != m_InverseOrientationMap.end() )
      {
      header << m_VB_ORIENTATION << ":\t" << it->second << std::endl;
      }
    }

  //Add CUB specific parameters to header from MetaDictionary
  MetaDataDictionary & dic = GetMetaDataDictionary();
  std::vector< std::string > keys = dic.GetKeys();
  std::string                word;
  for ( SizeValueType i = 0; i < keys.size(); i++ )
    {
    const std::string & key = keys[i];
    ExposeMetaData< std::string >(dic, key, word);
    if ( !strcmp(key.c_str(), "resample_date") )
      {
      time_t rawtime;
      time(&rawtime);
      word = ctime(&rawtime);
      header << key << ":\t" << word;
      }
    else
      {
      header << key << ":\t" << word << std::endl;
      }
    }
  // Write the terminating characters
  header << "\f\n";

  // Write the header to the file as data
  m_Writer->WriteData( header.str().c_str(), header.str().size() );
}

/** The write function is not implemented */
void
VoxBoCUBImageIO
::Write(const void *buffer)
{
  m_Writer = CreateWriter( m_FileName.c_str() );
  WriteImageInformation();
  m_Writer->WriteData( buffer, this->GetImageSizeInBytes() );
  delete m_Writer;
  m_Writer = ITK_NULLPTR;
}

/** Print Self Method */
void VoxBoCUBImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "PixelType " << m_PixelType << "\n";
}

bool VoxBoCUBImageIO::CheckExtension(const char *filename, bool & isCompressed)
{
  std::string fname = filename;

  if ( fname == "" )
    {
    itkDebugMacro(<< "No filename specified.");
    return false;
    }

  bool extensionFound = false;
  isCompressed = false;

  std::string::size_type giplPos = fname.rfind(".cub");
  if ( ( giplPos != std::string::npos )
       && ( giplPos == fname.length() - 4 ) )
    {
    extensionFound = true;
    }

  giplPos = fname.rfind(".cub.gz");
  if ( ( giplPos != std::string::npos )
       && ( giplPos == fname.length() - 7 ) )
    {
    extensionFound = true;
    isCompressed = true;
    }

  return extensionFound;
}

void
VoxBoCUBImageIO
::InitializeOrientationMap()
{
  m_OrientationMap["RIP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP;
  m_OrientationMap["LIP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP;
  m_OrientationMap["RSP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP;
  m_OrientationMap["LSP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP;
  m_OrientationMap["RIA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA;
  m_OrientationMap["LIA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA;
  m_OrientationMap["RSA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA;
  m_OrientationMap["LSA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA;
  m_OrientationMap["IRP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP;
  m_OrientationMap["ILP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP;
  m_OrientationMap["SRP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP;
  m_OrientationMap["SLP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP;
  m_OrientationMap["IRA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA;
  m_OrientationMap["ILA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA;
  m_OrientationMap["SRA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA;
  m_OrientationMap["SLA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA;
  m_OrientationMap["RPI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI;
  m_OrientationMap["LPI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI;
  m_OrientationMap["RAI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI;
  m_OrientationMap["LAI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI;
  m_OrientationMap["RPS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS;
  m_OrientationMap["LPS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS;
  m_OrientationMap["RAS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS;
  m_OrientationMap["LAS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS;
  m_OrientationMap["PRI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI;
  m_OrientationMap["PLI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI;
  m_OrientationMap["ARI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI;
  m_OrientationMap["ALI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI;
  m_OrientationMap["PRS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS;
  m_OrientationMap["PLS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS;
  m_OrientationMap["ARS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS;
  m_OrientationMap["ALS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS;
  m_OrientationMap["IPR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR;
  m_OrientationMap["SPR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR;
  m_OrientationMap["IAR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR;
  m_OrientationMap["SAR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR;
  m_OrientationMap["IPL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL;
  m_OrientationMap["SPL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL;
  m_OrientationMap["IAL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL;
  m_OrientationMap["SAL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL;
  m_OrientationMap["PIR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR;
  m_OrientationMap["PSR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR;
  m_OrientationMap["AIR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR;
  m_OrientationMap["ASR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR;
  m_OrientationMap["PIL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL;
  m_OrientationMap["PSL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL;
  m_OrientationMap["AIL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL;
  m_OrientationMap["ASL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL;

  OrientationMap::const_iterator it;
  for ( it = m_OrientationMap.begin(); it != m_OrientationMap.end(); ++it )
    {
    m_InverseOrientationMap[it->second] = it->first;
    }
}

void
VoxBoCUBImageIO
::SwapBytesIfNecessary(void *buffer, BufferSizeType numberOfBytes)
{
  if ( m_ComponentType == CHAR )
    {
    VoxBoCUBImageIOSwapHelper< char >::SwapIfNecessary(
      buffer, numberOfBytes, m_ByteOrder);
    }
  else if ( m_ComponentType == UCHAR )
    {
    VoxBoCUBImageIOSwapHelper< unsigned char >::SwapIfNecessary(
      buffer, numberOfBytes, m_ByteOrder);
    }
  else if ( m_ComponentType == SHORT )
    {
    VoxBoCUBImageIOSwapHelper< short >::SwapIfNecessary(
      buffer, numberOfBytes, m_ByteOrder);
    }
  else if ( m_ComponentType == USHORT )
    {
    VoxBoCUBImageIOSwapHelper< unsigned short >::SwapIfNecessary(
      buffer, numberOfBytes, m_ByteOrder);
    }
  else if ( m_ComponentType == INT )
    {
    VoxBoCUBImageIOSwapHelper< int >::SwapIfNecessary(
      buffer, numberOfBytes, m_ByteOrder);
    }
  else if ( m_ComponentType == UINT )
    {
    VoxBoCUBImageIOSwapHelper< unsigned int >::SwapIfNecessary(
      buffer, numberOfBytes, m_ByteOrder);
    }
  else if ( m_ComponentType == LONG )
    {
    VoxBoCUBImageIOSwapHelper< long >::SwapIfNecessary(
      buffer, numberOfBytes, m_ByteOrder);
    }
  else if ( m_ComponentType == ULONG )
    {
    VoxBoCUBImageIOSwapHelper< unsigned long >::SwapIfNecessary(
      buffer, numberOfBytes, m_ByteOrder);
    }
  else if ( m_ComponentType == FLOAT )
    {
    VoxBoCUBImageIOSwapHelper< float >::SwapIfNecessary(
      buffer, numberOfBytes, m_ByteOrder);
    }
  else if ( m_ComponentType == DOUBLE )
    {
    VoxBoCUBImageIOSwapHelper< double >::SwapIfNecessary(
      buffer, numberOfBytes, m_ByteOrder);
    }
  else
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("Pixel Type Unknown");
    throw exception;
    }
}
} // end namespace itk
