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
#include "itkBruker2DSEQImageIO.h"
#include "itkIOCommon.h"
#include "itkByteSwapper.h"
#include "itkMetaDataObject.h"
#include "itksys/SystemTools.hxx"
#include <fstream>

/**
 * \author Don C. Bigler
 *         The Pennsylvania State University 2005
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://insight-journal.org/midas/handle.php?handle=1926/1381
 *
 */

namespace itk
{
const char *const RECO_BYTE_ORDER = "##$RECO_byte_order";
const char *const RECO_FOV = "##$RECO_fov";
const char *const RECO_SIZE = "##$RECO_size";
const char *const RECO_WORDTYPE = "##$RECO_wordtype";
const char *const RECO_IMAGE_TYPE = "##$RECO_image_type";
const char *const RECO_TRANSPOSITION = "##$RECO_transposition";
const char *const ACQ_DIM = "##$ACQ_dim";
const char *const NI = "##$NI";
const char *const NR = "##$NR";
const char *const ACQ_SLICE_THICK = "##$ACQ_slice_thick";
const char *const NECHOES = "##$NECHOES";
const char *const ACQ_SLICE_SEPN = "##$ACQ_slice_sepn";
const char *const ACQ_SLICE_SEPN_MODE = "##$ACQ_slice_sepn_mode";
const char *const ACQ_ECHO_TIME = "##$ACQ_echo_time";
const char *const ACQ_REPETITION_TIME = "##$ACQ_repetition_time";
const char *const ACQ_INVERSION_TIME = "##$ACQ_inversion_time";

#define FORWARDSLASH_DIRECTORY_SEPARATOR   '/'

#define RECO_FILE         "reco"
#define ACQP_FILE        "acqp"
#define DTHREEPROC_FILE      "d3proc"
#define RECO_byte_order      "##$RECO_byte_order="
#define  BRUKER_LITTLE_ENDIAN  "littleEndian"
#define  BRUKER_BIG_ENDIAN    "bigEndian"
#define RECO_fov        "##$RECO_fov=("
#define RECO_size        "##$RECO_size=("
#define RECO_wordtype      "##$RECO_wordtype="
#define RECO_image_type      "##$RECO_image_type="
#define RECO_transposition    "##$RECO_transposition=("
#define MAGNITUDE_IMAGE      "MAGNITUDE_IMAGE"
#define REAL_IMAGE        "REAL_IMAGE"
#define IMAGINARY_IMAGE      "IMAGINARY_IMAGE"
#define COMPLEX_IMAGE      "COMPLEX_IMAGE"
#define PHASE_IMAGE        "PHASE_IMAGE"
#define IR_IMAGE        "IR_IMAGE"
#define ACQ_dim          "##$ACQ_dim="
#define Ni            "##$NI="
#define Nr            "##$NR="
#define Nechoes          "##$NECHOES="
#define ACQ_slice_thick      "##$ACQ_slice_thick="
#define ACQ_slice_sepn      "##$ACQ_slice_sepn=("
#define ACQ_slice_sepn_mode    "##$ACQ_slice_sepn_mode="
#define BRUKER_SIGNED_CHAR    "_8BIT_SGN_INT"
#define BRUKER_UNSIGNED_CHAR  "_8BIT_UNSGN_INT"
#define BRUKER_SIGNED_SHORT    "_16BIT_SGN_INT"
#define BRUKER_SIGNED_INT    "_32BIT_SGN_INT"
#define BRUKER_FLOAT      "_32BIT_FLOAT"
#define ACQ_echo_time      "##$ACQ_echo_time=("
#define ACQ_repetition_time    "##$ACQ_repetition_time=("
#define ACQ_inversion_time    "##$ACQ_inversion_time=("
#define ACQ_grad_matrix      "##$ACQ_grad_matrix=("
#define DATTYPE          "##$DATTYPE="
#define IM_SIX          "##$IM_SIX="
#define IM_SIY          "##$IM_SIY="
#define IM_SIZ          "##$IM_SIZ="
#define IP_CHAR          "ip_char"
#define IP_SHORT        "ip_short"
#define IP_INT          "ip_int"

void
Bruker2DSEQImageIO::SwapBytesIfNecessary(void *buffer,
                                         SizeValueType numberOfPixels)
{
  if ( m_ByteOrder == LittleEndian )
    {
    switch ( this->m_ComponentType )
      {
      case CHAR:
        ByteSwapper< char >::SwapRangeFromSystemToLittleEndian
          ( (char *)buffer, numberOfPixels );
        break;
      case UCHAR:
        ByteSwapper< unsigned char >::SwapRangeFromSystemToLittleEndian
          ( (unsigned char *)buffer, numberOfPixels );
        break;
      case SHORT:
        ByteSwapper< short >::SwapRangeFromSystemToLittleEndian
          ( (short *)buffer, numberOfPixels );
        break;
      case USHORT:
        ByteSwapper< unsigned short >::SwapRangeFromSystemToLittleEndian
          ( (unsigned short *)buffer, numberOfPixels );
        break;
      case INT:
        ByteSwapper< int >::SwapRangeFromSystemToLittleEndian
          ( (int *)buffer, numberOfPixels );
        break;
      case UINT:
        ByteSwapper< unsigned int >::SwapRangeFromSystemToLittleEndian
          ( (unsigned int *)buffer, numberOfPixels );
        break;
      case LONG:
        ByteSwapper< long >::SwapRangeFromSystemToLittleEndian
          ( (long *)buffer, numberOfPixels );
        break;
      case ULONG:
        ByteSwapper< unsigned long >::SwapRangeFromSystemToLittleEndian
          ( (unsigned long *)buffer, numberOfPixels );
        break;
      case FLOAT:
        ByteSwapper< float >::SwapRangeFromSystemToLittleEndian
          ( (float *)buffer, numberOfPixels );
        break;
      case DOUBLE:
        ByteSwapper< double >::SwapRangeFromSystemToLittleEndian
          ( (double *)buffer, numberOfPixels );
        break;
      default:
        ExceptionObject exception(__FILE__, __LINE__,
                                  "Component Type Unknown",
                                  ITK_LOCATION);
        throw exception;
      }
    }
  else
    {
    switch ( this->m_ComponentType )
      {
      case CHAR:
        ByteSwapper< char >::SwapRangeFromSystemToBigEndian
          ( (char *)buffer, numberOfPixels );
        break;
      case UCHAR:
        ByteSwapper< unsigned char >::SwapRangeFromSystemToBigEndian
          ( (unsigned char *)buffer, numberOfPixels );
        break;
      case SHORT:
        ByteSwapper< short >::SwapRangeFromSystemToBigEndian
          ( (short *)buffer, numberOfPixels );
        break;
      case USHORT:
        ByteSwapper< unsigned short >::SwapRangeFromSystemToBigEndian
          ( (unsigned short *)buffer, numberOfPixels );
        break;
      case INT:
        ByteSwapper< int >::SwapRangeFromSystemToBigEndian
          ( (int *)buffer, numberOfPixels );
        break;
      case UINT:
        ByteSwapper< unsigned int >::SwapRangeFromSystemToBigEndian
          ( (unsigned int *)buffer, numberOfPixels );
        break;
      case LONG:
        ByteSwapper< long >::SwapRangeFromSystemToBigEndian
          ( (long *)buffer, numberOfPixels );
        break;
      case ULONG:
        ByteSwapper< unsigned long >::SwapRangeFromSystemToBigEndian
          ( (unsigned long *)buffer, numberOfPixels );
        break;
      case FLOAT:
        ByteSwapper< float >::SwapRangeFromSystemToBigEndian
          ( (float *)buffer, numberOfPixels );
        break;
      case DOUBLE:
        ByteSwapper< double >::SwapRangeFromSystemToBigEndian
          ( (double *)buffer, numberOfPixels );
        break;
      default:
        ExceptionObject exception(__FILE__, __LINE__,
                                  "Component Type Unknown",
                                  ITK_LOCATION);
        throw exception;
      }
    }
}

Bruker2DSEQImageIO::Bruker2DSEQImageIO()
{
  //by default, only have 3 dimensions
  this->SetNumberOfDimensions(3);
  this->m_PixelType         = SCALAR;
  this->m_ComponentType     = CHAR;
  this->SetNumberOfComponents(1);

  // Set m_MachineByteOrder to the ByteOrder of the machine
  // Start out with file byte order == system byte order
  // this will be changed if we're reading a file to whatever
  // the file actually contains.
  if ( ByteSwapper< int >::SystemIsBigEndian() )
    {
    this->m_MachineByteOrder = this->m_ByteOrder = BigEndian;
    }
  else
    {
    this->m_MachineByteOrder = this->m_ByteOrder = LittleEndian;
    }
}

Bruker2DSEQImageIO::~Bruker2DSEQImageIO()
{
  // Left blank on purpose.
}

void Bruker2DSEQImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

void Bruker2DSEQImageIO::Read(void *buffer)
{
  unsigned int       dim;
  const unsigned int dimensions = this->GetNumberOfDimensions();
  unsigned int       numberOfPixels = 1;
  char *const        p = static_cast< char * >( buffer );

  for ( dim = 0; dim < dimensions; dim++ )
    {
    numberOfPixels *= this->m_Dimensions[dim];
    }

  /* Get the 2dseq filename */
  /* Use the same code as CanReadFile() */
  std::string file2Dseq =
    itksys::SystemTools::CollapseFullPath( this->m_FileName.c_str() );
  itksys::SystemTools::ConvertToUnixSlashes(file2Dseq);
  /* Try to open the file */
  std::ifstream twodseq_InputStream;
  twodseq_InputStream.imbue( std::locale::classic() );
  twodseq_InputStream.open(file2Dseq.c_str(), std::ios::in | std::ios::binary);

  if ( twodseq_InputStream.fail() )
    {
    std::ostringstream message;
    message << "The Brucker2DSEG Data File can not be opened. "
            << "The following file was attempted:" << std::endl
            << file2Dseq;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }

  twodseq_InputStream.read( p, Math::CastWithRangeCheck< std::streamsize, SizeType >( this->GetImageSizeInBytes() ) );

  if ( twodseq_InputStream.fail() )
    {
    std::ostringstream message;
    message << "The Brucker2DSEG Data File can not be read. "
            << "The following file was attempted:" << std::endl
            << file2Dseq;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }
  twodseq_InputStream.close();
  this->SwapBytesIfNecessary(buffer, numberOfPixels);
}

bool Bruker2DSEQImageIO::CanReadFile(const char *FileNameToRead)
{
  std::string file2Dseq = itksys::SystemTools::CollapseFullPath(FileNameToRead);

  itksys::SystemTools::ConvertToUnixSlashes(file2Dseq);
  std::string path = itksys::SystemTools::GetFilenamePath(file2Dseq);
  std::string filereco = path + FORWARDSLASH_DIRECTORY_SEPARATOR;
  filereco += RECO_FILE;
  std::string filed3proc = path + FORWARDSLASH_DIRECTORY_SEPARATOR;
  filed3proc += DTHREEPROC_FILE;
  std::vector< std::string > pathComponents;
  itksys::SystemTools::SplitPath(path.c_str(), pathComponents);
  if ( pathComponents.size() < 3 )
    {
    return false;
    }
  // Go two directories up.
  pathComponents.pop_back(); pathComponents.pop_back();
  path = itksys::SystemTools::JoinPath(pathComponents);
  std::string fileacqp = path + FORWARDSLASH_DIRECTORY_SEPARATOR;
  fileacqp += ACQP_FILE;
  std::string            readFileBufferString = "";
  char                   readFileBuffer[512] = "";
  std::string::size_type index;
  SizeValueType          length2DSEQ = 0;
  SizeValueType          calcLength = 1;

  // Does the '2dseq' file exist?
  if ( !itksys::SystemTools::FileExists( file2Dseq.c_str() ) )
    {
    return false;
    }

  // get length of file in bytes:
  length2DSEQ = itksys::SystemTools::FileLength( file2Dseq.c_str() );
  //std::cout << "length2DSEQ = " << length2DSEQ << std::endl;

  // Check reco for existence.
  std::ifstream reco_InputStream;
  reco_InputStream.open(filereco.c_str(),
                        std::ios::in);
  if ( reco_InputStream.fail() )
    {
    return false;
    }
  reco_InputStream.imbue( std::locale::classic() );
  while ( !reco_InputStream.eof() )
    {
    reco_InputStream.getline( readFileBuffer, sizeof( readFileBuffer ) );
    readFileBufferString = readFileBuffer;

    // Get the image data type.
    index = readFileBufferString.find(RECO_wordtype);
    if ( index != std::string::npos )
      {
      std::string tempString = RECO_wordtype;
      std::string dattypeString =
        readFileBufferString.substr( index + tempString.length() );
      if ( dattypeString.find(BRUKER_SIGNED_CHAR) != std::string::npos )
        {
        calcLength *= (SizeValueType)sizeof( char );
        }
      else if ( dattypeString.find(BRUKER_UNSIGNED_CHAR) != std::string::npos )
        {
        calcLength *= (SizeValueType)sizeof( unsigned char );
        }
      else if ( dattypeString.find(BRUKER_SIGNED_SHORT) != std::string::npos )
        {
        calcLength *= (SizeValueType)sizeof( short );
        }
      else if ( dattypeString.find(BRUKER_SIGNED_INT) != std::string::npos )
        {
        calcLength *= (SizeValueType)sizeof( int );
        }
      else if ( dattypeString.find(BRUKER_FLOAT) != std::string::npos )
        {
        calcLength *= (SizeValueType)sizeof( float );
        }
      else
        {
        reco_InputStream.close();
        return false;
        }
      //std::cout << "calcLength = " << calcLength << std::endl;
      }
    }
  reco_InputStream.close();

  // Check acqp for existence.
  //std::cout << "fileacqp = " << fileacqp << std::endl;
  if ( !itksys::SystemTools::FileExists( fileacqp.c_str() ) )
    {
    return false;
    }

  // Check d3proc for existence.
  std::ifstream d3proc_InputStream;
  d3proc_InputStream.open(filed3proc.c_str(),
                          std::ios::in);
  if ( d3proc_InputStream.fail() )
    {
    return false;
    }
  d3proc_InputStream.imbue( std::locale::classic() );
  while ( !d3proc_InputStream.eof() )
    {
    d3proc_InputStream.getline( readFileBuffer, sizeof( readFileBuffer ) );
    readFileBufferString = readFileBuffer;

    // Get the image data type.
    // This method is not a reliable method for determining the data type.
    // Using RECO_wordtype instead.
    //index = readFileBufferString.find(DATTYPE);
    //if( index != std::string::npos )
    //  {
    //  std::string tempString = DATTYPE;
    //  std::string dattypeString =
    //  readFileBufferString.substr(index+tempString.length());
    //  if( dattypeString.find(IP_CHAR) != std::string::npos )
    //    {
    //    calcLength *= 1;
    //    }
    //  else if( (dattypeString.find(IP_SHORT) != std::string::npos) ||
    //    (dattypeString.find("3") != std::string::npos) )
    //    {
    //    calcLength *= 2;
    //    }
    //    else if( (dattypeString.find(IP_INT) != std::string::npos) ||
    //      (dattypeString.find("5") != std::string::npos) )
    //    {
    //    calcLength *= 4;
    //    }
    //    else
    //    {
    //    std::cerr << "FIX ME: Unknown DATTYPE = ";
    //    std::cerr << dattypeString << std::endl;
    //    }
    //  std::cout << "calcLength = " << calcLength << std::endl;
    //}

    // Get the x size.
    index = readFileBufferString.find(IM_SIX);
    if ( index != std::string::npos )
      {
      SizeValueType      xDim = 0;
      std::string        tempString = IM_SIX;
      std::istringstream im_sixString( readFileBufferString.substr(
                                         index + tempString.length() ) );
      if ( !im_sixString )
        {
        d3proc_InputStream.close();
        return false;
        }
      im_sixString >> xDim;
      //std::cout << "xDim = " << xDim << std::endl;
      calcLength *= xDim;
      //std::cout << "calcLength = " << calcLength << std::endl;
      }

    // Get the y size.
    index = readFileBufferString.find(IM_SIY);
    if ( index != std::string::npos )
      {
      SizeValueType      yDim = 0;
      std::string        tempString = IM_SIY;
      std::istringstream im_siyString( readFileBufferString.substr(
                                         index + tempString.length() ) );
      if ( !im_siyString )
        {
        d3proc_InputStream.close();
        return false;
        }
      im_siyString >> yDim;
      //std::cout << "yDim = " << yDim << std::endl;
      calcLength *= yDim;
      //std::cout << "calcLength = " << calcLength << std::endl;
      }

    // Get the z size.
    index = readFileBufferString.find(IM_SIZ);
    if ( index != std::string::npos )
      {
      SizeValueType      zDim = 0;
      std::string        tempString = IM_SIZ;
      std::istringstream im_sizString( readFileBufferString.substr(
                                         index + tempString.length() ) );
      if ( !im_sizString )
        {
        d3proc_InputStream.close();
        return false;
        }
      im_sizString >> zDim;
      //std::cout << "zDim = " << zDim << std::endl;
      calcLength *= zDim;
      //std::cout << "calcLength = " << calcLength << std::endl;
      }
    }
  d3proc_InputStream.close();

  // Compare the file length to the calculated length.
  // Are they equal?
  if ( calcLength != length2DSEQ )
    {
    return false;
    }

  return true;
}

void Bruker2DSEQImageIO::ReadImageInformation()
{
  unsigned int dim;
  std::string  file2Dseq =
    itksys::SystemTools::CollapseFullPath( this->m_FileName.c_str() );

  itksys::SystemTools::ConvertToUnixSlashes(file2Dseq);
  std::string path = itksys::SystemTools::GetFilenamePath(file2Dseq);
  std::string filereco = path + FORWARDSLASH_DIRECTORY_SEPARATOR;
  filereco += RECO_FILE;
  std::string filed3proc = path + FORWARDSLASH_DIRECTORY_SEPARATOR;
  filed3proc += DTHREEPROC_FILE;
  std::vector< std::string > pathComponents;
  itksys::SystemTools::SplitPath(path.c_str(), pathComponents);
  if ( pathComponents.size() < 3 )
    {
    std::ostringstream message;
    message << "Cannot create path for acqp file: "
            << path << std::endl
            << "Path Components: ";
    for ( unsigned int i = 0; i < pathComponents.size(); i++ )
      {
      message << pathComponents[i] << "' ";
      }
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }
  // Go two directories up.
  pathComponents.pop_back(); pathComponents.pop_back();
  path = itksys::SystemTools::JoinPath(pathComponents);
  std::string fileacqp = path + FORWARDSLASH_DIRECTORY_SEPARATOR;
  fileacqp += ACQP_FILE;
  std::string                 readFileBufferString = "";
  char                        readFileBuffer[512] = "";
  std::string::size_type      index;
  std::string::size_type      tempIndex = 0;
  std::vector< double >       imageFOV(3);
  std::vector< unsigned int > imageDim(3);
  bool                        numDimensions = false;
  bool                        byteOrder = false;
  bool                        slicesNotInSameOrientation = false;
  bool                        echoTime = false;
  bool                        repetitionTime = false;
  bool                        inversionTime = false;
  int                         ni = 0;
  int                         nr = 0;
  unsigned int                numEchoImages = 0;
  bool                        sliceThickness = false;
  bool                        sliceSeperation = false;
  int                         numSeperation = 0;
  int                         numRecoTranspose = -1;
  double                      sliceThick = 0;
  std::string                 seperationMode = "";
  std::vector< double >       dirx(3, 0), diry(3, 0), dirz(3, 0);
  std::vector< int >          recoTransposition;
  int                         acq_dim = -1;
  int                         transpose = 0;

  // Get the meta dictionary for this object.
  MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
  std::string          classname( this->GetNameOfClass() );
  EncapsulateMetaData< std::string >(thisDic, ITK_InputFilterName, classname);

  std::ifstream d3proc_InputStream;
  d3proc_InputStream.open(filed3proc.c_str(),
                          std::ios::in);
  if ( d3proc_InputStream.fail() )
    {
    std::ostringstream message;
    message << "d3proc file: " <<  filed3proc << " cannot be opened.";
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }
  d3proc_InputStream.imbue( std::locale::classic() );
  while ( !d3proc_InputStream.eof() )
    {
    d3proc_InputStream.getline( readFileBuffer, sizeof( readFileBuffer ) );
    readFileBufferString = readFileBuffer;

    // Get the image data type.
    // This method is not a reliable method for determining the data type.
    // Using RECO_wordtype instead.
    //index = readFileBufferString.find(DATTYPE);
    //if( index != std::string::npos )
    //  {
    //  std::string tempString = DATTYPE;
    //  std::string dattypeString =
    //    readFileBufferString.substr(index+tempString.length());
    //  if( dattypeString.find(IP_CHAR) != std::string::npos )
    //    {
    //    this->m_ComponentType = CHAR;
    //    this->m_PixelType = SCALAR;
    //    }
    //    else if( (dattypeString.find(IP_SHORT) != std::string::npos) ||
    //      (dattypeString.find("3") != std::string::npos) )
    //    {
    //    this->m_ComponentType = SHORT;
    //    this->m_PixelType = SCALAR;
    //    }
    //    else if( (dattypeString.find(IP_INT) != std::string::npos) ||
    //      (dattypeString.find("5") != std::string::npos) )
    //    {
    //    this->m_ComponentType = INT;
    //    this->m_PixelType = SCALAR;
    //    }
    //    else
    //    {
    //    ExceptionObject exception(__FILE__, __LINE__);
    //    exception.SetDescription("Invalid d3proc file: "
    //      "Couldn't locate data type string");
    //    throw exception;
    //    }
    //  }

    // Get the x size.
    index = readFileBufferString.find(IM_SIX);
    if ( index != std::string::npos )
      {
      std::string        tempString = IM_SIX;
      std::istringstream im_sixString( readFileBufferString.substr(
                                         index + tempString.length() ) );
      if ( !im_sixString )
        {
        d3proc_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$IM_SIX" << std::endl
                << ". File is "
                << filed3proc;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      im_sixString >> imageDim[0];
      }

    // Get the y size.
    index = readFileBufferString.find(IM_SIY);
    if ( index != std::string::npos )
      {
      std::string        tempString = IM_SIY;
      std::istringstream im_siyString( readFileBufferString.substr(
                                         index + tempString.length() ) );
      if ( !im_siyString )
        {
        d3proc_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$IM_SIY" << std::endl
                << ". File is "
                << filed3proc;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      im_siyString >> imageDim[1];
      }

    // Get the z size.
    index = readFileBufferString.find(IM_SIZ);
    if ( index != std::string::npos )
      {
      std::string        tempString = IM_SIZ;
      std::istringstream im_sizString( readFileBufferString.substr(
                                         index + tempString.length() ) );
      if ( !im_sizString )
        {
        d3proc_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$IM_SIZ" << std::endl
                << ". File is "
                << filed3proc;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      im_sizString >> imageDim[2];
      }
    }
  d3proc_InputStream.close();

  std::ifstream reco_InputStream;
  reco_InputStream.open(filereco.c_str(),
                        std::ios::in);
  if ( reco_InputStream.fail() )
    {
    std::ostringstream message;
    message << "reco file: " <<  filereco << " cannot be opened";
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }
  reco_InputStream.imbue( std::locale::classic() );
  while ( !reco_InputStream.eof() )
    {
    reco_InputStream.getline( readFileBuffer, sizeof( readFileBuffer ) );
    readFileBufferString = readFileBuffer;

    // Set number of dimensions and get fov.
    index = readFileBufferString.find(RECO_fov);
    if ( index != std::string::npos )
      {
      RECOFOVContainerType::Pointer tempRecoFOV = RECOFOVContainerType::New();
      tempIndex = readFileBufferString.find("2");
      if ( tempIndex != std::string::npos )
        {
        reco_InputStream >> imageFOV[0] >> imageFOV[1];
        imageFOV[2] = 0;
        tempRecoFOV->resize(2);
        tempRecoFOV->SetElement(0, imageFOV[0]);
        tempRecoFOV->SetElement(1, imageFOV[1]);
        numDimensions = true;
        }
      else
        {
        tempIndex = readFileBufferString.find("3");
        if ( tempIndex != std::string::npos )
          {
          reco_InputStream >> imageFOV[0] >> imageFOV[1] >> imageFOV[2];
          tempRecoFOV->resize(3);
          tempRecoFOV->SetElement(0, imageFOV[0]);
          tempRecoFOV->SetElement(1, imageFOV[1]);
          tempRecoFOV->SetElement(2, imageFOV[2]);
          numDimensions = true;
          }
        else
          {
          reco_InputStream.close();
          std::ostringstream message;
          message << "Invalid reco file: Couldn't locate proper "
                  << "fov parameters" << std::endl
                  << "Reco file is "
                  << filereco;
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        }
      EncapsulateMetaData< RECOFOVContainerType::Pointer >(
        thisDic, RECO_FOV, tempRecoFOV);
      }

    // Get reco size.
    index = readFileBufferString.find(RECO_size);
    if ( index != std::string::npos )
      {
      unsigned int tempRecoSize = 2;
      tempIndex = readFileBufferString.find("2");
      if ( tempIndex == std::string::npos )
        {
        tempIndex = readFileBufferString.find("3");
        tempRecoSize = 3;
        if ( tempIndex == std::string::npos )
          {
          reco_InputStream.close();
          std::ostringstream message;
          message << "Invalid reco file: Couldn't locate proper "
                  << "dimension parameters" << std::endl
                  << "Reco file is "
                  << filereco;
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        }
      EncapsulateMetaData< unsigned int >(thisDic, RECO_SIZE, tempRecoSize);
      }

    // Get data type
    index = readFileBufferString.find(RECO_wordtype);
    if ( index != std::string::npos )
      {
      //std::cout << readFileBufferString.c_str() << std::endl;
      tempIndex = readFileBufferString.find(BRUKER_SIGNED_CHAR);
      if ( tempIndex != std::string::npos )
        {
        this->m_ComponentType = CHAR;
        this->m_PixelType = SCALAR;
        EncapsulateMetaData< std::string >(
          thisDic, RECO_WORDTYPE, std::string(BRUKER_SIGNED_CHAR, 13) );
        }
      else
        {
        tempIndex = readFileBufferString.find(BRUKER_UNSIGNED_CHAR);
        if ( tempIndex != std::string::npos )
          {
          this->m_ComponentType = UCHAR;
          this->m_PixelType = SCALAR;
          EncapsulateMetaData< std::string >(
            thisDic, RECO_WORDTYPE, std::string(BRUKER_UNSIGNED_CHAR, 15) );
          }
        else
          {
          tempIndex = readFileBufferString.find(BRUKER_SIGNED_SHORT);
          if ( tempIndex != std::string::npos )
            {
            this->m_ComponentType = SHORT;
            this->m_PixelType = SCALAR;
            EncapsulateMetaData< std::string >(
              thisDic, RECO_WORDTYPE, std::string(BRUKER_SIGNED_SHORT, 14) );
            }
          else
            {
            tempIndex = readFileBufferString.find(BRUKER_SIGNED_INT);
            if ( tempIndex != std::string::npos )
              {
              this->m_ComponentType = INT;
              this->m_PixelType = SCALAR;
              EncapsulateMetaData< std::string >(
                thisDic, RECO_WORDTYPE, std::string(BRUKER_SIGNED_INT, 14) );
              }
            else
              {
              tempIndex = readFileBufferString.find(BRUKER_FLOAT);
              if ( tempIndex != std::string::npos )
                {
                this->m_ComponentType = FLOAT;
                this->m_PixelType = SCALAR;
                EncapsulateMetaData< std::string >(
                  thisDic, RECO_WORDTYPE, std::string(BRUKER_FLOAT, 12) );
                }
              else
                {
                reco_InputStream.close();
                std::ostringstream message;
                message << "Invalid reco file: Couldn't locate proper "
                        << "wordtype parameter" << std::endl
                        << "Reco file is "
                        << filereco;
                ExceptionObject exception(__FILE__, __LINE__,
                                          message.str(),
                                          ITK_LOCATION);
                throw exception;
                }
              }
            }
          }
        }
      }

    // OK, handle RECO_transposition!
    index = readFileBufferString.find(RECO_transposition);
    if ( index != std::string::npos )
      {
      std::string        tempString = RECO_transposition;
      std::istringstream recoTransposeString( readFileBufferString.substr(
                                                index + tempString.length() ) );
      if ( !recoTransposeString )
        {
        reco_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$RECO_transposition" << std::endl
                << "Reco file is "
                << filereco;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      recoTransposeString >> numRecoTranspose;
      //std::cout << "numRecoTranspose = " << numRecoTranspose << std::endl;
      if ( numRecoTranspose > 0 )
        {
        RECOTranspositionContainerType::Pointer tempRecoTransposition =
          RECOTranspositionContainerType::New();
        recoTransposition.resize(numRecoTranspose);
        tempRecoTransposition->resize(numRecoTranspose);
        for ( unsigned int i = 0; i < (unsigned int)numRecoTranspose; i++ )
          {
          reco_InputStream >> recoTransposition[i];
          tempRecoTransposition->SetElement(i, recoTransposition[i]);
          }
        EncapsulateMetaData< RECOTranspositionContainerType::Pointer >(
          thisDic, RECO_TRANSPOSITION, tempRecoTransposition);
        }
      }

    // Get RECO_image_type.
    index = readFileBufferString.find(RECO_image_type);
    if ( index != std::string::npos )
      {
      std::string tempString = RECO_image_type;
      std::string recoType = readFileBufferString.substr( index + tempString.length() );
      if ( recoType.find(MAGNITUDE_IMAGE) != std::string::npos )
        {
        EncapsulateMetaData< std::string >(
          thisDic, RECO_IMAGE_TYPE, std::string(MAGNITUDE_IMAGE, 15) );
        }
      else if ( recoType.find(REAL_IMAGE) != std::string::npos )
        {
        EncapsulateMetaData< std::string >(
          thisDic, RECO_IMAGE_TYPE, std::string(REAL_IMAGE, 10) );
        }
      else if ( recoType.find(IMAGINARY_IMAGE) != std::string::npos )
        {
        EncapsulateMetaData< std::string >(
          thisDic, RECO_IMAGE_TYPE, std::string(IMAGINARY_IMAGE, 15) );
        }
      else if ( recoType.find(COMPLEX_IMAGE) != std::string::npos )
        {
        EncapsulateMetaData< std::string >(
          thisDic, RECO_IMAGE_TYPE, std::string(COMPLEX_IMAGE, 15) );
        }
      else if ( recoType.find(PHASE_IMAGE) != std::string::npos )
        {
        EncapsulateMetaData< std::string >(
          thisDic, RECO_IMAGE_TYPE, std::string(PHASE_IMAGE, 11) );
        }
      else if ( recoType.find(IR_IMAGE) != std::string::npos )
        {
        EncapsulateMetaData< std::string >(
          thisDic, RECO_IMAGE_TYPE, std::string(IR_IMAGE, 8) );
        }
      else
        {
        reco_InputStream.close();
        std::ostringstream message;
        message << "Invalid reco file: Couldn't locate proper"
                << "datatype parameter" << std::endl
                << "Reco file is "
                << filereco;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      }

    // Set byte order
    index = readFileBufferString.find(RECO_byte_order);
    if ( index != std::string::npos )
      {
      tempIndex = readFileBufferString.find(BRUKER_LITTLE_ENDIAN);
      if ( tempIndex != std::string::npos )
        {
        this->m_ByteOrder = LittleEndian;
        byteOrder = true;
        EncapsulateMetaData< std::string >(
          thisDic, RECO_BYTE_ORDER, std::string(BRUKER_LITTLE_ENDIAN, 12) );
        }
      else
        {
        tempIndex = readFileBufferString.find(BRUKER_BIG_ENDIAN);
        if ( tempIndex != std::string::npos )
          {
          this->m_ByteOrder = BigEndian;
          byteOrder = true;
          EncapsulateMetaData< std::string >(
            thisDic, RECO_BYTE_ORDER, std::string(BRUKER_BIG_ENDIAN, 9) );
          }
        else
          {
          reco_InputStream.close();
          std::ostringstream message;
          message << "Invalid reco file: Couldn't locate proper"
                  << "byte order parameter" << std::endl
                  << "Reco file is "
                  << filereco;
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        }
      }
    }
  reco_InputStream.close();

  if ( !numDimensions )
    {
    std::ostringstream message;
    message << "Invalid reco file: Couldn't locate "
            << "'##$RECO_fov=(' tag" << std::endl
            << "Reco file is "
            << filereco;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }
  if ( !byteOrder )
    {
    std::ostringstream message;
    message << "Invalid reco file: Couldn't locate "
            << "'##$RECO_byte_order=' tag" << std::endl
            << "Reco file is "
            << filereco;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }
  if ( numRecoTranspose < 0 )
    {
    std::ostringstream message;
    message << "Invalid reco file: Couldn't locate "
            << "'##$RECO_transposition=(' tag" << std::endl
            << "Reco file is "
            << filereco;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }

  // Open the acqp file & extract relevant info.
  std::ifstream acqp_InputStream;
  std::string   acqpFileString = "";
  acqp_InputStream.open(fileacqp.c_str(),
                        std::ios::in);
  //std::cout << fileacqp.c_str() << std::endl;
  if ( acqp_InputStream.fail() )
    {
    std::ostringstream message;
    message << "acqp file cannot be opened. "
            << "File is "
            << fileacqp;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }
  acqp_InputStream.imbue( std::locale::classic() );
  while ( !acqp_InputStream.eof() )
    {
    acqp_InputStream.getline( readFileBuffer, sizeof( readFileBuffer ) );

    acqpFileString = readFileBuffer;

    // Get ACQ_dim.
    index = acqpFileString.find(ACQ_dim);
    if ( index != std::string::npos )
      {
      std::string        tempString = ACQ_dim;
      std::istringstream acqDimString( acqpFileString.substr(
                                         index + tempString.length() ) );
      if ( !acqDimString )
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$ACQ_dim. "
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      acqDimString >> acq_dim;
      //std::cout << "acq_dim = " << acq_dim << std::endl;
      EncapsulateMetaData< int >(thisDic, ACQ_DIM, acq_dim);
      }

    // Get the number of objects produced by a single repetition.
    // Number of slices multiplied by the number of echos.
    index = acqpFileString.find(Ni);
    if ( index != std::string::npos )
      {
      std::string        tempString = Ni;
      std::istringstream niString( acqpFileString.substr(
                                     index + tempString.length() ) );
      if ( !niString )
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$NI" << std::endl
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      niString >> ni;
      //std::cout << "ni = " << ni << std::endl;
      EncapsulateMetaData< int >(thisDic, NI, ni);
      }

    // Get the number of repetitions of this experiment.
    index = acqpFileString.find(Nr);
    if ( index != std::string::npos )
      {
      std::string        tempString = Nr;
      std::istringstream nrString( acqpFileString.substr(
                                     index + tempString.length() ) );
      if ( !nrString )
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$NR" << std::endl
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      nrString >> nr;
      //std::cout << "nr = " << nr << std::endl;
      EncapsulateMetaData< int >(thisDic, NR, nr);
      }

    // Get number of echo images.
    index = acqpFileString.find(Nechoes);
    if ( index != std::string::npos )
      {
      std::string        tempString = Nechoes;
      std::istringstream dimString( acqpFileString.substr(
                                      index + tempString.length() ) );
      if ( !dimString )
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$NECHOES" << std::endl
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      dimString >> numEchoImages;
      //std::cout << "numEchoImages = " << numEchoImages << std::endl;
      EncapsulateMetaData< unsigned int >(thisDic, NECHOES, numEchoImages);
      }

    //Get the slice thickness
    index = acqpFileString.find(ACQ_slice_thick);
    if ( index != std::string::npos )
      {
      std::string        tempString = ACQ_slice_thick;
      std::istringstream sliceThickString( acqpFileString.substr(
                                             index + tempString.length() ) );
      if ( !sliceThickString )
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$ACQ_slice_thick" << std::endl
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      sliceThickString >> sliceThick;
      //std::cout << "sliceThick = " << sliceThick << std::endl;
      sliceThickness = true;
      EncapsulateMetaData< double >(thisDic, ACQ_SLICE_THICK, sliceThick);
      }

    // Get the slice separation.
    index = acqpFileString.find(ACQ_slice_sepn);
    if ( index != std::string::npos )
      {
      std::string        tempString = ACQ_slice_sepn;
      std::istringstream sliceSepString( acqpFileString.substr(
                                           index + tempString.length() ) );
      if ( !sliceSepString )
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$ACQ_slice_sepn" << std::endl
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      sliceSepString >> numSeperation;
      //std::cout << "numSeperation = " << numSeperation << std::endl;
      if ( numSeperation > 0 )
        {
        std::vector< double >              imageSliceSeperation(numSeperation);
        ACQSliceSepnContainerType::Pointer sliceSepn =
          ACQSliceSepnContainerType::New();
        sliceSepn->resize(numSeperation);
        for ( unsigned int i = 0; i < (unsigned int)numSeperation; i++ )
          {
          acqp_InputStream >> imageSliceSeperation[i];
          sliceSepn->SetElement(i, imageSliceSeperation[i]);
          }
        EncapsulateMetaData< ACQSliceSepnContainerType::Pointer >(
          thisDic, ACQ_SLICE_SEPN, sliceSepn);
        sliceSeperation = true;
        }
      }

    // Get the slice separation mode.
    index = acqpFileString.find(ACQ_slice_sepn_mode);
    if ( index != std::string::npos )
      {
      std::string tempString = ACQ_slice_sepn_mode;
      seperationMode = acqpFileString.substr( index + tempString.length() );
      //std::cout << "seperationMode = " << seperationMode << std::endl;
      EncapsulateMetaData< std::string >(
        thisDic, ACQ_SLICE_SEPN_MODE, seperationMode);
      }

    // Get echo times.
    index = acqpFileString.find(ACQ_echo_time);
    if ( index != std::string::npos )
      {
      int numEchoes = 0;
      std::string        tempString = ACQ_echo_time;
      std::istringstream echoTimeString( acqpFileString.substr(
                                           index + tempString.length() ) );
      if ( !echoTimeString )
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$ACQ_echo_time" << std::endl
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      echoTimeString >> numEchoes;
      //std::cout << "numEchoes = " << numEchoes << std::endl;
      if ( numEchoes > 0 )
        {
        std::vector< double >             Echo_time(numEchoes);
        ACQEchoTimeContainerType::Pointer echoTimes =
          ACQEchoTimeContainerType::New();
        echoTimes->resize(numEchoes);
        for ( unsigned int i = 0; i < (unsigned int)numEchoes; i++ )
          {
          acqp_InputStream >> Echo_time[i];
          echoTimes->SetElement(i, Echo_time[i]);
          }
        EncapsulateMetaData< ACQEchoTimeContainerType::Pointer >(
          thisDic, ACQ_ECHO_TIME, echoTimes);
        echoTime = true;
        }
      else
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not retrieve ##$ACQ_echo_times" << std::endl
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      }

    // Get repetition times.
    index = acqpFileString.find(ACQ_repetition_time);
    if ( index != std::string::npos )
      {
      int numRepetitions = 0;
      std::string        tempString = ACQ_repetition_time;
      std::istringstream reptitionTimeString( acqpFileString.substr(
                                                index + tempString.length() ) );
      if ( !reptitionTimeString )
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$ACQ_repetition_time" << std::endl
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      reptitionTimeString >> numRepetitions;
      //std::cout << "numRepetitions = " << numRepetitions << std::endl;
      if ( numRepetitions > 0 )
        {
        std::vector< double >                   Repetition_time(numRepetitions);
        ACQRepetitionTimeContainerType::Pointer repetitionTimes =
          ACQRepetitionTimeContainerType::New();
        repetitionTimes->resize(numRepetitions);
        for ( unsigned int i = 0; i < (unsigned int)numRepetitions; i++ )
          {
          acqp_InputStream >> Repetition_time[i];
          repetitionTimes->SetElement(i, Repetition_time[i]);
          }
        EncapsulateMetaData< ACQRepetitionTimeContainerType::Pointer >(
          thisDic, ACQ_REPETITION_TIME, repetitionTimes);
        repetitionTime = true;
        }
      else
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not retrieve ##$ACQ_repetition_time" << std::endl
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      }

    // Get inversion times.
    index = acqpFileString.find(ACQ_inversion_time);
    if ( index != std::string::npos )
      {
      int numInversionTimes = 0;
      std::string        tempString = ACQ_inversion_time;
      std::istringstream inversionTimeString( acqpFileString.substr(
                                                index + tempString.length() ) );
      if ( !inversionTimeString )
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$ACQ_inversion_time" << std::endl
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      inversionTimeString >> numInversionTimes;
      //std::cout << "numInversionTimes = " << numInversionTimes << std::endl;
      if ( numInversionTimes > 0 )
        {
        std::vector< double >                  Inversion_time(numInversionTimes);
        ACQInversionTimeContainerType::Pointer inversionTimes =
          ACQInversionTimeContainerType::New();
        inversionTimes->resize(numInversionTimes);
        for ( unsigned int i = 0; i < (unsigned int)numInversionTimes; i++ )
          {
          acqp_InputStream >> Inversion_time[i];
          inversionTimes->SetElement(i, Inversion_time[i]);
          }
        EncapsulateMetaData< ACQInversionTimeContainerType::Pointer >(
          thisDic, ACQ_INVERSION_TIME, inversionTimes);
        inversionTime = true;
        }
      else
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not retrieve ##$ACQ_inversion_time" << std::endl
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      }

    // Get direction cosines.
    index = acqpFileString.find(ACQ_grad_matrix);
    if ( index != std::string::npos )
      {
      std::string tempString = ACQ_grad_matrix;
      int         numMatrix = 0, dim1 = 0, dim2 = 0;
      tempString = acqpFileString.substr( index + tempString.length() );
      // MS VC++ cannot handle commas, so replace with spaces.
      for ( std::string::iterator iter = tempString.begin();
            iter != tempString.end(); ++iter )
        {
        if ( *iter == ',' )
          {
          *iter = ' ';
          }
        }
      std::istringstream gradMatrixString(tempString);
      if ( !gradMatrixString )
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not create std::istringstream for "
                << "##$ACQ_grad_matrix" << std::endl
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      gradMatrixString >> numMatrix >> dim1 >> dim2;
      //std::cout << "numMatrix = " << numMatrix << std::endl;
      //std::cout << "dim1 = " << dim1 << std::endl;
      //std::cout << "dim2 = " << dim2 << std::endl;
      if ( numMatrix && ( dim1 == 3 ) && ( dim2 == 3 ) )
        {
        // OK, I need ACQ_dim at this point in the code,
        // so throw an exception if I don't have it.
        if ( acq_dim < 0 )
          {
          acqp_InputStream.close();
          std::ostringstream message;
          message << "Invalid acqp file: Couldn't locate "
                  << "'##$ACQ_dim=' tag" << std::endl
                  << "The file is "
                  << fileacqp;
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        int i = 0;
        for ( i = 0; i < 3; i++ )
          {
          acqp_InputStream >> dirx[i];
          if ( dirx[i] == -0 )
            {
            dirx[i] = 0;
            }
          //std::cout << "dirx[" << i << "]= " << dirx[i] << std::endl;
          }
        for ( i = 0; i < 3; i++ )
          {
          acqp_InputStream >> diry[i];
          if ( diry[i] == -0 )
            {
            diry[i] = 0;
            }
          //std::cout << "diry[" << i << "]= " << diry[i] << std::endl;
          }
        for ( i = 0; i < 3; i++ )
          {
          acqp_InputStream >> dirz[i];
          if ( dirz[i] == -0 )
            {
            dirz[i] = 0;
            }
          //std::cout << "dirz[" << i << "]= " << dirz[i] << std::endl;
          }
        // Ok, now that the directions are read in transpose if necessary.
        if ( ( acq_dim == 2 )
             && ( numRecoTranspose == numMatrix )
             && recoTransposition[0] )
          {
          // Transpose read/phase.
          transpose = 1;
          std::vector< double > temp(3, 0);
          for ( i = 0; i < 3; i++ )
            {
            temp[i] = dirx[i];
            dirx[i] = diry[i];
            diry[i] = temp[i];
            }
          }
        else if ( recoTransposition[0] == 1 )
          {
          // Transpose read/phase.
          transpose = 1;
          std::vector< double > temp(3, 0);
          for ( i = 0; i < 3; i++ )
            {
            temp[i] = dirx[i];
            dirx[i] = diry[i];
            diry[i] = temp[i];
            }
          }
        else if ( recoTransposition[0] == 2 )
          {
          // Transpose phase/slice.
          transpose = 2;
          std::vector< double > temp(3, 0);
          for ( i = 0; i < 3; i++ )
            {
            temp[i] = diry[i];
            diry[i] = dirz[i];
            dirz[i] = temp[i];
            }
          }
        else if ( recoTransposition[0] == 3 )
          {
          // Transpose read/slice.
          transpose = 3;
          std::vector< double > temp(3, 0);
          for ( i = 0; i < 3; i++ )
            {
            temp[i] = dirx[i];
            dirx[i] = dirz[i];
            dirz[i] = temp[i];
            }
          }
        // Check to see if all of the slices are in the same orientation.
        // If not then only use the first slice (may change this behavior
        // later).
        if ( ( numMatrix - 1 ) > 0 )
          {
          std::vector< double > gradMatrixX(3, 0);
          std::vector< double > gradMatrixY(3, 0);
          std::vector< double > gradMatrixZ(3, 0);
          for ( int j = 0; j < ( numMatrix - 1 ); j++ )
            {
            int l = 0;
            for ( l = 0; l < 3; l++ )
              {
              acqp_InputStream >> gradMatrixX[l];
              if ( gradMatrixX[l] == -0 )
                {
                gradMatrixX[l] = 0;
                }
              }
            for ( l = 0; l < 3; l++ )
              {
              acqp_InputStream >> gradMatrixY[l];
              if ( gradMatrixY[l] == -0 )
                {
                gradMatrixY[l] = 0;
                }
              }
            for ( l = 0; l < 3; l++ )
              {
              acqp_InputStream >> gradMatrixZ[l];
              if ( gradMatrixZ[l] == -0 )
                {
                gradMatrixZ[l] = 0;
                }
              }
            // Transpose if necessary.
            if ( ( acq_dim == 2 ) && recoTransposition[j + 1] )
              {
              // Transpose read/phase.
              std::vector< double > temp(3, 0);
              for ( i = 0; i < 3; i++ )
                {
                temp[i] = gradMatrixX[i];
                gradMatrixX[i] = gradMatrixY[i];
                gradMatrixY[i] = temp[i];
                }
              }
            else if ( recoTransposition[j + 1] == 1 )
              {
              // Transpose read/phase.
              std::vector< double > temp(3, 0);
              for ( i = 0; i < 3; i++ )
                {
                temp[i] = gradMatrixX[i];
                gradMatrixX[i] = gradMatrixY[i];
                gradMatrixY[i] = temp[i];
                }
              }
            else if ( recoTransposition[j + 1] == 2 )
              {
              // Transpose phase/slice.
              std::vector< double > temp(3, 0);
              for ( i = 0; i < 3; i++ )
                {
                temp[i] = gradMatrixY[i];
                gradMatrixY[i] = gradMatrixZ[i];
                gradMatrixZ[i] = temp[i];
                }
              }
            else if ( recoTransposition[j + 1] == 3 )
              {
              // Transpose read/slice.
              std::vector< double > temp(3, 0);
              for ( i = 0; i < 3; i++ )
                {
                temp[i] = gradMatrixX[i];
                gradMatrixX[i] = gradMatrixZ[i];
                gradMatrixZ[i] = temp[i];
                }
              }
            // Compare with original
            if ( !std::equal( dirx.begin(), dirx.end(), gradMatrixX.begin() )
                 || !std::equal( diry.begin(), diry.end(), gradMatrixY.begin() )
                 || !std::equal( dirz.begin(), dirz.end(), gradMatrixZ.begin() ) )
              {
              slicesNotInSameOrientation = true;
              break;
              }
            }
          }
        }
      else
        {
        acqp_InputStream.close();
        std::ostringstream message;
        message << "Could not retrieve ##$ACQ_grad_matrix" << std::endl
                << "The file is "
                << fileacqp;
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      }
    }
  acqp_InputStream.close();

  if ( !echoTime )
    {
    std::ostringstream message;
    message << "Invalid acqp file: Couldn't locate "
            <<       "'##$ACQ_echo_time=( ' tag" << std::endl
            << "The file is "
            << fileacqp;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }

  if ( !sliceThickness )
    {
    std::ostringstream message;
    message << "Invalid acqp file: Couldn't locate "
            << "'##$ACQ_slice_thick=' tag" << std::endl
            << "The file is "
            << fileacqp;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }

  if ( !sliceSeperation )
    {
    std::ostringstream message;
    message << "Invalid acqp file: Couldn't locate "
            << "'##$ACQ_slice_sepn=(' tag" << std::endl
            << "The file is "
            << fileacqp;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }

  if ( !nr )
    {
    std::ostringstream message;
    message << "Invalid acqp file: Couldn't locate "
            << "'##$NR=' tag" << std::endl
            << "The file is "
            << fileacqp;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }

  if ( !ni )
    {
    std::ostringstream message;
    message << "Invalid acqp file: Couldn't locate "
            << "'##$NI=' tag" << std::endl
            << "The file is "
            << fileacqp;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }

  if ( !echoTime )
    {
    std::ostringstream message;
    message << "Invalid acqp file: Couldn't locate "
            << "'##$ACQ_echo_time=( ' tag" << std::endl
            << "The file is "
            << fileacqp;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }

  if ( !repetitionTime )
    {
    std::ostringstream message;
    message << "Invalid acqp file: Couldn't locate "
            << "'##$ACQ_repetition_time=( ' tag" << std::endl
            << "The file is "
            << fileacqp;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }

  if ( !inversionTime )
    {
    std::ostringstream message;
    message << "Invalid acqp file: Couldn't locate "
            << "'##$ACQ_inversion_time=( ' tag" << std::endl
            << "The file is "
            << fileacqp;
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }

  // This is definitely a hack that will not always be correct, but should work
  // for Bruker images that have been acquired as homogeneous volumes.
  if ( imageFOV[2] == 0 )
    {
    imageFOV[2] = imageDim[2] * sliceThick;
    imageFOV[2] /= 10.0f; //Convert from mm to cm.
    }

  if ( slicesNotInSameOrientation )
    {
    imageDim[2] = 1;
    }

  EncapsulateMetaData< unsigned int >(
    thisDic, ITK_NumberOfDimensions, this->GetNumberOfDimensions() );

  //
  // Transpose the dims and FOV if required.
  switch ( transpose )
    {
    case 1:
      {
      double       tempFOV;
      unsigned int tempDim;
      tempFOV = imageFOV[0];
      imageFOV[0] = imageFOV[1];
      imageFOV[1] = tempFOV;
      tempDim = imageDim[0];
      imageDim[0] = imageDim[1];
      imageDim[1] = tempDim;
      }
      break;
    case 2:
      {
      double       tempFOV;
      unsigned int tempDim;
      tempFOV = imageFOV[1];
      imageFOV[1] = imageFOV[2];
      imageFOV[2] = tempFOV;
      tempDim = imageDim[1];
      imageDim[1] = imageDim[2];
      imageDim[2] = tempDim;
      }
      break;
    case 3:
      {
      double       tempFOV;
      unsigned int tempDim;
      tempFOV = imageFOV[0];
      imageFOV[0] = imageFOV[2];
      imageFOV[2] = tempFOV;
      tempDim = imageDim[0];
      imageDim[0] = imageDim[2];
      imageDim[2] = tempDim;
      }
      break;
    }

  //
  // set up the dimension stuff
  for ( dim = 0; dim < this->GetNumberOfDimensions(); dim++ )
    {
    this->SetDimensions(dim, imageDim[dim]);
    imageFOV[dim] *= 10.0f; //Convert from cm to mm.
    this->SetSpacing(dim, imageFOV[dim] / (double)imageDim[dim]);
    this->SetOrigin(dim, -imageFOV[dim] / 2.0f);
    }
  switch ( this->GetNumberOfDimensions() )
    {
    case 1:
      this->SetDirection(0, dirx);
      break;
    case 2:
      this->SetDirection(0, dirx);
      this->SetDirection(1, diry);
      break;
    case 3:
    default:
      this->SetDirection(0, dirx);
      this->SetDirection(1, diry);
      this->SetDirection(2, dirz);
    }
}
} // end namespace itk
