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
#include <nifti1_io.h> // Needed to make sure that the overlapping
                       // Analyze/Nifti readers do not overlap
#include "itkAnalyzeImageIO.h"
#include "itkIOCommon.h"
#include "itkByteSwapper.h"
#include "itkMetaDataObject.h"
#include "itkSpatialOrientationAdapter.h"
#include "itksys/SystemTools.hxx"
#include "itk_zlib.h"

namespace itk
{
const char *const ANALYZE_ScanNumber = "ANALYZE_ScanNumber";
const char *const ANALYZE_O_MAX = "ANALYZE_O_MAX";
const char *const ANALYZE_O_MIN = "ANALYZE_O_MIN";
const char *const ANALYZE_S_MAX = "ANALYZE_S_MAX";
const char *const ANALYZE_S_MIN = "ANALYZE_S_MIN";
const char *const ANALYZE_CAL_MAX = "ANALYZE_CAL_MAX";
const char *const ANALYZE_CAL_MIN = "ANALYZE_CAL_MIN";
const char *const ANALYZE_GLMAX = "ANALYZE_GLMAX";
const char *const ANALYZE_GLMIN = "ANALYZE_GLMIN";
const char *const ANALYZE_AUX_FILE_NAME = "ANALYZE_AUX_FILE_NAME";
const char *const ANALYZE_CALIBRATIONUNITS = "ANALYZE_CALIBRATIONUNITS";
//An array of the Analyze v7.5 known DataTypes
const char DataTypes[12][10] =
  {
  "UNKNOWN", "BINARY", "CHAR", "SHORT", "INT", "FLOAT",
  "COMPLEX", "DOUBLE", "RGB", "ALL", "USHORT", "UINT"
  };

//An array with the corresponding number of bits for each image type.
//NOTE: the following two line should be equivalent.
const short int DataTypeSizes[12] = { 0, 1, 8, 16, 32, 32, 64, 64, 24, 0, 16, 32 };

//An array with Data type key sizes
const short int DataTypeKey[12] =
  {
  ANALYZE_DT_UNKNOWN,
  ANALYZE_DT_BINARY,
  ANALYZE_DT_UNSIGNED_CHAR,
  ANALYZE_DT_SIGNED_SHORT,
  ANALYZE_DT_SIGNED_INT,
  ANALYZE_DT_FLOAT,
  ANALYZE_DT_COMPLEX,
  ANALYZE_DT_DOUBLE,
  ANALYZE_DT_RGB,
  ANALYZE_DT_ALL,
  SPMANALYZE_DT_UNSIGNED_SHORT,
  SPMANALYZE_DT_UNSIGNED_INT
  };

// due to gzip and other io limitations this is the maximum size to
// read at one time
const unsigned int ANALYZE_MAXIMUM_IO_CHUNK = itk::NumericTraits< unsigned int >::max() / 4;

static std::string
GetExtension(const std::string & filename)
{
  std::string fileExt( itksys::SystemTools::GetFilenameLastExtension(filename) );

  //If the last extension is .gz, then need to pull off 2 extensions.
  //.gz is the only valid compression extension.
  if ( fileExt == std::string(".gz") )
    {
    fileExt =
      itksys::SystemTools::GetFilenameLastExtension( itksys::SystemTools::GetFilenameWithoutLastExtension(filename) );
    fileExt += ".gz";
    }
  //Check that a valid extension was found.
  const char* fileExtension = fileExt.c_str();
  if ( itksys::SystemTools::Strucmp(fileExtension, ".img.gz") != 0
       && itksys::SystemTools::Strucmp(fileExtension, ".img") != 0
       && itksys::SystemTools::Strucmp(fileExtension, ".hdr") != 0 )
    {
    return ( "" );
    }
  return ( fileExt );
}

static std::string
GetRootName(const std::string & filename)
{
  const std::string fileExt = GetExtension(filename);

  // Create a base filename
  // i.e Image.hdr --> Image
  if ( fileExt.length() > 0                    //Ensure that an extension was
                                               // found
       && filename.length() > fileExt.length() //Ensure that the filename does
                                               // not contain only the extension
       )
    {
    const std::string::size_type it = filename.find_last_of(fileExt);
    const std::string            baseName( filename, 0, it - ( fileExt.length() - 1 ) );
    return ( baseName );
    }
  //Default to return same as input when the extension is nothing (Analyze)
  return ( filename );
}

static std::string
GetHeaderFileName(const std::string & filename)
{
  std::string ImageFileName = GetRootName(filename);

  ImageFileName += ".hdr";
  return ( ImageFileName );
}

//Returns the base image filename.
static std::string GetImageFileName(const std::string & filename)
{
  std::string       ImageFileName (filename);
  const std::string fileExt = GetExtension(filename);

  if ( fileExt == ".hdr" ) //Default to uncompressed .img if .hdr is given as
                           // file name.
    {
    ImageFileName = GetRootName(filename);
    ImageFileName += ".img";
    }
  return ( ImageFileName );
}

void
AnalyzeImageIO::SwapBytesIfNecessary(void *buffer,
                                     SizeType _numberOfPixels)
{
  // todo check for overflow error
  size_t numberOfPixels = static_cast< size_t >( _numberOfPixels );

  if ( m_ByteOrder == LittleEndian )
    {
    switch ( m_ComponentType )
      {
      case CHAR:
        ByteSwapper< char >::SwapRangeFromSystemToLittleEndian( (char *)buffer,
                                                                numberOfPixels );
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
        ByteSwapper< float >::SwapRangeFromSystemToLittleEndian( (float *)buffer,
                                                                 numberOfPixels );
        break;
      case DOUBLE:
        ByteSwapper< double >::SwapRangeFromSystemToLittleEndian
          ( (double *)buffer, numberOfPixels );
        break;
      default:
        itkExceptionMacro(<< "Pixel Type Unknown");
      }
    }
  else
    {
    switch ( m_ComponentType )
      {
      case CHAR:
        ByteSwapper< char >::SwapRangeFromSystemToBigEndian( (char *)buffer,
                                                             numberOfPixels );
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
        itkExceptionMacro(<< "Pixel Type Unknown");
      }
    }
}

ImageIOBase::ByteOrder
AnalyzeImageIO::CheckAnalyzeEndian(const struct dsr & temphdr)
{
  ImageIOBase::ByteOrder returnvalue;
  // Machine and header endianness is same

  // checking hk.extents only is NOT a good idea. Many programs do not set
  // hk.extents correctly. Doing an additional check on hk.sizeof_hdr
  // increases chance of correct result. --Juerg Tschirrin University of Iowa
  // All properly constructed analyze images should have the extents feild
  // set.  It is part of the file format standard.  While most headers of
  // analyze images are 348 bytes long, The Analyze file format allows the
  // header to have other lengths.
  // This code will fail in the unlikely event that the extents feild is
  // not set (invalid anlyze file anyway) and the header is not the normal
  // size.  Other peices of code have used a heuristic on the image
  // dimensions.  If the Image dimensions is greater
  // than 16000 then the image is almost certainly byte-swapped-- Hans

  const ImageIOBase::ByteOrder systemOrder =
    ( ByteSwapper< int >::SystemIsBigEndian() ) ? BigEndian : LittleEndian;

  if ( ( temphdr.hk.extents == 16384 ) || ( temphdr.hk.sizeof_hdr == 348 ) )
    {
    returnvalue = systemOrder;
    }
  else
    {
    // File does not match machine
    returnvalue = ( systemOrder == BigEndian ) ? LittleEndian : BigEndian;
    }
  return returnvalue;
}

void
AnalyzeImageIO::SwapHeaderBytesIfNecessary(struct dsr *const imageheader)
{
  if ( m_ByteOrder == LittleEndian )
    {
    // NOTE: If machine order is little endian, and the data needs to be
    // swapped, the SwapFromBigEndianToSystem is equivalent to
    // SwapFromSystemToBigEndian.
    ByteSwapper< int >::SwapFromSystemToLittleEndian(
      &imageheader->hk.sizeof_hdr);
    ByteSwapper< int  >::SwapFromSystemToLittleEndian(
      &imageheader->hk.extents);
    ByteSwapper< short int >::SwapFromSystemToLittleEndian(
      &imageheader->hk.session_error);
    ByteSwapper< short int >::SwapRangeFromSystemToLittleEndian(
      &imageheader->dime.dim[0], 8);
    ByteSwapper< short int >::SwapFromSystemToLittleEndian(
      &imageheader->dime.unused1);
    ByteSwapper< short int >::SwapFromSystemToLittleEndian(
      &imageheader->dime.datatype);
    ByteSwapper< short int >::SwapFromSystemToLittleEndian(
      &imageheader->dime.bitpix);
    ByteSwapper< short int >::SwapFromSystemToLittleEndian(
      &imageheader->dime.dim_un0);

    ByteSwapper< float >::SwapRangeFromSystemToLittleEndian(
      &imageheader->dime.pixdim[0], 8);
    ByteSwapper< float >::SwapFromSystemToLittleEndian(
      &imageheader->dime.vox_offset);
    ByteSwapper< float >::SwapFromSystemToLittleEndian(
      &imageheader->dime.roi_scale);
    ByteSwapper< float >::SwapFromSystemToLittleEndian(
      &imageheader->dime.funused1);
    ByteSwapper< float >::SwapFromSystemToLittleEndian(
      &imageheader->dime.funused2);
    ByteSwapper< float >::SwapFromSystemToLittleEndian(
      &imageheader->dime.cal_max);
    ByteSwapper< float >::SwapFromSystemToLittleEndian(
      &imageheader->dime.cal_min);
    ByteSwapper< int >::SwapFromSystemToLittleEndian(
      &imageheader->dime.compressed);
    ByteSwapper< int >::SwapFromSystemToLittleEndian(
      &imageheader->dime.verified);
    ByteSwapper< int >::SwapFromSystemToLittleEndian(
      &imageheader->dime.glmax);
    ByteSwapper< int >::SwapFromSystemToLittleEndian(
      &imageheader->dime.glmin);

    ByteSwapper< int >::SwapFromSystemToLittleEndian(
      &imageheader->hist.views);
    ByteSwapper< int >::SwapFromSystemToLittleEndian(
      &imageheader->hist.vols_added);
    ByteSwapper< int >::SwapFromSystemToLittleEndian(
      &imageheader->hist.start_field);
    ByteSwapper< int >::SwapFromSystemToLittleEndian(
      &imageheader->hist.field_skip);
    ByteSwapper< int >::SwapFromSystemToLittleEndian(
      &imageheader->hist.omax);
    ByteSwapper< int >::SwapFromSystemToLittleEndian(
      &imageheader->hist.omin);
    ByteSwapper< int >::SwapFromSystemToLittleEndian(
      &imageheader->hist.smax);
    ByteSwapper< int >::SwapFromSystemToLittleEndian(
      &imageheader->hist.smin);
    }
  else if ( m_ByteOrder == BigEndian )
    {
    //NOTE: If machine order is little endian, and the data needs to be
    // swapped, the SwapFromBigEndianToSystem is equivalent to
    // SwapFromSystemToLittleEndian.
    ByteSwapper< int  >::SwapFromSystemToBigEndian(
      &imageheader->hk.sizeof_hdr);
    ByteSwapper< int  >::SwapFromSystemToBigEndian(
      &imageheader->hk.extents);
    ByteSwapper< short int >::SwapFromSystemToBigEndian(
      &imageheader->hk.session_error);

    ByteSwapper< short int >::SwapRangeFromSystemToBigEndian(
      &imageheader->dime.dim[0], 8);
    ByteSwapper< short int >::SwapFromSystemToBigEndian(
      &imageheader->dime.unused1);
    ByteSwapper< short int >::SwapFromSystemToBigEndian(
      &imageheader->dime.datatype);
    ByteSwapper< short int >::SwapFromSystemToBigEndian(
      &imageheader->dime.bitpix);
    ByteSwapper< short int >::SwapFromSystemToBigEndian(
      &imageheader->dime.dim_un0);

    ByteSwapper< float >::SwapRangeFromSystemToBigEndian(
      &imageheader->dime.pixdim[0], 8);
    ByteSwapper< float >::SwapFromSystemToBigEndian(
      &imageheader->dime.vox_offset);
    ByteSwapper< float >::SwapFromSystemToBigEndian(
      &imageheader->dime.roi_scale);
    ByteSwapper< float >::SwapFromSystemToBigEndian(
      &imageheader->dime.funused1);
    ByteSwapper< float >::SwapFromSystemToBigEndian(
      &imageheader->dime.funused2);
    ByteSwapper< float >::SwapFromSystemToBigEndian(
      &imageheader->dime.cal_max);
    ByteSwapper< float >::SwapFromSystemToBigEndian(
      &imageheader->dime.cal_min);
    ByteSwapper< int >::SwapFromSystemToBigEndian(
      &imageheader->dime.compressed);
    ByteSwapper< int >::SwapFromSystemToBigEndian(
      &imageheader->dime.verified);
    ByteSwapper< int >::SwapFromSystemToBigEndian(
      &imageheader->dime.glmax);
    ByteSwapper< int >::SwapFromSystemToBigEndian(
      &imageheader->dime.glmin);

    ByteSwapper< int >::SwapFromSystemToBigEndian(
      &imageheader->hist.views);
    ByteSwapper< int >::SwapFromSystemToBigEndian(
      &imageheader->hist.vols_added);
    ByteSwapper< int >::SwapFromSystemToBigEndian(
      &imageheader->hist.start_field);
    ByteSwapper< int >::SwapFromSystemToBigEndian(
      &imageheader->hist.field_skip);
    ByteSwapper< int >::SwapFromSystemToBigEndian(
      &imageheader->hist.omax);
    ByteSwapper< int >::SwapFromSystemToBigEndian(
      &imageheader->hist.omin);
    ByteSwapper< int >::SwapFromSystemToBigEndian(
      &imageheader->hist.smax);
    ByteSwapper< int >::SwapFromSystemToBigEndian(
      &imageheader->hist.smin);
    }
  else
    {
    itkExceptionMacro(<< "Machine Endian Type Unknown");
    }
}

AnalyzeImageIO::AnalyzeImageIO()
{
  // By default, only have 3 dimensions
  this->SetNumberOfDimensions(3);
  m_PixelType         = SCALAR;
  m_ComponentType     = UCHAR;
  // Set m_MachineByteOrder to the ByteOrder of the machine
  // Start out with file byte order == system byte order
  // this will be changed if we're reading a file to whatever
  // the file actually contains.
  if ( ByteSwapper< int >::SystemIsBigEndian() )
    {
    m_MachineByteOrder = m_ByteOrder = BigEndian;
    }
  else
    {
    m_MachineByteOrder = m_ByteOrder = LittleEndian;
    }

  // Set all values to a default value
  // Must check again -- memset!!!!

  // Analyze stuff
  //  memset sets the first n bytes in memory area s to the value of c
  //  (cothis->m_Hdr.dime.dim[4]erted to an unsigned char).  It returns s.
  //  void *memset (void *s, int c, size_t n);
  memset( &( this->m_Hdr ), 0, sizeof( struct dsr ) );

  // strcpy(this->m_Hdr.hk.data_type,DataTypes[DT_INDEX_UNKNOWN]);
  /* Acceptable this->m_Hdr.hk.data_type values are */
  /* "UNKNOWN","BINARY","CHAR","SHORT","INT","FLOAT","COMPLEX","DOUBLE","RGB" */
  this->m_Hdr.hk.sizeof_hdr = static_cast< int >( sizeof( struct dsr ) );
  this->m_Hdr.hk.db_name[0] = '\0';
  this->m_Hdr.hk.extents = 16384;
  this->m_Hdr.hk.session_error = 0;
  this->m_Hdr.hk.regular = 'r';
  this->m_Hdr.hk.hkey_un0 = '\0';

  /* HeaderObj_dimension information*/
  this->m_Hdr.dime.dim[0] = 4;     //Usually 4 x,y,z,time
  this->m_Hdr.dime.dim[1] = 1;     //size_x;//number of columns
  this->m_Hdr.dime.dim[2] = 1;     //size_y;//number of rows
  this->m_Hdr.dime.dim[3] = 1;     //size_z;//number of slices
  this->m_Hdr.dime.dim[4] = 1;     //size_t;//number of volumes
  this->m_Hdr.dime.dim[5] = 1;
  this->m_Hdr.dime.dim[6] = 1;
  this->m_Hdr.dime.dim[7] = 1;

  /* labels voxel spatial unit */
  this->m_Hdr.dime.vox_units[0] = '\0';
  /* labels voxel calibration unit */
  this->m_Hdr.dime.cal_units[0] = '\0';

  this->m_Hdr.dime.unused1 = 0;
  // Acceptable data values are DT_NONE, DT_UNKOWN, DT_BINARY,
  // DT_UNSIGNED_CHAR
  // DT_SIGNED_SHORT, DT_SIGNED_INT, DT_FLOAT, DT_COMPLEX, DT_DOUBLE,
  // DT_RGB, DT_ALL
  //this->m_Hdr.dime.datatype=DataTypeKey[DT_INDEX_UNKNOWN];

  // this->m_Hdr.dime.bitpix=DataTypeSizes[DT_INDEX_UNKNOWN];/*bits per pixel*/
  this->m_Hdr.dime.dim_un0 = 0;

  // Set the voxel dimension fields:
  // A value of 0.0 for these fields implies that the value is unknown.
  // Change these values to what is appropriate for your data
  // or pass additional commathis->m_Hdr.dime.dim[0] line arguments
  this->m_Hdr.dime.pixdim[0] = 0.0f; //Unused field
  this->m_Hdr.dime.pixdim[1] = 1.0f; //x_dimension
  this->m_Hdr.dime.pixdim[2] = 1.0f; //y_dimension
  this->m_Hdr.dime.pixdim[3] = 1.0f; //z_dimension
  this->m_Hdr.dime.pixdim[4] = 1.0f; //t_dimension
  this->m_Hdr.dime.pixdim[5] = 1.0f;
  this->m_Hdr.dime.pixdim[6] = 1.0f;
  this->m_Hdr.dime.pixdim[7] = 1.0f;
  // Assume zero offset in .img file, byte at which pixel data starts in
  // the HeaderObj file
  // byte offset in the HeaderObj file which voxels start
  this->m_Hdr.dime.vox_offset = 0.0f;

  this->m_Hdr.dime.roi_scale = 0.0f;
  this->m_Hdr.dime.funused1 = 0.0f;
  this->m_Hdr.dime.funused2 = 0.0f;
  this->m_Hdr.dime.cal_max = 0.0f; // specify range of calibration values
  this->m_Hdr.dime.cal_min = 0.0f; // specify range of calibration values
  this->m_Hdr.dime.compressed = 0; // specify that the data file with extension
  // .img is not compressed
  this->m_Hdr.dime.verified = 0;
  this->m_Hdr.dime.glmax = 0;      // max value for all of the data set
  this->m_Hdr.dime.glmin = 0;      // min value for all of the data set

  /*data_history*/
  this->m_Hdr.hist.descrip[0] = '\0';
  this->m_Hdr.hist.aux_file[0] = '\0';
  /*Acceptable values are*/
  /*0-transverse unflipped*/
  /*1-coronal unflipped*/
  /*2-sagittal unfipped*/
  /*3-transverse flipped*/
  /*4-coronal flipped*/
  /*5-sagittal flipped*/
  // Default orientation is ITK_ANALYZE_TRANSVERSE
  this->m_Hdr.hist.orient =
    itk::AnalyzeImageIO::ITK_ANALYZE_ORIENTATION_RPI_TRANSVERSE;

  this->m_Hdr.hist.originator[0] = '\0';
  this->m_Hdr.hist.generated[0] = '\0';
  this->m_Hdr.hist.scannum[0] = '\0';
  this->m_Hdr.hist.patient_id[0] = '\0';
  this->m_Hdr.hist.exp_date[0] = '\0';
  this->m_Hdr.hist.exp_time[0] = '\0';
  this->m_Hdr.hist.hist_un0[0] = '\0';
  this->m_Hdr.hist.views = 0;
  this->m_Hdr.hist.vols_added = 0;
  this->m_Hdr.hist.start_field = 0;
  this->m_Hdr.hist.field_skip = 0;
  this->m_Hdr.hist.omax = 0;
  this->m_Hdr.hist.omin = 0;
  this->m_Hdr.hist.smax = 0;
  this->m_Hdr.hist.smin = 0;
  this->AddSupportedWriteExtension(".hdr");
  this->AddSupportedWriteExtension(".img");
  this->AddSupportedWriteExtension(".img.gz");
  this->AddSupportedReadExtension(".hdr");
  this->AddSupportedReadExtension(".img");
  this->AddSupportedReadExtension(".img.gz");
}

AnalyzeImageIO::~AnalyzeImageIO()
{
  //Purposefully left blank
}

void AnalyzeImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

bool AnalyzeImageIO::CanWriteFile(const char *FileNameToWrite)
{
  std::string filename(FileNameToWrite);
  // Data file name given?
  std::string::size_type imgPos = filename.rfind(".img");

  if ( ( imgPos != std::string::npos )
       && ( imgPos == filename.length() - 4 ) )
    {
    return true;
    }

  // Header file given?
  std::string::size_type hdrPos = filename.rfind(".hdr");
  if ( ( hdrPos != std::string::npos )
       && ( hdrPos == filename.length() - 4 ) )
    {
    return true;
    }

  // Compressed image given?
  std::string::size_type imggzPos = filename.rfind(".img.gz");
  if ( ( imggzPos != std::string::npos )
       && ( imggzPos == filename.length() - 7 ) )
    {
    return true;
    }

  return false;
}

//Set Data Type Values and min/max values
//////////////////////////////////////////////////////////////////////////
// Programmer: Hans J. Johnson
//       Date: 10/29/98
//   Function: DefineHeaderObjDataType
//  Algorithm: Set DataType Values appropriately
// Func. Ret.:
//     Output:
//      Input: DataTypeIndex - Is one of the following
//              DT_INDEX_UNSIGNED_CHAR
//              DT_INDEX_SIGNED_SHORT   DT_INDEX_SIGNED_INT
//              DT_INDEX_FLOAT          DT_INDEX_DOUBLE
//              DT_INDEX_COMPLEX        DT_INDEX_RGB
//              DT_INDEX_BINARY         DT_INDEX_UNKNOWN
//////////////////////////////////////////////////////////////////////////
void AnalyzeImageIO::DefineHeaderObjectDataType()
{
  enum DataTypeIndex eNewType;

  switch ( m_ComponentType )
    {
    case CHAR:
    case UCHAR:
      if ( this->GetPixelType() == RGB )
        {
        eNewType = ANALYZE_DT_INDEX_RGB;
        this->SetNumberOfComponents(3);
        }
      else
        {
        eNewType = ANALYZE_DT_INDEX_UNSIGNED_CHAR;
        }
      break;
    case SHORT:
      eNewType = ANALYZE_DT_INDEX_SIGNED_SHORT;
      break;
    case USHORT:
      eNewType = SPMANALYZE_DT_INDEX_UNSIGNED_SHORT;
      break;
    case INT:
      eNewType = ANALYZE_DT_INDEX_SIGNED_INT;
      break;
    case UINT:
      eNewType = SPMANALYZE_DT_INDEX_UNSIGNED_INT;
      break;
    case FLOAT:
      eNewType = ANALYZE_DT_INDEX_FLOAT;
      break;
    case DOUBLE:
      eNewType = ANALYZE_DT_INDEX_DOUBLE;
      break;
    //case DATA_COMPLEX_FLOAT:
    //  eNewType=ANALYZE_DT_INDEX_COMPLEX;
    //  break;
    //case DATA_RGBTRIPLE:
    //  eNewType=ANALYZE_DT_INDEX_RGB;
    //  break;
    //case DATA_BINARY:
    //  eNewType=ANALYZE_DT_INDEX_BINARY;
    //  break;
    //  case
    //       DATA_UNKNOWN:
    //        eNewType=ANALYZE_DT_INDEX_UNKNOWN;
    //  break;
    default:
      eNewType = ANALYZE_DT_INDEX_UNKNOWN;
      itkExceptionMacro(<< "Pixel Type Unknown");
    }
  m_Hdr.dime.datatype = DataTypeKey[eNewType];
  m_Hdr.dime.bitpix = DataTypeSizes[eNewType];
  strcpy(m_Hdr.hk.data_type, DataTypes[eNewType]);
  switch ( m_Hdr.dime.datatype )
    {
    case ANALYZE_DT_BINARY:
      m_Hdr.dime.glmax = 1;  /*max value for all of the data set*/
      m_Hdr.dime.glmin = 0;  /*min value for all of the data set*/
      break;
    case ANALYZE_DT_UNSIGNED_CHAR:
      m_Hdr.dime.glmax = 255; /*max value for all of the data set*/
      m_Hdr.dime.glmin = 0;   /*min value for all of the data set*/
      break;
    case ANALYZE_DT_SIGNED_SHORT:
      //m_Hdr.dime.glmax=0;/*max value for all of the data set*/
      //m_Hdr.dime.glmin=0;/*min value for all of the data set*/
      break;
    case ANALYZE_DT_FLOAT:
      //m_Hdr.dime.glmax=0;/*max value for all of the data set*/
      //m_Hdr.dime.glmin=0;/*min value for all of the data set*/
      break;
    case ANALYZE_DT_DOUBLE:
      //m_Hdr.dime.glmax=0;/*max value for all of the data set*/
      //m_Hdr.dime.glmin=0;/*min value for all of the data set*/
      break;
    case ANALYZE_DT_RGB:
      m_Hdr.dime.glmax = 255; /*max value for all of the data set*/
      m_Hdr.dime.glmin = 0;   /*min value for all of the data set*/
      break;
    default:
      m_Hdr.dime.glmax = 0;  /*max value for all of the
                             data set*/
      m_Hdr.dime.glmin = 0;  /*min value for all of
                             the data set*/
      break;
    }
}

void AnalyzeImageIO::Read(void *buffer)
{
  //4 cases to handle
  //1: given .hdr and image is .img
  //2: given .img
  //3: given .img.gz
  //4: given .hdr and image is .img.gz
  //   Special processing needed for this case onl
  // NOT NEEDED const std::string fileExt = GetExtension(m_FileName);

  /* Returns proper name for cases 1,2,3 */
  std::string ImageFileName = GetImageFileName(m_FileName);
  // NOTE: gzFile operations act just like FILE * operations when the
  // files are not in gzip fromat.This greatly simplifies the
  // following code, and gzFile types are used everywhere. In
  // addition, it has the added benefit of reading gzip compressed
  // image files that do not have a .gz ending.
  ///

  gzFile file_p = gzopen(ImageFileName.c_str(), "rb");

  try  // try block to ensure we close the file
    {
    if ( file_p == ITK_NULLPTR )
      {
      /* Do a separate check to take care of case #4 */
      ImageFileName += ".gz";
      file_p = gzopen(ImageFileName.c_str(), "rb");
      if ( file_p == ITK_NULLPTR )
        {
        itkExceptionMacro(<< "Analyze Data File can not be read: "
                          << " The following files were attempted:\n "
                          << GetImageFileName(m_FileName) << "\n"
                          << ImageFileName << "\n");
        }
      }

    // Apply the offset if any.
    // From itkAnalyzeDbh.h:
    // Byte offset in the .img file at which voxels start.
    // If value is negative specifies that the absolute value is
    // applied for every image in the file.
    z_off_t byteOffset = static_cast< z_off_t >( fabs(m_Hdr.dime.vox_offset) );
    if ( byteOffset > 0 )
      {
      if ( gzseek(file_p, byteOffset, SEEK_SET) == -1 )
        {
        itkExceptionMacro (<< "Analyze Data File can not be read: "
                           << " Unable to seek to the vox_offset: "
                           << byteOffset << "\n");
        }
      }

    // CAVEAT: gzread in particular only accepts "unsigned int" for the
    // number of bytes to read, thus limiting the amount which may be
    // read in a single operation on some platforms to a different limit
    // than the corresponding fread operation.

    //size of the maximum chunk to read , if the file is larger than this it
    // will be read as several chunks.
    //This is due to the limitation of 'unsigned int' in  the gzread()
    //  function.
    static const unsigned int maxChunk = ANALYZE_MAXIMUM_IO_CHUNK;

    char *p = static_cast< char * >( buffer );

    SizeType bytesRemaining = this->GetImageSizeInBytes();
    while ( bytesRemaining )
      {
      unsigned int bytesToRead = bytesRemaining > static_cast< SizeType >( maxChunk )
                                 ? maxChunk : static_cast< unsigned int >( bytesRemaining );

      int retval = gzread(file_p, p,  bytesToRead);

      //
      // check for error from gzread
      // careful .. due to unsigned/signed conversion the
      // real return value could be -1 if a chunk equal to 2^32-1 is allowed!
      //
      if ( retval != static_cast< int >( bytesToRead ) )
        {
        itkExceptionMacro(<< "Analyze Data File : gzread returned bad value: "
                          << retval << "\n");
        }

      p += bytesToRead;
      bytesRemaining -= bytesToRead;
      }

    gzclose(file_p);
    file_p = ITK_NULLPTR;
    SwapBytesIfNecessary( buffer, this->GetImageSizeInPixels() );
    }
  catch ( ... )
    {
    // close file and rethrow
    if ( file_p != ITK_NULLPTR )
      {
      gzclose(file_p);
      }
    throw;
    }
}

// This method will only test if the header looks like an
// Analyze Header.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool AnalyzeImageIO::CanReadFile(const char *FileNameToRead)
{
  std::string filename(FileNameToRead);
  // we check that the correct extension is given by the user
  std::string filenameext = GetExtension(filename);

  const char* filenameExtension = filenameext.c_str();

  if ( itksys::SystemTools::Strucmp(filenameExtension, ".hdr") != 0
       && itksys::SystemTools::Strucmp(filenameExtension, ".img.gz") != 0
       && itksys::SystemTools::Strucmp(filenameExtension, ".img") != 0 )
    {
    return false;
    }

  const std::string HeaderFileName = GetHeaderFileName(filename);

  std::ifstream local_InputStream;
  try
    {
    this->OpenFileForReading( local_InputStream, HeaderFileName );
    }
  catch( ExceptionObject & )
    {
    return false;
    }
  if ( !this->ReadBufferAsBinary( local_InputStream,
                                  (void *)&( this->m_Hdr ),
                                  sizeof( struct dsr ) ) )
    {
    local_InputStream.close();
    return false;
    }
  local_InputStream.close();

  // if the machine and file endianness are different
  // perform the byte swapping on it
  this->m_ByteOrder = this->CheckAnalyzeEndian(this->m_Hdr);
  this->SwapHeaderBytesIfNecessary( &( this->m_Hdr ) );
#ifdef OMIT_THIS_CODE
  //It is OK for this flag to be set because the zlib will
  //support the Unix compress files
  if ( this->m_Hdr.dime.compressed == 1 )
    {
    return false;
    //    ExceptionObject exception(__FILE__, __LINE__);
    //    exception.SetDescription("Unix compress file is not supported.");
    //    throw exception;
    }
#endif
  //The final check is to make sure that it is not a nifti
  // version of the analyze file.
  //Eventually the entire itkAnalyzeImageIO class will be
  //subsumed by the nifti reader.
  const bool NotNiftiTaggedFile = ( is_nifti_file(FileNameToRead) == 0 );
  return NotNiftiTaggedFile;
}

void AnalyzeImageIO::ReadImageInformation()
{
  unsigned int      dim;
  const std::string HeaderFileName = GetHeaderFileName(m_FileName);
  std::ifstream     local_InputStream;

  this->OpenFileForReading( local_InputStream, HeaderFileName );
  if ( !this->ReadBufferAsBinary( local_InputStream,
                                  (void *)&( this->m_Hdr ),
                                  sizeof( struct dsr ) ) )
    {
    itkExceptionMacro(<< "Unexpected end of file");
    }
  local_InputStream.close();

  // if the machine and file endianness are different
  // perform the byte swapping on it
  this->m_ByteOrder = this->CheckAnalyzeEndian(this->m_Hdr);
  if ( this->m_MachineByteOrder != this->m_ByteOrder  )
    {
    this->SwapHeaderBytesIfNecessary( &( this->m_Hdr ) );
    }

  // Check if any dimensions are 1. If they are, reduce dimensionality
  // This shouldn't be necessary, but Analyse75 seems to require the first
  // field to be 4. So when writing say a 50 x 27 2D image,
  //   m_Hdr.dime.dim[0] = 4;
  //   m_Hdr.dime.dim[1] = 50;
  //   m_Hdr.dime.dim[2] = 27;
  //   m_Hdr.dime.dim[3] = 1;
  //   m_Hdr.dime.dim[4] = 1;
  unsigned int numberOfDimensions = this->m_Hdr.dime.dim[0];
  if ( numberOfDimensions == 0 )
    {
    // If the dimension is 0, it may still contain valid dimension
    // values. Let's try to compute the number of dimensions from
    // other values. We will output a warning, but we may not want
    // to throw an exception already.
    const unsigned int maxNumberOfDimensions = 4;
    for ( unsigned int idx = 1;
          ( idx <= maxNumberOfDimensions ) && ( this->m_Hdr.dime.dim[idx] );
          idx++ )
      {
      if ( this->m_Hdr.dime.dim[idx] > 0 )
        {
        numberOfDimensions++;
        }
      else
        {
        itkWarningMacro("AnalyzeImageIO is ignoring dimension " << idx << " with value " <<  this->m_Hdr.dime.dim[idx]);
        }
      }
    }

  if ( numberOfDimensions == 0 )
    {
    itkExceptionMacro("AnalyzeImageIO cannot process file: "
                      << this->GetFileName()
                      << ". Number of dimensions is 0." << std::endl
                      << "hdr.dime[0] = " << m_Hdr.dime.dim[0] << std::endl
                      << "hdr.dime[1] = " << m_Hdr.dime.dim[1] << std::endl
                      << "hdr.dime[2] = " << m_Hdr.dime.dim[2] << std::endl
                      << "hdr.dime[3] = " << m_Hdr.dime.dim[3] << std::endl
                      << "hdr.dime[4] = " << m_Hdr.dime.dim[4] << std::endl);
    return;
    }
  while ( this->m_Hdr.dime.dim[numberOfDimensions] <= 1 )
    {
    --numberOfDimensions;
    }

  this->SetNumberOfDimensions(numberOfDimensions);
  this->SetNumberOfComponents(1); // default
  switch ( this->m_Hdr.dime.datatype )
    {
    case ANALYZE_DT_BINARY:
      m_ComponentType = CHAR;
      m_PixelType = SCALAR;
      break;
    case ANALYZE_DT_UNSIGNED_CHAR:
      m_ComponentType = UCHAR;
      m_PixelType = SCALAR;
      break;
    case ANALYZE_DT_SIGNED_SHORT:
      m_ComponentType = SHORT;
      m_PixelType = SCALAR;
      break;
    case SPMANALYZE_DT_UNSIGNED_SHORT:
      m_ComponentType = USHORT;
      m_PixelType = SCALAR;
      break;
    case ANALYZE_DT_SIGNED_INT:
      m_ComponentType = INT;
      m_PixelType = SCALAR;
      break;
    case SPMANALYZE_DT_UNSIGNED_INT:
      m_ComponentType = UINT;
      m_PixelType = SCALAR;
      break;
    case ANALYZE_DT_FLOAT:
      m_ComponentType = FLOAT;
      m_PixelType = SCALAR;
      break;
    case ANALYZE_DT_DOUBLE:
      m_ComponentType = DOUBLE;
      m_PixelType = SCALAR;
      break;
    case ANALYZE_DT_RGB:
      // DEBUG -- Assuming this is a triple, not quad
      //image.setDataType( uiig::DATA_RGBQUAD );
      m_ComponentType = UCHAR;
      m_PixelType = RGB;
      this->SetNumberOfComponents(3);
      break;
    default:
      break;
    }
  //
  // set up the dimension stuff
  for ( dim = 0; dim < this->GetNumberOfDimensions(); dim++ )
    {
    this->SetDimensions(dim, this->m_Hdr.dime.dim[dim + 1]);
    this->SetSpacing(dim, this->m_Hdr.dime.pixdim[dim + 1]);
    }
  //
  // figure out re-orientation required if not in Coronal
  this->ComputeStrides();

  //Get Dictionary Information
  //Insert Orientation.
  //  char temp[348];
  //Important hk fields.
  itk::MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
  std::string               classname( this->GetNameOfClass() );
  itk::EncapsulateMetaData< std::string >(thisDic, ITK_InputFilterName, classname);

  itk::EncapsulateMetaData< std::string >
    ( thisDic, ITK_ImageFileBaseName, std::string(this->m_Hdr.hk.db_name, 18) );

  //Important dime fields
  itk::EncapsulateMetaData< std::string >
    ( thisDic, ITK_VoxelUnits, std::string(this->m_Hdr.dime.vox_units, 4) );
  itk::EncapsulateMetaData< std::string >
    ( thisDic, ANALYZE_CALIBRATIONUNITS,
    std::string(this->m_Hdr.dime.cal_units, 8) );
  itk::EncapsulateMetaData< short int >
    (thisDic, ITK_OnDiskBitPerPixel, this->m_Hdr.dime.bitpix);
  itk::EncapsulateMetaData< float >
    (thisDic, SPM_ROI_SCALE, this->m_Hdr.dime.roi_scale);
  itk::EncapsulateMetaData< float >(thisDic, ANALYZE_CAL_MAX,
                                    this->m_Hdr.dime.cal_max);
  itk::EncapsulateMetaData< float >(thisDic, ANALYZE_CAL_MIN,
                                    this->m_Hdr.dime.cal_min);
  itk::EncapsulateMetaData< int >(thisDic, ANALYZE_GLMAX, this->m_Hdr.dime.glmax);
  itk::EncapsulateMetaData< int >(thisDic, ANALYZE_GLMIN, this->m_Hdr.dime.glmin);

  for ( dim = this->GetNumberOfDimensions(); dim > 0; dim-- )
    {
    if ( m_Hdr.dime.dim[dim] != 1 )
      {
      break;
      }
    }
  itk::EncapsulateMetaData< int >(thisDic, ITK_NumberOfDimensions, dim);


  //Important hist fields
  itk::EncapsulateMetaData< std::string >
    ( thisDic, ITK_FileNotes,
    std::string(this->m_Hdr.hist.descrip, 80) );
  itk::EncapsulateMetaData< std::string >
    ( thisDic, ANALYZE_AUX_FILE_NAME,
    std::string(this->m_Hdr.hist.aux_file, 24) );

  itk::AnalyzeImageIO::ValidAnalyzeOrientationFlags temporient =
    static_cast< itk::AnalyzeImageIO::ValidAnalyzeOrientationFlags >
    ( this->m_Hdr.hist.orient );
  itk::SpatialOrientation::ValidCoordinateOrientationFlags coord_orient;
  switch ( temporient )
    {
    case itk::AnalyzeImageIO::ITK_ANALYZE_ORIENTATION_RPI_TRANSVERSE:
      coord_orient = itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI;
      break;
    case itk::AnalyzeImageIO::ITK_ANALYZE_ORIENTATION_PIR_SAGITTAL:
      coord_orient = itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR;
      break;
    case itk::AnalyzeImageIO::ITK_ANALYZE_ORIENTATION_RIP_CORONAL:
      coord_orient = itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP;
      break;
    default:
      coord_orient = itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP;
      itkWarningMacro("Unknown orientation in file " << m_FileName);
    }
  // An error was encountered in code that depends upon the
  // valid coord_orientation.
  typedef SpatialOrientationAdapter OrientAdapterType;
  SpatialOrientationAdapter::DirectionType dir =
    OrientAdapterType().ToDirectionCosines(coord_orient);
  unsigned dims = this->GetNumberOfDimensions();
  // always have at least 3 dimensions for the purposes of
  // setting directions
#define itkAnalzyeImageIO_MINDIMS_IS_THREE ( ( dims < 3 ) ? 3 : dims )
  std::vector< double > dirx(itkAnalzyeImageIO_MINDIMS_IS_THREE, 0),
  diry(itkAnalzyeImageIO_MINDIMS_IS_THREE, 0),
  dirz(itkAnalzyeImageIO_MINDIMS_IS_THREE, 0);
#undef itkAnalzyeImageIO_MINDIMS_IS_THREE

  dirx[0] = dir[0][0];
  dirx[1] = dir[1][0];
  dirx[2] = dir[2][0];
  diry[0] = dir[0][1];
  diry[1] = dir[1][1];
  diry[2] = dir[2][1];
  dirz[0] = dir[0][2];
  dirz[1] = dir[1][2];
  dirz[2] = dir[2][2];
  for ( unsigned i = 3; i < dims; i++ )
    {
    dirx[i] = diry[i] = dirz[i] = 0;
    }
  this->SetDirection(0, dirx);
  this->SetDirection(1, diry);
  if ( numberOfDimensions > 2 )
    {
    this->SetDirection(2, dirz);
    }
  else
    {
    //
    // don't allow degenerate direction cosines
    // This is a pure punt; it will prevent the exception being
    // thrown, but doesn't do the right thing at all -- replacing e.g
    // [0, 0, -1] with [0, 1] doesn't make any real sense.
    // On the other hand, programs that depend on 2D Direction Cosines
    // are pretty much guaranteed to be disappointed if they expect anything
    // meaningful in the direction cosines anyway.
    if ( dirx[0] == 0.0 && dirx[1] == 0.0 )
      {
      if ( diry[0] != 0 )
        {
        dirx[1] = 1.0;
        }
      else
        {
        dirx[0] = 1.0;
        }
      }
    else if ( diry[0] == 0.0 && diry[1] == 0.0 )
      {
      if ( dirx[0] != 0 )
        {
        diry[1] = 1.0;
        }
      else
        {
        diry[0] = 1.0;
        }
      }
    }

  itk::EncapsulateMetaData< std::string >
    ( thisDic, ITK_FileOriginator,
    std::string(this->m_Hdr.hist.originator, 10) );
  itk::EncapsulateMetaData< std::string >
    ( thisDic, ITK_OriginationDate,
    std::string(this->m_Hdr.hist.generated, 10) );
  itk::EncapsulateMetaData< std::string >
    ( thisDic, ANALYZE_ScanNumber,
    std::string(this->m_Hdr.hist.scannum, 10) );
  itk::EncapsulateMetaData< std::string >
    ( thisDic, ITK_PatientID,
    std::string(this->m_Hdr.hist.patient_id, 10) );
  itk::EncapsulateMetaData< std::string >
    ( thisDic, ITK_ExperimentDate,
    std::string(this->m_Hdr.hist.exp_date, 10) );
  itk::EncapsulateMetaData< std::string >
    ( thisDic, ITK_ExperimentTime,
    std::string(this->m_Hdr.hist.exp_time, 10) );

  itk::EncapsulateMetaData< int >
    (thisDic, ANALYZE_O_MAX,
    this->m_Hdr.hist.omax);
  itk::EncapsulateMetaData< int >
    (thisDic, ANALYZE_O_MIN,
    this->m_Hdr.hist.omin);
  itk::EncapsulateMetaData< int >
    (thisDic, ANALYZE_S_MAX,
    this->m_Hdr.hist.smax);
  itk::EncapsulateMetaData< int >
    (thisDic, ANALYZE_S_MIN,
    this->m_Hdr.hist.smin);
}

/**
 *
 */
void
AnalyzeImageIO
::WriteImageInformation(void)
{
  // First of all we need to not go any further if there's
  // a dimension of the image that won't fit in a 16 bit short.
  for ( unsigned int i = 0; i < this->GetNumberOfDimensions(); i++ )
    {
    unsigned int curdim( this->GetDimensions(i) );
    if ( curdim > static_cast< unsigned int >( NumericTraits< short >::max() ) )
      {
      itkExceptionMacro( << "Dimension(" << i << ") = " << curdim
                         << " is greater than maximum possible dimension "
                         << NumericTraits< short >::max() );
      }
    }
  if ( this->GetPixelType() == RGB )
    {
    if ( this->GetComponentType() != UCHAR )
      {
      itkExceptionMacro(<< "Only unsigned char RGB files supported");
      }
    }
  else
    {
    if ( this->GetNumberOfComponents() > 1 )
      {
      itkExceptionMacro(<< "More than one component per pixel not supported");
      }
    }

  const std::string HeaderFileName = GetHeaderFileName(m_FileName);
  std::ofstream     local_OutputStream;
  this->OpenFileForWriting( local_OutputStream, HeaderFileName );
  std::string temp;
  //
  // most likely NONE of this below does anything useful because
  // the MetaDataDictionary is basically not set with any of these fields
  // except by the image file reader.
  //Important hk fields.
  itk::MetaDataDictionary & thisDic = this->GetMetaDataDictionary();

  if ( itk::ExposeMetaData< std::string >(thisDic, ITK_ImageFileBaseName, temp) )
    {
    strncpy(this->m_Hdr.hk.db_name, temp.c_str(), 18);
    }
  //Important dime fields
  if ( itk::ExposeMetaData< std::string >(thisDic, ITK_VoxelUnits, temp) )
    {
    strncpy(this->m_Hdr.dime.vox_units, temp.c_str(), 4);
    }

  if ( itk::ExposeMetaData< std::string >(thisDic, ANALYZE_CALIBRATIONUNITS, temp) )
    {
    strncpy(this->m_Hdr.dime.cal_units, temp.c_str(), 8);
    }

  itk::ExposeMetaData< short int >
    (thisDic, ITK_OnDiskBitPerPixel,
    this->m_Hdr.dime.bitpix);
  itk::ExposeMetaData< float >
    (thisDic, SPM_ROI_SCALE,
    this->m_Hdr.dime.roi_scale);
  itk::ExposeMetaData< float >
    (thisDic, ANALYZE_CAL_MAX,
    this->m_Hdr.dime.cal_max);
  itk::ExposeMetaData< float >
    (thisDic, ANALYZE_CAL_MIN,
    this->m_Hdr.dime.cal_min);
  itk::ExposeMetaData< int >
    (thisDic, ANALYZE_GLMAX,
    this->m_Hdr.dime.glmax);
  itk::ExposeMetaData< int >
    (thisDic, ANALYZE_GLMIN,
    this->m_Hdr.dime.glmin);
  //Important hist fields
  if ( itk::ExposeMetaData< std::string >(thisDic, ITK_FileNotes, temp) )
    {
    strncpy(this->m_Hdr.hist.descrip, temp.c_str(), 80);
    }

  if ( itk::ExposeMetaData< std::string >(thisDic, ANALYZE_AUX_FILE_NAME, temp) )
    {
    strncpy(this->m_Hdr.hist.aux_file, temp.c_str(), 24);
    }

  itk::SpatialOrientation::ValidCoordinateOrientationFlags coord_orient =
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_INVALID;
  typedef itk::SpatialOrientationAdapter::DirectionType DirectionType;
  DirectionType dir;
  unsigned int dims = this->GetNumberOfDimensions();
  std::vector< double > dirx = this->GetDirection(0);
  std::vector< double > diry = this->GetDirection(1);
  std::vector< double > dirz;
  if ( dims > 2 )
    {
    dirz = this->GetDirection(2);
    }
  else
    {
    for ( unsigned i = 0; i < 3; i++ )
      {
      dirz.push_back(0.0);
      }
    }
    {
    unsigned int dirIndex=0;
    while (  dirIndex < dims  )
      {
      dir[dirIndex][0] = dirx[dirIndex];
      dir[dirIndex][1] = diry[dirIndex];
      dir[dirIndex][2] = dirz[dirIndex];
      dirIndex++;
      }
    while (dirIndex < 3 )
      { //Fill out from 1D or 2D to 3D
      dir[dirIndex][0] = 0;
      dir[dirIndex][1] = 0;
      dir[dirIndex][2] = 0;
      dirIndex++;
      }
    }
  coord_orient =
    itk::SpatialOrientationAdapter().FromDirectionCosines(dir);
  switch ( coord_orient )
    {
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
      this->m_Hdr.hist.orient =
        itk::AnalyzeImageIO::ITK_ANALYZE_ORIENTATION_RPI_TRANSVERSE;
      break;
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
      this->m_Hdr.hist.orient =
        itk::AnalyzeImageIO::ITK_ANALYZE_ORIENTATION_PIR_SAGITTAL;
      break;
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
      this->m_Hdr.hist.orient =
        itk::AnalyzeImageIO::ITK_ANALYZE_ORIENTATION_RIP_CORONAL;
      break;
    default:
      this->m_Hdr.hist.orient =
        itk::AnalyzeImageIO::ITK_ANALYZE_ORIENTATION_RIP_CORONAL;
      itkWarningMacro("ERROR: Analyze 7.5 File Format"
                      " Only Allows RPI, PIR, and RIP Orientation ");
    }

  if ( itk::ExposeMetaData< std::string >(thisDic, ITK_FileOriginator, temp) )
    {
    strncpy(this->m_Hdr.hist.originator, temp.c_str(), 10);
    }

  if ( itk::ExposeMetaData< std::string >(thisDic, ITK_OriginationDate, temp) )
    {
    strncpy(this->m_Hdr.hist.generated, temp.c_str(), 10);
    }

  if ( itk::ExposeMetaData< std::string >(thisDic, ANALYZE_ScanNumber, temp) )
    {
    strncpy(this->m_Hdr.hist.scannum, temp.c_str(), 10);
    }

  if ( itk::ExposeMetaData< std::string >(thisDic, ITK_PatientID, temp) )
    {
    strncpy(this->m_Hdr.hist.patient_id, temp.c_str(), 10);
    }

  if ( itk::ExposeMetaData< std::string >(thisDic, ITK_ExperimentDate, temp) )
    {
    strncpy(this->m_Hdr.hist.exp_date, temp.c_str(), 10);
    }

  if ( itk::ExposeMetaData< std::string >(thisDic, ITK_ExperimentTime, temp) )
    {
    strncpy(this->m_Hdr.hist.exp_time, temp.c_str(), 10);
    }

  itk::ExposeMetaData< int >(thisDic, ANALYZE_O_MAX, this->m_Hdr.hist.omax);
  itk::ExposeMetaData< int >(thisDic, ANALYZE_O_MIN, this->m_Hdr.hist.omin);
  itk::ExposeMetaData< int >(thisDic, ANALYZE_S_MAX, this->m_Hdr.hist.smax);
  itk::ExposeMetaData< int >(thisDic, ANALYZE_S_MIN, this->m_Hdr.hist.smin);

  // Check for image dimensions to be smaller enough to fit in
  // a short int. First generate the number that is the maximum allowable.
  const SizeValueType maximumNumberOfPixelsAllowedInOneDimension =
    itk::NumericTraits< short >::max();

  for ( unsigned int dim = 0; dim < this->GetNumberOfDimensions(); dim++ )
    {
    const SizeValueType numberOfPixelsAlongThisDimension = m_Dimensions[dim];

    if ( numberOfPixelsAlongThisDimension > maximumNumberOfPixelsAllowedInOneDimension )
      {
      itkExceptionMacro("Number of pixels along dimension " << dim
                                                            << " is " << numberOfPixelsAlongThisDimension
                                                            << " which exceeds maximum allowable dimension of "
                                                            << maximumNumberOfPixelsAllowedInOneDimension);
      }

    //NOTE: Analyze dim[0] are the number of dims, and dim[1..7] are
    // the actual dims.
    this->m_Hdr.dime.dim[dim + 1]  = numberOfPixelsAlongThisDimension;
    }

  //DEBUG--HACK It seems that analyze 7.5 requires 4 dimensions.
  this->m_Hdr.dime.dim[0] = 4;
  for ( unsigned int dim = this->GetNumberOfDimensions(); (int)dim < this->m_Hdr.dime.dim[0];
        dim++ )
    {
    //NOTE: Analyze dim[0] are the number of dims,
    //and dim[1..7] are the actual dims.
    this->m_Hdr.dime.dim[dim + 1]  = 1; //Hardcoded to be 1;
    }
  for ( unsigned int dim = 0; dim < this->GetNumberOfDimensions(); dim++ )
    {
    //NOTE: Analyze pixdim[0] is ignored, and the number of dims are
    //taken from dims[0], and pixdim[1..7] are the actual pixdims.
    this->m_Hdr.dime.pixdim[dim + 1] = static_cast< float >( m_Spacing[dim] );
    }
  //The next funciton sets bitpix, and datatype, and data_type fields
  //Along with gl_min and gl_max fields.
  this->DefineHeaderObjectDataType();

  local_OutputStream.write( (const char *)&( this->m_Hdr ), sizeof( struct dsr ) );
  if ( local_OutputStream.eof() )
    {
    itkExceptionMacro(<< "Unexpected end of file");
    }
  local_OutputStream.close();
}

/** Return the directions that this particular ImageIO would use by default
 *  in the case the recipient image dimension is smaller than the dimension
 *  of the image in file. */
std::vector< double >
AnalyzeImageIO
::GetDirection(unsigned int k) const
{
  std::vector< double > correctedDirection = this->ImageIOBase::GetDirection(k);
  if ( this->m_Dimensions.size() == correctedDirection.size() )
    {
    return correctedDirection;
    }

  // Apply corrections for 2D cases
  std::vector< double > direction0 = this->ImageIOBase::GetDirection(0);
  std::vector< double > direction1 = this->ImageIOBase::GetDirection(1);

  if ( k == 0 )
    {
    if ( direction0[0] == 0.0 && direction1[0] == 0 )
      {
      if ( direction0[1] == 0.0 )
        {
        correctedDirection[0] = 1.0;
        }
      }
    }

  if ( k == 1 )
    {
    if ( direction0[0] == 0.0 && direction1[0] == 0 )
      {
      if ( direction1[1] == 0.0 )
        {
        correctedDirection[0] = 1.0;
        }
      }
    else if ( direction0[1] == 0.0 && direction1[1] == 0 )
      {
      if ( direction0[0] == 0.0 )
        {
        correctedDirection[0] = 1.0;
        }
      if ( direction1[0] == 0.0 )
        {
        correctedDirection[1] = 1.0;
        }
      }
    }

  return correctedDirection;
}

/** Return the directions that this particular ImageIO would use by default
 *  in the case the recipient image dimension is smaller than the dimension
 *  of the image in file. */
std::vector< double >
AnalyzeImageIO
::GetDefaultDirection(unsigned int k) const
{
  std::vector< double > defaultDirection = this->ImageIOBase::GetDefaultDirection(k);

  // Apply corrections for 2D cases
  std::vector< double > direction0 = this->GetDirection(0);
  std::vector< double > direction1 = this->GetDirection(1);

  if ( k == 0 )
    {
    if ( direction0[0] == 0.0 && direction1[0] == 0 )
      {
      if ( direction0[1] == 0.0 )
        {
        defaultDirection[0] = 1.0;
        }
      }
    }

  if ( k == 1 )
    {
    if ( direction0[0] == 0.0 && direction1[0] == 0 )
      {
      if ( direction1[1] == 0.0 )
        {
        defaultDirection[0] = 1.0;
        }
      }
    else if ( direction0[1] == 0.0 && direction1[1] == 0 )
      {
      if ( direction0[0] == 0.0 )
        {
        defaultDirection[0] = 1.0;
        }
      if ( direction1[0] == 0.0 )
        {
        defaultDirection[1] = 1.0;
        }
      }
    }

  return defaultDirection;
}

/**
 * This method writes the content of the image buffer to disk.
 */
void
AnalyzeImageIO
::Write(const void *buffer)
{
  //Write the image Information before writing data
  this->WriteImageInformation();

  //NOTE: voidp is defined by zlib.h
  //NOTE: Need const_cast because voidp is "void*", so
  //      "const voidp" is "void* const", not "const void*".
  voidp             p = const_cast< voidp >( buffer );
  const std::string ImageFileName = GetImageFileName(m_FileName);
  const std::string fileExt = GetExtension(m_FileName);
  // Check case where image is acually a compressed image

  if ( !fileExt.compare(".img.gz") )
    {
    // Open the *.img.gz file for writing.
    gzFile file_p = gzopen(ImageFileName.c_str(), "wb");
    if ( file_p == ITK_NULLPTR )
      {
      itkExceptionMacro(<< "Error, Can not write compressed image file for " << m_FileName);
      }

    try // try block to ensure file gets closed
      {
      // CAVEAT: gzwrite in particular only accepts "unsigned int" for the
      // number of bytes to read, thus limiting the amount which may be
      // read in a single operation on some platforms to a different limit
      // than the corresponding fread operation.

      static const unsigned int maxChunk = ANALYZE_MAXIMUM_IO_CHUNK;

      SizeType bytesRemaining = this->GetImageSizeInBytes();
      while ( bytesRemaining )
        {
        unsigned int bytesToWrite = bytesRemaining > static_cast< SizeType >( maxChunk )
                                    ? maxChunk : static_cast< unsigned int >( bytesRemaining );

        if ( gzwrite(file_p, p, bytesToWrite) != static_cast< int >( bytesToWrite ) )
          {
          itkExceptionMacro(<< "Error, Can not write compressed image file for " << m_FileName);
          }
        p = static_cast< char * >( p ) + bytesToWrite;
        bytesRemaining -= bytesToWrite;
        }
      gzclose(file_p);
      file_p = ITK_NULLPTR;
      }
    catch ( ... )
      {
      // close file and rethrow exception
      if ( file_p != ITK_NULLPTR )
        {
        gzclose(file_p);
        }
      throw;
      }

    //RemoveFile FileNameToRead.img so that it does not get confused with
    //FileNameToRead.img.gz
    //The following is a hack that can be used to remove ambiguity when an
    //uncompressed image is read, and then written as compressed.
    //This results in one *.hdr file being assosiated with a *.img and a
    // *.img.gz image file.
    //DEBUG -- Will this work under windows?
    std::string unusedbaseimgname = GetRootName( GetHeaderFileName(m_FileName) );
    unusedbaseimgname += ".img";
    itksys::SystemTools::RemoveFile( unusedbaseimgname.c_str() );
    }
  else
    {
    //No compression
    std::ofstream local_OutputStream;
    this->OpenFileForWriting( local_OutputStream, ImageFileName );
    local_OutputStream.write( (const char *)p, static_cast< std::streamsize >( this->GetImageSizeInBytes() ) );
    bool success = !local_OutputStream.bad();
    local_OutputStream.close();
    if ( !success )
      {
      itkExceptionMacro(<< "Error writing image data."
                        << m_FileName);
      }
    //RemoveFile FileNameToRead.img.gz so that it does not get confused with
    // FileNameToRead.img
    //The following is a hack that can be used to remove ambiguity when an
    //uncompressed image is read, and then written as compressed.
    //This results in one *.hdr file being assosiated with a *.img and a
    // *.img.gz image file.
    //DEBUG -- Will this work under windows?
    std::string unusedbaseimgname = GetRootName( GetHeaderFileName(m_FileName) );
    unusedbaseimgname += ".img.gz";
    itksys::SystemTools::RemoveFile( unusedbaseimgname.c_str() );
    }
}
} // end namespace itk
