/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLSMImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkLSMImageIO.h"
#include "itkByteSwapper.h"
#include <string.h> // for memset
#include <itksys/FundamentalType.h>

extern "C" {
#include "tiffio.h"
}
/* Structure with LSM-specific data ( only in the first image directory). */
#define TIF_CZ_LSMINFO 34412 /* 0x866c, Type: TIF_BYTE, Length: 512 */
#define TIF_CZ_LSMINFO_SIZE_RESERVED 90+6
#define TIF_CZ_LSMINFO_SIZE 512

namespace itk
{
extern "C"
{
  static void TagExtender(TIFF *tiff)
  {
    static const TIFFFieldInfo xtiffFieldInfo[] = {
        { TIF_CZ_LSMINFO, TIFF_VARIABLE, TIFF_VARIABLE, TIFF_BYTE,
        FIELD_CUSTOM, 0, 1, (char*)"LSM Private Tag" }
    };
  
    TIFFMergeFieldInfo( tiff, xtiffFieldInfo,
      sizeof(xtiffFieldInfo) / sizeof(xtiffFieldInfo[0]) );
  }
}

typedef itksysFundamentalType_Int32 int32_t;
typedef itksysFundamentalType_UInt32 uint32_t;

typedef float       float32_t;
typedef double      float64_t;
typedef long double float96_t;

typedef struct {
  uint32_t    u32MagicNumber;
  int32_t     s32StructureSize;
  int32_t     s32DimensionX;
  int32_t     s32DimensionY;
  int32_t     s32DimensionZ;
  int32_t     s32DimensionChannels;
  int32_t     s32DimensionTime;
  int32_t     s32DataType;
  int32_t     s32ThumbnailX;
  int32_t     s32ThumbnailY;
  float64_t   f64VoxelSizeX;
  float64_t   f64VoxelSizeY;
  float64_t   f64VoxelSizeZ;
  uint32_t    u32ScanType;
  uint32_t    u32DataType;
  uint32_t    u32OffsetVectorOverlay;
  uint32_t    u32OffsetInputLut;
  uint32_t    u32OffsetOutputLut;
  uint32_t    u32OffsetChannelColors;
  float64_t   f64TimeIntervall;
  uint32_t    u32OffsetChannelDataTypes;
  uint32_t    u32OffsetScanInformation;
  uint32_t    u32OffsetKsData;
  uint32_t    u32OffsetTimeStamps;
  uint32_t    u32OffsetEventList;
  uint32_t    u32OffsetRoi;
  uint32_t    u32OffsetBleachRoi;
  uint32_t    u32OffsetNextRecording;
  uint32_t    u32Reserved [ TIF_CZ_LSMINFO_SIZE_RESERVED ];
} zeiss_info;

LSMImageIO::LSMImageIO()
{
  m_ByteOrder = LittleEndian;
  m_FileType = Binary;
}

LSMImageIO::~LSMImageIO()
{
}


// This method will only test if the header looks like a
// LSM image file.
bool LSMImageIO::CanReadFile(const char* filename) 
{ 
  std::ifstream file;
  std::string fname(filename);

  if( fname == "" )
    {
    itkDebugMacro(<<"No filename specified.");
    return false;
    }

  bool extensionFound = false;
  std::string::size_type sprPos = fname.rfind(".lsm");
  if ((sprPos != std::string::npos)
      && (sprPos == fname.length() - 4))
    {
    extensionFound = true;
    }
  sprPos = fname.rfind(".LSM");
  if ((sprPos != std::string::npos)
      && (sprPos == fname.length() - 4))
    {
    extensionFound = true;
    }

  if( !extensionFound )
    {
    itkDebugMacro(<<"The filename extension is not recognized");
    return false;
    }

  // Check that TIFFImageIO can read this file:
  TIFFErrorHandler save = TIFFSetWarningHandler(0); // get rid of warning about Zeiss tag
  if( !this->TIFFImageIO::CanReadFile(filename) )
    {
    return false;
    }
  TIFFSetWarningHandler(save);

  // Check this is indeed a LSM file
  if( !this->CanFindTIFFTag( TIF_CZ_LSMINFO ) )
    {
    return false;
    }

  // everything was ok
  return true;
}

void LSMImageIO::Read(void* buffer)
{
  this->TIFFImageIO::Read(buffer);
}

void LSMImageIO::ReadImageInformation()
{
  this->TIFFImageIO::ReadImageInformation();

  // Now is a good time to check what was read and replaced it with LSM
  // information
  void *praw = this->TIFFImageIO::ReadRawByteFromTag( TIF_CZ_LSMINFO );
  // FIXME byte swap
  ByteSwapper<unsigned short>::SwapRangeFromSystemToLittleEndian(
    reinterpret_cast<unsigned short*>(praw), TIF_CZ_LSMINFO_SIZE) ;
  const zeiss_info *zi = reinterpret_cast<zeiss_info*>(praw);
  if( sizeof(*zi) != TIF_CZ_LSMINFO_SIZE)
    {
    itkExceptionMacro( << "Problem of alignement on your plateform" );
    return;
    }
  m_Spacing[0] = zi->f64VoxelSizeX;
  m_Spacing[1] = zi->f64VoxelSizeY;
  // TIFF only support 2 or 3 dimension:
  if ( m_NumberOfDimensions == 3 )
    {
    m_Spacing[2] = zi->f64VoxelSizeZ;
    }
}


bool LSMImageIO::CanWriteFile( const char* name )
{
  std::string filename = name;

  if (filename == "")
    {
    return false;
    }
  
  std::string::size_type pos = filename.rfind(".lsm");
  if ( (pos != std::string::npos)
       && (pos == filename.length() - 4) )
    {
    return true;
    }

  pos = filename.rfind(".LSM");
  if ( (pos != std::string::npos)
       && (pos == filename.length() - 4) )
    {
    return true;
    }
 
  return false;
}

void LSMImageIO::FillZeissStruct(char *cz)
{
  memset(cz, 0, TIF_CZ_LSMINFO_SIZE); // fill with 0
  zeiss_info *z = reinterpret_cast<zeiss_info*>(cz);
  z->u32MagicNumber = 0x0400494c;
  z->s32StructureSize = TIF_CZ_LSMINFO_SIZE;
  z->s32DimensionX = m_Dimensions[0];
  z->s32DimensionY = m_Dimensions[1];
  if( m_NumberOfDimensions == 3 )
    {
    z->s32DimensionZ = m_Dimensions[2];
    }
  z->s32DimensionChannels = m_NumberOfComponents;
  z->s32DimensionTime = 1;
  z->s32DataType = 0;
  z->s32ThumbnailX = 128*m_Dimensions[0]/m_Dimensions[1];
  z->s32ThumbnailY = 128;
  z->f64VoxelSizeX = m_Spacing[0];
  z->f64VoxelSizeY = m_Spacing[1];
  if( m_NumberOfDimensions == 3 )
    {
    z->f64VoxelSizeZ = m_Spacing[2];
    }
}

void LSMImageIO::Write(const void* buffer)
{
  unsigned char *outPtr = (unsigned char *) buffer;

  unsigned int width, height, page, pages = 1;
  width =  m_Dimensions[0];
  height = m_Dimensions[1];  
  if( m_NumberOfDimensions == 3 )
    {
    pages = m_Dimensions[2];
    }

  int scomponents = this->GetNumberOfComponents();
  double resolution = -1;
  int bps;

  switch (this->GetComponentType())
    {
  case UCHAR:
    bps = 8;
    break;

  case USHORT:
    bps = 16;
    break;

  default:
    itkExceptionMacro(<<"TIFF supports unsigned char and unsigned short");
    }

  int predictor;

  TIFF *tif = TIFFOpen(m_FileName.c_str(), "w");
  if ( !tif )
    {
    itkDebugMacro( << "Returning" );
    return;
    }

  uint32 w = width;
  uint32 h = height;

  TIFFSetTagExtender(TagExtender);
  if( m_NumberOfDimensions == 3)
    {
    TIFFCreateDirectory( tif );
    }
  for (page = 0; page < pages; page++)
    {
    TIFFSetDirectory( tif, page );
    TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, w);
    TIFFSetField(tif, TIFFTAG_IMAGELENGTH, h);
    TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, scomponents);
    TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, bps); // Fix for stype
    TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
    char zeiss[TIF_CZ_LSMINFO_SIZE];
    FillZeissStruct(zeiss);
    unsigned int iCount = sizeof(zeiss)/sizeof(zeiss[0]);
    // Zeiss field is only on the first TIFF image
    if( page == 0)
      {
      TIFFSetField(tif, TIF_CZ_LSMINFO, iCount, zeiss);
      }


    if ( scomponents > 3 )
      {
      // if number of scalar components is greater than 3, that means we assume
      // there is alpha.
      uint16 extra_samples = scomponents-3;
      uint16 *sample_info = new uint16[scomponents-3];
      sample_info[0]=EXTRASAMPLE_ASSOCALPHA;
      int cc;
      for ( cc = 1; cc < scomponents-3; cc ++ )
        {
        sample_info[cc] = EXTRASAMPLE_UNSPECIFIED;
        }
      TIFFSetField(tif,TIFFTAG_EXTRASAMPLES,extra_samples,
        sample_info);
      delete [] sample_info;
      }

    int compression;

    if(m_UseCompression)
      {
      switch ( m_Compression )
        {
      case TIFFImageIO::PackBits: compression = COMPRESSION_PACKBITS; break;
      case TIFFImageIO::JPEG:     compression = COMPRESSION_JPEG; break;
      case TIFFImageIO::Deflate:  compression = COMPRESSION_DEFLATE; break;
      case TIFFImageIO::LZW:      compression = COMPRESSION_LZW; break;
      default: compression = COMPRESSION_NONE;
        }
      }
    else
      {
      compression = COMPRESSION_NONE;
      }

    TIFFSetField(tif, TIFFTAG_COMPRESSION, compression); // Fix for compression

    uint16 photometric = (scomponents ==1) ? PHOTOMETRIC_MINISBLACK : PHOTOMETRIC_RGB;

    if ( compression == COMPRESSION_JPEG )
      {
      TIFFSetField(tif, TIFFTAG_JPEGQUALITY, 75); // Parameter
      TIFFSetField(tif, TIFFTAG_JPEGCOLORMODE, JPEGCOLORMODE_RGB);
      photometric = PHOTOMETRIC_YCBCR;
      }
    else if ( compression == COMPRESSION_LZW )
      {
      predictor = 2;
      TIFFSetField(tif, TIFFTAG_PREDICTOR, predictor);
      itkDebugMacro( << "LZW compression is patented outside US so it is disabled" );
      }
    else if ( compression == COMPRESSION_DEFLATE )
      {
      predictor = 2;
      TIFFSetField(tif, TIFFTAG_PREDICTOR, predictor);
      }

    TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, photometric); // Fix for scomponents

    if (resolution > 0)
      {
      TIFFSetField(tif, TIFFTAG_XRESOLUTION, resolution);
      TIFFSetField(tif, TIFFTAG_YRESOLUTION, resolution);
      TIFFSetField(tif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_INCH);
      }

    if( m_NumberOfDimensions == 3)
      {
      // We are writing single page of the multipage file
      TIFFSetField(tif, TIFFTAG_SUBFILETYPE, FILETYPE_PAGE);
      }
    int rowLength; // in bytes

    switch (this->GetComponentType())
      {
    case UCHAR:
      rowLength = sizeof(unsigned char); 
      break;
    case USHORT:
      rowLength = sizeof(unsigned short);
      break;
    default:
      itkExceptionMacro(<<"TIFF supports unsigned char and unsigned short");
      }

    rowLength *= this->GetNumberOfComponents();
    rowLength *= width;

    int row = 0;
    for (unsigned int idx2 = 0; idx2 < height; idx2++)
      {
      if ( TIFFWriteScanline(tif, const_cast<unsigned char*>(outPtr), row, 0) < 0)
        {
        itkExceptionMacro( << "TIFFImageIO: error out of disk space" );
        break;
        }
      outPtr += rowLength;
      row ++;
      }

    if(m_NumberOfDimensions == 3 )
      {
      TIFFWriteDirectory(tif);
      }
    }
  TIFFClose(tif);
}

void LSMImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk
