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
#include "itkGE5ImageIO.h"
#include "itkByteSwapper.h"
#include "itksys/SystemTools.hxx"
#include "Ge5xHdr.h"
#include <algorithm>
#include <iostream>
#include <fstream>

#include "vnl/vnl_cross.h"


//From uiig library "The University of Iowa Imaging Group-UIIG"

namespace itk
{
static ITK_CONSTEXPR_VAR char GE_PROD_STR[] = "SIGNA";
// Default constructor
GE5ImageIO::GE5ImageIO()
{}

GE5ImageIO::~GE5ImageIO()
{
  //Purposefully left blank
}

int GE5ImageIO
::CheckGE5xImages(char const *const imageFileTemplate, std::string & reason)
{
  //
  // Does it exist?
  if ( !itksys::SystemTools::FileExists(imageFileTemplate) )
    {
    reason = "File does not exist";
    return -1;
    }
  //
  // is it at least 5000 bytes?
  if ( itksys::SystemTools::FileLength(imageFileTemplate) < 5000 )
    {
    reason = "File size is less than 5000 bytes";
    return -1;
    }

  std::ifstream f;
  try
    {
    this->OpenFileForReading( f, imageFileTemplate );
    }
  catch( ExceptionObject & )
    {
    reason = "File could not be opened for read";
    return -1;
    }

  Ge5xPixelHeader imageHdr;                /* Header Structure for GE 5x images
                                             */
  char            hdr[GENESIS_SU_HDR_LEN]; /* Header to hold GE Suite header */
  char            prod[16];                /* Product name from Suite Header */

  // First pass see if image is a raw MR extracted via ximg
  if ( !this->ReadBufferAsBinary( f, (void *)&imageHdr, sizeof( imageHdr ) ) )
    {
    f.close();
    return -1;
    }
  ByteSwapper< int >::SwapFromSystemToBigEndian(&imageHdr.GENESIS_IH_img_magic);
  if ( imageHdr.GENESIS_IH_img_magic == GE_5X_MAGIC_NUMBER )
    {
    f.close();
    return 0;
    }
  f.seekg(0, std::ios::beg);

  //
  // Second pass see if image was extracted via tape by Gene's tape
  // reading software.
  //
  if ( !this->ReadBufferAsBinary(f, (void *)hdr, GENESIS_SU_HDR_LEN) )
    {
    reason = "Failed to read study header";
    f.close();
    return -1;
    }
  strncpy (prod, hdr + GENESIS_SU_PRODID, 13);
  prod[13] = '\0';
  if ( strcmp (prod, GE_PROD_STR) == 0 )
    {
    f.close();
    return 0;
    }

  reason = "Failed to find string SIGNA";
  f.close();
  return -1;
}

bool GE5ImageIO::CanReadFile(const char *FileNameToRead)
{
  std::string reason;

  return this->CheckGE5xImages(FileNameToRead, reason) == 0 ? true : false;
}

namespace
{
void
SwapPixHdr(Ge5xPixelHeader *hdr)
{
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_magic ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_hdr_length ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_width ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_height ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_depth ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_compress ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_dwindow ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_dlevel ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_bgshade ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_ovrflow ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_undflow ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_top_offset ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_bot_offset ) );
  ByteSwapper< short >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_version ) );
  ByteSwapper< unsigned short >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_checksum ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_p_id ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_l_id ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_p_unpack ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_l_unpack ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_p_compress ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_l_compress ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_p_histo ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_l_histo ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_p_text ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_l_text ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_p_graphics ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_l_graphics ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_p_dbHdr ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_l_dbHdr ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_levelOffset ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_p_user ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_l_user ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_p_suite ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_l_suite ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_p_exam ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_l_exam ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_p_series ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_l_series ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_p_image ) );
  ByteSwapper< int >::SwapFromSystemToBigEndian ( &( hdr->GENESIS_IH_img_l_image ) );
}
}

GEImageHeader *
GE5ImageIO::ReadHeader(const char  *FileNameToRead)
{
  //#define VERBOSE_DEBUGGING
#if defined( VERBOSE_DEBUGGING )
#define RGEDEBUG(x) x
#else
#define RGEDEBUG(x)
  #endif

  Ge5xPixelHeader imageHdr; // GE 5x Header
  GEImageHeader * curImage;
  bool            pixelHdrFlag;
  std::string     reason;
  if ( this->CheckGE5xImages(FileNameToRead, reason) != 0 )
    {
    itkExceptionMacro(
      "GE5ImageIO could not open file "
      << FileNameToRead << " for reading."
      << std::endl
      << "Reason: "
      << reason
      );
    }

  curImage = new GEImageHeader;
  if ( curImage == ITK_NULLPTR )
    {
    itkExceptionMacro(
      "GE5ImageIO failed to create a GEImageHeader while reading "
      << FileNameToRead << " ." << std::endl << "Reason: "
      << "new GEImageHeader failed."
      );
    }
  memset( curImage, 0, sizeof( GEImageHeader ) );

  std::ifstream f;
  this->OpenFileForReading( f, FileNameToRead );

  f.read( (char *)&imageHdr, sizeof( imageHdr ) );
  if ( f.fail() )
    {
    itkExceptionMacro(
      "GE5ImageIO IO error while reading  "
      << FileNameToRead << " ." << std::endl
      << "Reason: " << itksys::SystemTools::GetLastSystemError()
      );
    }
  SwapPixHdr(&imageHdr);

  // NOTE: The handling of version 2 vs Version3 is modelled after
  // the sivic GE5Signa5x reader -- found here
  // http://sivic.svn.sourceforge.net
  //
  // if i didn't see it with my own eyes I wouldn't believe it.
  // Signa 5x either have a proper header or they don't!
  // Appparently Version 2 files do always have a header or
  // we'd be totally lost!
  // if they don't have a header, we have to make assumptions
  // about where they start and hope we're right; below, the offset
  // is computed once the X & Y dims are known
  if ( imageHdr.GENESIS_IH_img_magic == GE_5X_MAGIC_NUMBER )
    {
    pixelHdrFlag = true;
    curImage->offset = imageHdr.GENESIS_IH_img_hdr_length;
    }
  else
    {
    pixelHdrFlag = false;
    }
  strncpy (curImage->filename, FileNameToRead, IOCommon::ITK_MAXPATHLEN);

  //
  // if there's no GE5 header on the file we have to assume
  // it's Version 3. If there is a header, it could be version2
  // in which case we need to fill out the fields in the header
  // that are defined in version 3 and not version 2.
  if(pixelHdrFlag && imageHdr.GENESIS_IH_img_version == 2)
    {
    imageHdr.GENESIS_IH_img_p_suite = 124; // Version 3 is 2304
    imageHdr.GENESIS_IH_img_l_suite = 116;  // Version 3 is 114
    imageHdr.GENESIS_IH_img_p_exam = 240;  // Version 3 is 2418
    imageHdr.GENESIS_IH_img_l_exam = 1040; // Version 3 is 1024
    imageHdr.GENESIS_IH_img_p_series = 1280; // Version 3 is 3442
    imageHdr.GENESIS_IH_img_l_series = 1028;  // Version 3 is 1020
    imageHdr.GENESIS_IH_img_p_image = 2308;  // Version 3 is 4462
    imageHdr.GENESIS_IH_img_l_image = 1044; // Don't know for sure?
    }

  // if we have a version2 file, most of the fields are offset from
  // their version3 positions.
#define VOff(a,b) (imageHdr.GENESIS_IH_img_version != 2 ? a : b)
  // Create a buffer to read the exam header.
  // Now seek to the exam header and read the data into the buffer.
  char *buffer = ITK_NULLPTR;
  if(pixelHdrFlag)
    {
    buffer = new char[imageHdr.GENESIS_IH_img_l_exam];
    if ( buffer == ITK_NULLPTR )
      {
      f.close();
      itkExceptionMacro("GE5ImageIO:Unable to allocate memory for exam header!");
      }
    f.seekg(imageHdr.GENESIS_IH_img_p_exam,std::ios::beg);
    f.read(buffer,imageHdr.GENESIS_IH_img_l_exam);
    }
  else
    {
    buffer = new char[GENESIS_EX_HDR_LEN];
    if ( buffer == ITK_NULLPTR )
      {
      f.close();
      itkExceptionMacro("GE5ImageIO:Unable to allocate memory for exam header!");
      }
    f.seekg(GENESIS_EX_HDR_START,std::ios::beg);
    f.read(buffer,GENESIS_EX_HDR_LEN);
    }
  if(f.fail())
    {
    f.close();
    delete[] buffer;
    itkExceptionMacro("GE5ImageIO:Could not read exam header!");
    }

  // Now extract the exam information from the buffer.
  curImage->examNumber = hdr2Short(buffer+8);

  strncpy(curImage->hospital,buffer+10,34);
  curImage->hospital[34] = '\0';

  // patient id
  std::string tmpId(buffer+VOff(84,88), 13);
  std::remove(tmpId.begin(), tmpId.end(), '-');
  strncpy(curImage->patientId, tmpId.c_str(), sizeof(curImage->patientId)-1);
  curImage->patientId[sizeof(curImage->patientId)-1] = '\0';

  strncpy(curImage->name,buffer+VOff(97,101),25);
  curImage->name[24] = '\0';

  // Need to know modality as well.
  strncpy(curImage->modality,buffer+VOff(305,309),3);
  curImage->modality[3] = '\0';
  bool isCT = false;
  if( strncmp(curImage->modality, "CT", 2) == 0)
    {
    isCT = true;
    }

  // Done with exam, delete buffer.
  delete[] buffer;
  buffer = ITK_NULLPTR;

  // Allocate buffer for series header.
  // Now seek to the series header and read the data into the buffer.
  if(pixelHdrFlag)
    {
    buffer = new char[imageHdr.GENESIS_IH_img_l_series];
    if ( buffer == ITK_NULLPTR )
      {
      f.close();
      itkExceptionMacro
        ("GE5ImageIO:Unable to allocate memory for series header!");
      }
    f.seekg(imageHdr.GENESIS_IH_img_p_series, std::ios::beg);
    f.read(buffer, imageHdr.GENESIS_IH_img_l_series);
    }
  else
    {
    buffer = new char[GENESIS_SE_HDR_LEN];
    if ( buffer == ITK_NULLPTR )
      {
      f.close();
      itkExceptionMacro
        ("GE5ImageIO:Unable to allocate memory for series header!");
      }
    f.seekg(GENESIS_SE_HDR_START);
    f.read(buffer,GENESIS_SE_HDR_LEN);
    }
  if(f.fail())
    {
    f.close();
    itkExceptionMacro("GE5ImageIO:Could not read exam header!");
    }

  // Now extract the series information from the buffer.
  curImage->seriesNumber = hdr2Short(buffer+10);

  int timeStamp = hdr2Int(buffer+12);
  statTimeToAscii(&timeStamp,curImage->date,sizeof(curImage->date));

  // Done with series, delete buffer and allocate for MR header.
  delete[] buffer;
  buffer = ITK_NULLPTR;
  if(pixelHdrFlag)
    {
    buffer = new char[imageHdr.GENESIS_IH_img_l_image];
    if ( buffer == ITK_NULLPTR )
      {
      f.close();
      itkExceptionMacro("GE5ImageIO:Unable to allocate memory for MR header!");
      }
    // Now seek to the MR header and read the data into the buffer.
    f.seekg(imageHdr.GENESIS_IH_img_p_image, std::ios::beg);
    f.read(buffer,imageHdr.GENESIS_IH_img_l_image);
    }
  else
    {
    buffer = new char[GENESIS_MR_HDR_LEN];
    if ( buffer == ITK_NULLPTR )
      {
      f.close();
      itkExceptionMacro("GE5ImageIO:Unable to allocate memory for MR header!");
      }
    f.seekg(GENESIS_IM_HDR_START, std::ios::beg);
    f.read(buffer,GENESIS_MR_HDR_LEN);
    }
  if(f.fail())
    {
    itkExceptionMacro("GE5ImageIOCould not read exam header!");
    }
  // Won't need anymore info from the file after this, so close file.
  f.close();

  // Now extract the MR information from the buffer.
  // This is the largest header!
  curImage->imageNumber = hdr2Short(buffer+12);

  curImage->sliceThickness = hdr2Float(buffer+VOff(26,28));

  curImage->imageXsize = hdr2Short(buffer+VOff(30,32));
  curImage->imageYsize = hdr2Short(buffer+VOff(32,34));
  //
  // if this a headerless flag, we don't know until now
  // where to begin reading image data
  if(!pixelHdrFlag)
    {
    curImage->offset =
      itksys::SystemTools::FileLength(FileNameToRead)
      - ( curImage->imageXsize * curImage->imageYsize * 2 );
    }

  curImage->xFOV = hdr2Float(buffer+VOff(34,36));
  curImage->yFOV = hdr2Float(buffer+VOff(38,40));

  curImage->acqXsize = hdr2Short(buffer+VOff(42,44));
  curImage->acqYsize = hdr2Short(buffer+VOff(46,48));

  curImage->imageXres = hdr2Float(buffer+VOff(50,52));
  curImage->imageYres = hdr2Float(buffer+VOff(54,56));

  short int GE_Plane(hdr2Short(buffer+VOff(114,116)));
  switch ( GE_Plane )
    {
    case GE_CORONAL:
      curImage->coordinateOrientation =
        itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP;
      break;
    case GE_SAGITTAL:
      curImage->coordinateOrientation =
        itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR;
      break;
    case GE_AXIAL:
      curImage->coordinateOrientation =
        itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI;
      break;
    default:
      curImage->coordinateOrientation =
        itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP;
      break;
    }


  curImage->sliceLocation = hdr2Float(buffer+VOff(126,132));

  curImage->centerR = hdr2Float(buffer+VOff(130,136));
  curImage->centerA = hdr2Float(buffer+VOff(134,140));
  curImage->centerS = hdr2Float(buffer+VOff(138,144));
  curImage->normR = hdr2Float(buffer+VOff(142,146));
  curImage->normA = hdr2Float(buffer+VOff(146,152));
  curImage->normS = hdr2Float(buffer+VOff(150,156));
  curImage->tlhcR = hdr2Float(buffer+VOff(154,160));
  curImage->tlhcA = hdr2Float(buffer+VOff(158,164));
  curImage->tlhcS = hdr2Float(buffer+VOff(162,168));
  curImage->trhcR = hdr2Float(buffer+VOff(166,172));
  curImage->trhcA = hdr2Float(buffer+VOff(170,176));
  curImage->trhcS = hdr2Float(buffer+VOff(174,180));
  curImage->brhcR = hdr2Float(buffer+VOff(178,184));
  curImage->brhcA = hdr2Float(buffer+VOff(182,188));
  curImage->brhcS = hdr2Float(buffer+VOff(186,192));

  // These values are all MR specific
  if(!isCT)
    {
    curImage->TR = hdr2Int(buffer+VOff(194,200));
    curImage->TI = hdr2Int(buffer+VOff(198,204));
    curImage->TE = hdr2Int(buffer+VOff(202,208));
    curImage->TE2 = hdr2Int(buffer+VOff(206,212));

    if((curImage->numberOfEchoes = hdr2Short(buffer+VOff(210,216))) == 0)
      {
      curImage->numberOfEchoes = 1;
      }

    curImage->echoNumber = hdr2Short(buffer+VOff(212,218));

    curImage->NEX = hdr2Int(buffer+VOff(218,224));

    curImage->flipAngle = hdr2Short(buffer+VOff(254,260));

    strncpy(curImage->pulseSequence,
      buffer+VOff(308,320),
      34);
    curImage->pulseSequence[33] = '\0';

    curImage->numberOfSlices = hdr2Short(buffer+VOff(398,416));
    }
  else
    {
    curImage->TR = 0;
    curImage->TI = 0;
    curImage->TE = 0;
    curImage->TE2 = 0;
    curImage->numberOfEchoes = 1;
    curImage->echoNumber = 1;
    curImage->NEX = 1;
    curImage->flipAngle = 0;
    curImage->pulseSequence[0] = '\0';
    curImage->numberOfSlices = 1;
    }

  // Delete the buffer and return the pointer to the header.
  // The function that receives the pointer must do memory
  // cleanup or a memory leak will occur.
  delete[] buffer;

  return ( curImage );
}

void
GE5ImageIO::ModifyImageInformation()
{
  vnl_vector< double > dirx(3), diry(3), dirz(3);

  // NOTE: itk use LPS coordinates while the GE system uses RAS
  // coordinates. Consequently, the R and A coordinates must be negated
  // to convert them to L and P.

  dirx[0] = -( m_ImageHeader->trhcR - m_ImageHeader->tlhcR );
  dirx[1] = -( m_ImageHeader->trhcA - m_ImageHeader->tlhcA );
  dirx[2] =  ( m_ImageHeader->trhcS - m_ImageHeader->tlhcS );
  dirx.normalize();

  diry[0] = -( m_ImageHeader->brhcR - m_ImageHeader->trhcR );
  diry[1] = -( m_ImageHeader->brhcA - m_ImageHeader->trhcA );
  diry[2] =  ( m_ImageHeader->brhcS - m_ImageHeader->trhcS );
  diry.normalize();

  dirz[0] = -m_ImageHeader->normR;
  dirz[1] = -m_ImageHeader->normA;
  dirz[2] =  m_ImageHeader->normS;
  dirz.normalize();

  // Set the directions
  this->SetDirection(0, dirx);
  this->SetDirection(1, diry);
  this->SetDirection(2, dirz);

  // See if slices need to be reversed. itk uses a right hand
  // coordinate system. If the computed slice direction is opposite
  // the direction in the header, the files have to be read in reverse
  // order.
  vnl_vector< double > sliceDirection = vnl_cross_3d(dirx, diry);
  if ( dot_product(sliceDirection, dirz) < 0 )
    {
    // Use the computed direction
    this->SetDirection(2, sliceDirection);

    // Sort image list in reverse order
    m_FilenameList->SetSortOrder(IPLFileNameList::SortGlobalDescend);
    m_FilenameList->sortImageList();
    }

  // Compute the spacing between two slices  from the origins of the
  // first two files in the study
  if ( m_FilenameList->NumFiles() > 1 )
    {
    IPLFileNameList::IteratorType it = m_FilenameList->begin();

    // The first file
    std::string file1 = ( *it )->GetImageFileName();

    // The second file
    it++;
    std::string file2 = ( *it )->GetImageFileName();

    GEImageHeader *hdr1 = this->ReadHeader( file1.c_str() );
    GEImageHeader *hdr2 = this->ReadHeader( file2.c_str() );

    float origin1[3], origin2[3];
    origin1[0] = hdr1->tlhcR;
    origin1[1] = hdr1->tlhcA;
    origin1[2] = hdr1->tlhcS;

    // Origin shopuld always come from the first slice
    this->SetOrigin(0, -hdr1->tlhcR);
    this->SetOrigin(1, -hdr1->tlhcA);
    this->SetOrigin(2,  hdr1->tlhcS);

    origin2[0] = hdr2->tlhcR;
    origin2[1] = hdr2->tlhcA;
    origin2[2] = hdr2->tlhcS;

    float distanceBetweenTwoSlices = std::sqrt(
      ( origin1[0] - origin2[0] ) * ( origin1[0] - origin2[0] )
      + ( origin1[1] - origin2[1] ) * ( origin1[1] - origin2[1] )
      + ( origin1[2] - origin2[2] ) * ( origin1[2] - origin2[2] ) );

    this->SetSpacing(2, distanceBetweenTwoSlices);

    // Cleanup
    delete hdr1;
    delete hdr2;
    }
  else
    // If there is only one slice, the use it's origin
    {
    this->SetOrigin(0, -m_ImageHeader->tlhcR);
    this->SetOrigin(1, -m_ImageHeader->tlhcA);
    this->SetOrigin(2,  m_ImageHeader->tlhcS);
    }
}
} // end namespace itk
