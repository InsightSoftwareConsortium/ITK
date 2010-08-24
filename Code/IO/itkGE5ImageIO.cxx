/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGE5ImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkGE5ImageIO.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkDirectory.h"
#include <itksys/SystemTools.hxx>
#include <iostream>
#include <fstream>
#include <string.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <vector>
#include <string>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_cross.h>

#include "itkGEImageHeader.h"
#include "itkIOCommon.h"

//From uiig library "The University of Iowa Imaging Group-UIIG"

namespace itk
{
static const char GE_PROD_STR[] = "SIGNA";
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

  std::ifstream f(imageFileTemplate, std::ios::binary | std::ios::in);
  if ( !f.is_open() )
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

void
GE5ImageIO::SwapPixHdr(Ge5xPixelHeader *hdr)
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

  return;
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

  Ge5xPixelHeader imageHdr;                                       /* Header
                                                                    Structure
                                                                    for GE 5x
                                                                    images */
  char            hdr[GENESIS_IM_HDR_START + GENESIS_MR_HDR_LEN]; /* Header to
                                                                    hold GE
                                                                    header */
  GEImageHeader * curImage;
  bool            pixelHdrFlag;
  int             timeStamp;
  char            tmpId[64];
  char *          ptr;
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
  if ( curImage == NULL )
    {
    itkExceptionMacro(
      "GE5ImageIO failed to create a GEImageHeader while reading "
      << FileNameToRead << " ."
      << std::endl
      << "Reason: "
      << "new GEImageHeader failed."
      );
    }
  memset( curImage, 0, sizeof( GEImageHeader ) );
  pixelHdrFlag = false;

  std::ifstream f(FileNameToRead, std::ios::binary | std::ios::in);
  if ( !f.is_open() )
    {
    itkExceptionMacro(
      "GE5ImageIO failed to open "
      << FileNameToRead << " for input."
      << std::endl
      << "Reason: "
      << itksys::SystemTools::GetLastSystemError()
      );
    }
  f.read( (char *)&imageHdr, sizeof( imageHdr ) );
  if ( f.fail() )
    {
    if ( f.is_open() )
      {
      f.close();
      }
    itkExceptionMacro(
      "GE5ImageIO IO error while reading  "
      << FileNameToRead << " ."
      << std::endl
      << "Reason: "
      << itksys::SystemTools::GetLastSystemError()
      );
    }
  this->SwapPixHdr(&imageHdr);

  if ( imageHdr.GENESIS_IH_img_magic == GE_5X_MAGIC_NUMBER )
    {
    f.seekg(imageHdr.GENESIS_IH_img_p_suite, std::ios::beg);
    if ( f.fail() )
      {
      if ( f.is_open() )
        {
        f.close();
        }
      itkExceptionMacro(
        "GE5ImageIO IO error while seeking  "
        << FileNameToRead << " ."
        << std::endl
        << "Reason: "
        << itksys::SystemTools::GetLastSystemError()
        );
      }
    pixelHdrFlag = true;
    }
  else
    {
    f.seekg(0, std::ios::beg);
    }
  f.read(hdr, GENESIS_IM_HDR_START + GENESIS_MR_HDR_LEN);
  if ( f.fail() )
    {
    if ( f.is_open() )
      {
      f.close();
      }
    itkExceptionMacro(
      "GE5ImageIO IO error while reading  "
      << FileNameToRead << " ."
      << std::endl
      << "Reason: "
      << itksys::SystemTools::GetLastSystemError()
      );
    }

  /* Set Patient-Name */
  strncpy (curImage->name, &hdr[GENESIS_EX_HDR_START + GENESIS_EX_PATNAME],
           GENESIS_EX_PATAGE - GENESIS_EX_PATNAME + 1);
  sprintf (curImage->scanner, "GE-5X");

  /* Set Hospital-Name */
  strncpy (curImage->hospital, &hdr[GENESIS_EX_HDR_START + GENESIS_EX_HOSPNAME],
           GENESIS_EX_DETECT - GENESIS_EX_HOSPNAME + 1);

  // Get the coordinate information from the header. This will be used
  // later on to compute the origin, spacing and directions.
  curImage->centerR = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_CTR_R]);
  curImage->centerA = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_CTR_A]);
  curImage->centerS = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_CTR_S]);
  curImage->normR = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_NORM_R]);
  curImage->normA = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_NORM_A]);
  curImage->normS = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_NORM_S]);
  curImage->tlhcR = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_TLHC_R]);
  curImage->tlhcA = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_TLHC_A]);
  curImage->tlhcS = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_TLHC_S]);
  curImage->trhcR = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_TRHC_R]);
  curImage->trhcA = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_TRHC_A]);
  curImage->trhcS = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_TRHC_S]);
  curImage->brhcR = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_BRHC_R]);
  curImage->brhcA = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_BRHC_A]);
  curImage->brhcS = hdr2Float(&hdr[GENESIS_IM_HDR_START + GENESIS_CT_BRHC_S]);

  /* Set Images-Per-Slice */
  curImage->imagesPerSlice = hdr2Short(&hdr[GENESIS_IM_HDR_START + GENESIS_MR_CPHASE]);

  /* Set Date */
  timeStamp = hdr2Int (&hdr[GENESIS_SE_HDR_START + GENESIS_SE_ACTUAL_DT]);
  statTimeToAscii (&timeStamp, curImage->date);

  /* Set Patient-Id */
  strncpy (tmpId, &hdr[GENESIS_EX_HDR_START + GENESIS_EX_PATID],
           GENESIS_EX_PATNAME - GENESIS_EX_PATID + 1);
  tmpId[GENESIS_EX_PATNAME - GENESIS_EX_PATID + 1] = '\0';
  curImage->patientId[0] = '\0';

  ptr = strtok (tmpId, "-");

  while ( ptr != NULL )
    {
    strcat (curImage->patientId, ptr);
    ptr = strtok (NULL, "-");
    }

  RGEDEBUG(fprintf (stderr, "Id <%s>\n", curImage->patientId); )

  curImage->seriesNumber =
    hdr2Short (&hdr[GENESIS_SE_HDR_START + GENESIS_SE_NO]);
  RGEDEBUG(fprintf (stderr, "Series Number %d\n", curImage->seriesNumber); )

  curImage->imageNumber =
    hdr2Short (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_IM_NO]);
  RGEDEBUG(fprintf (stderr, "Image Number %d\n", curImage->imageNumber); )

  curImage->sliceThickness =
    hdr2Float (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_SLTHICK]);
  RGEDEBUG(fprintf (stderr, "Thickness %f\n", curImage->sliceThickness); )

  curImage->imageXsize =
    hdr2Short (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_IMATRIX_X]);
  curImage->imageYsize = hdr2Short (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_IMATRIX_Y]);
  RGEDEBUG(fprintf (stderr, "Acq Size %dx%d\n", curImage->acqXsize, curImage->acqYsize); )

  curImage->xFOV =
    hdr2Float (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_DFOV]);
  curImage->yFOV =
    hdr2Float (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_DFOV]);
  if ( curImage->yFOV == 0.0 )
    {
    curImage->yFOV = curImage->xFOV;
    }
  RGEDEBUG(fprintf (stderr, "FOV %fx%f\n", curImage->xFOV, curImage->yFOV); )

  curImage->acqXsize =
    (int)hdr2Float (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_DIM_X]);
  curImage->acqYsize =
    (int)hdr2Float (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_DIM_Y]);
  RGEDEBUG(fprintf (stderr, "Image Size %dx%d\n", curImage->imageXsize, curImage->imageYsize); )

  curImage->imageXres =
    hdr2Float (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_PIXSIZE_X]);
  curImage->imageYres =
    hdr2Float (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_PIXSIZE_Y]);
  RGEDEBUG(fprintf (stderr, "Image Res %fx%f\n", curImage->imageXres, curImage->imageYres); )
  short int GE_Plane =
    hdr2Short (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_PLANE]);

  //RECODE image plane to be brains2 compliant.!!
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
  curImage->sliceLocation =
    hdr2Float (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_LOC]);

  RGEDEBUG(fprintf (stderr, "Location %f %c %c\n", curImage->sliceLocation,
                    hdr[GENESIS_IM_HDR_START + GENESIS_MR_LOC_RAS], hdr[GENESIS_IM_HDR_START + GENESIS_MR_LOC_RAS + 1]); )

  curImage->TR = hdr2Int (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_TR]) / 1000.0f;
  curImage->TI = hdr2Int (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_TI]) / 1000.0f;
  curImage->TE = hdr2Int (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_TE]) / 1000.0f;
  curImage->TE2 = hdr2Int (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_TE2]) / 1000.0f;
  RGEDEBUG(fprintf (stderr, "TR %f, TI %f, TE %f, TE2 %f\n", curImage->TR, curImage->TI, curImage->TE, curImage->TE2); )

  curImage->numberOfEchoes =
    hdr2Short (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_NUMECHO]);
  curImage->echoNumber =
    hdr2Short (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_ECHONUM]);

  if ( curImage->numberOfEchoes == 0 )
    {
    curImage->numberOfEchoes = 1;
    }
  RGEDEBUG(fprintf (stderr, "Echos %d,  Number %d\n", curImage->numberOfEchoes, curImage->echoNumber); )

  curImage->NEX = (int)hdr2Float (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_NEX]);
  RGEDEBUG(fprintf (stderr, "NEX %d\n", curImage->NEX); )

  curImage->flipAngle =
    hdr2Short (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_FLIP]);
  RGEDEBUG(fprintf (stderr, "Flip Angle %d\n", curImage->flipAngle); )

  strncpy (curImage->pulseSequence,
           &hdr[GENESIS_IM_HDR_START + GENESIS_MR_PSDNAME],
           31);
  curImage->pulseSequence[31] = '\0';
  RGEDEBUG(fprintf (stderr, "Sequence %s\n", curImage->pulseSequence); )

  curImage->numberOfSlices =
    hdr2Short (&hdr[GENESIS_IM_HDR_START + GENESIS_MR_SLQUANT]);
  RGEDEBUG(fprintf (stderr, "Number Of Slices %d\n", curImage->numberOfSlices); )

  if ( pixelHdrFlag )
    {
    curImage->offset = imageHdr.GENESIS_IH_img_hdr_length;
    }
  else
    {
    curImage->offset =
      itksys::SystemTools::FileLength(FileNameToRead)
      - ( curImage->imageXsize * curImage->imageYsize * 2 );
    }

  strncpy (curImage->filename, FileNameToRead, IOCommon::ITK_MAXPATHLEN + 1);

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

    float distanceBetweenTwoSlices = vcl_sqrt(
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
