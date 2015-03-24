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
#include "itkGE4ImageIO.h"
#include "itkByteSwapper.h"
#include "Ge4xHdr.h"
#include "itksys/SystemTools.hxx"
#include <iostream>
#include <fstream>
//From uiig library "The University of Iowa Imaging Group-UIIG"

namespace itk
{
// Default constructor
GE4ImageIO::GE4ImageIO()
{}

GE4ImageIO::~GE4ImageIO()
{
  //Purposefully left blank
}

bool GE4ImageIO::CanReadFile(const char *FileNameToRead)
{
  char tmpStr[64];

  std::ifstream f;
  try
    {
    this->OpenFileForReading( f, FileNameToRead );
    }
  catch( ExceptionObject & )
    {
    return false;
    }

  // This is a weak heuristic but should only be true for GE4 files
  //
  // Get the Plane from the IMAGE Header.
  if ( this->GetStringAt(f, SIGNA_SEHDR_START * 2 + SIGNA_SEHDR_PLANENAME * 2, tmpStr, 16, false) == -1 )
    {
    f.close();
    return false;
    }
  tmpStr[16] = '\0';
  // if none of these strings show up, most likely not GE4
  if ( strstr (tmpStr, "CORONAL") == ITK_NULLPTR
       && strstr (tmpStr, "SAGITTAL") == ITK_NULLPTR
       && strstr (tmpStr, "AXIAL") == ITK_NULLPTR
       && strstr (tmpStr, "OBLIQUE") == ITK_NULLPTR )
    {
    f.close();
    return false;
    }
  //
  // doesn't appear to be any signature in the header so I guess
  // I have to assume it's readable
  f.close();
  return true;
}

GEImageHeader * GE4ImageIO::ReadHeader(const char *FileNameToRead)
{
  // #define VERBOSE_DEBUGGING
#if defined( VERBOSE_DEBUGGING )
#define RGEDEBUG(x) x
#else
#define RGEDEBUG(x)
#endif
  if ( FileNameToRead == ITK_NULLPTR || strlen(FileNameToRead) == 0 )
    {
    return ITK_NULLPTR;
    }
  //
  // need to check if this is a valid file before going further
  if ( !this->CanReadFile(FileNameToRead) )
    {
    RAISE_EXCEPTION();
    }
  GEImageHeader *hdr = new GEImageHeader;
  if ( hdr == ITK_NULLPTR )
    {
    RAISE_EXCEPTION();
    }
  // Set modality to UNKNOWN
  strcpy(hdr->modality, "UNK");

  //  RGEDEBUG(char debugbuf[16384];)
  char      tmpStr[IOCommon::ITK_MAXPATHLEN + 1];
  int       intTmp;
  short int tmpShort;
  float     tmpFloat;

  //
  // save off the name of the current file...
  strncpy(hdr->filename, FileNameToRead, sizeof(hdr->filename)-1);
  hdr->filename[sizeof(hdr->filename)-1] = '\0';

  //
  // Next, can you open it?

  std::ifstream f;
  this->OpenFileForReading( f, FileNameToRead );

  this->GetStringAt(f, SIGNA_STHDR_START * 2 + SIGNA_STHDR_DATE_ASCII * 2, tmpStr, 10);
  tmpStr[10] = '\0';
  RGEDEBUG(std::sprintf (debugbuf, "Date = %s\n", tmpStr); cerr << debugbuf; )
  strncpy(hdr->date, tmpStr, sizeof(hdr->date)-1);
  hdr->date[sizeof(hdr->date)-1] = '\0';

  // Get Patient-Name from the STUDY Header
  this->GetStringAt(f, SIGNA_STHDR_START * 2 + SIGNA_STHDR_PATIENT_NAME * 2, tmpStr, 32);
  tmpStr[32] = '\0';
  strncpy(hdr->hospital, tmpStr, sizeof(hdr->hospital)-1);
  hdr->hospital[sizeof(hdr->hospital)-1] = '\0';

  /* Get Patient-Number from the STUDY Header */
  this->GetStringAt(f, SIGNA_STHDR_START * 2 + SIGNA_STHDR_PATIENT_ID * 2, tmpStr, 12);
  tmpStr[12] = '\0';
  RGEDEBUG(std::sprintf (debugbuf, "Patient-Number = %s\n", tmpStr); cerr << debugbuf; )
  strncpy(hdr->patientId, tmpStr, sizeof(hdr->patientId)-1);
  hdr->patientId[sizeof(hdr->patientId)-1] = '\0';

  /* Get the Exam-Number from the STUDY Header */
  this->GetStringAt(f, SIGNA_STHDR_START * 2 + SIGNA_STHDR_STUDY_NUM * 2, tmpStr, 6);
  tmpStr[6] = '\0';
  RGEDEBUG(std::sprintf (debugbuf, "Exam-Number = %s\n", tmpStr); cerr << debugbuf; )
  strncpy(hdr->scanId, tmpStr, sizeof(hdr->scanId)-1);
  hdr->scanId[sizeof(hdr->scanId)-1] = '\0';

  /* Get the FOV from the SERIES Header */
  f.seekg (SIGNA_SEHDR_START * 2 + SIGNA_SEHDR_FOV * 2, std::ios::beg);
  IOCHECK();
  f.read ( (char *)&intTmp, sizeof( intTmp ) );
  IOCHECK();
  tmpFloat = MvtSunf (intTmp);

  hdr->xFOV = tmpFloat;
  hdr->yFOV = hdr->xFOV;
  RGEDEBUG(std::sprintf (debugbuf, "FOV = %fx%f\n", hdr->xFOV, hdr->yFOV); cerr << debugbuf; )

  /* Get the Plane from the IMAGE Header */
  this->GetStringAt(f, SIGNA_SEHDR_START * 2 + SIGNA_SEHDR_PLANENAME * 2, tmpStr, 16);
  tmpStr[16] = '\0';

  if ( strstr (tmpStr, "CORONAL") != ITK_NULLPTR )
    {
    //hdr->imagePlane =
    // itk::SpatialOrientation::ITK_ANALYZE_ORIENTATION_IRP_CORONAL;
    //hdr->origin = itk::SpatialOrientation::ITK_ORIGIN_SRP; // was SLA in the
    // brains2 filter.
    hdr->coordinateOrientation = itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP;
    }
  else if ( strstr (tmpStr, "SAGITTAL") != ITK_NULLPTR )
    {
    //hdr->imagePlane =
    // itk::SpatialOrientation::ITK_ANALYZE_ORIENTATION_IRP_SAGITTAL;
    //hdr->origin = itk::SpatialOrientation::ITK_ORIGIN_SRA;  //was SLP in the
    // brains2 filter.
    hdr->coordinateOrientation = itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR;
    }
  else if ( strstr (tmpStr, "AXIAL") != ITK_NULLPTR )
    {
    //hdr->imagePlane =
    // itk::SpatialOrientation::ITK_ANALYZE_ORIENTATION_IRP_TRANSVERSE;
    //hdr->origin = itk::SpatialOrientation::ITK_ORIGIN_SRA;  //was SLP in the
    // brains2 filter.
    hdr->coordinateOrientation = itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI;
    }
  else
    {
    //hdr->imagePlane =
    // itk::SpatialOrientation::ITK_ANALYZE_ORIENTATION_IRP_CORONAL;
    //hdr->origin = itk::SpatialOrientation::ITK_ORIGIN_SRP; // was SLA
    hdr->coordinateOrientation = itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP;
    }
  //RGEDEBUG(std::sprintf (debugbuf, "Plane = %d\n", hdr->imagePlane); cerr <<
  // debugbuf;)

  /* Get the Scan Matrix from the IMAGE Header */
  this->GetShortAt( f, SIGNA_SEHDR_START * 2 + SIGNA_SEHDR_SCANMATRIXX * 2, &( hdr->acqXsize ) );
  this->GetShortAt( f, ( SIGNA_SEHDR_START * 2 + SIGNA_SEHDR_SCANMATRIXY * 2 ) + sizeof( short ),
                    &( hdr->acqYsize ) );

  RGEDEBUG(std::sprintf (debugbuf, "Scan Matrix = %dx%d\n", hdr->acqXsize, hdr->acqYsize); cerr << debugbuf; )

  /* Get Series-Number from SERIES Header */
  this->GetStringAt(f, SIGNA_SEHDR_START * 2 + SIGNA_SEHDR_SERIES_NUM * 2, tmpStr, 3);
  tmpStr[3] = '\0';
  hdr->seriesNumber = atoi (tmpStr);
  RGEDEBUG(std::sprintf (debugbuf, "Series Number = %d\n", hdr->seriesNumber); cerr << debugbuf; )

  /* Get Image-Number from IMAGE Header */
  this->GetStringAt(f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_IMAGE_NUM * 2, tmpStr, 3);
  tmpStr[3] = '\0';
  hdr->imageNumber = atoi (tmpStr);
  RGEDEBUG(std::sprintf (debugbuf, "Image Number = %d\n", hdr->imageNumber); cerr << debugbuf; )

  /* Get Images-Per-Slice from IMAGE Header */
  this->GetStringAt(f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_PHASENUM * 2, tmpStr, 3);
  tmpStr[3] = '\0';
  hdr->imagesPerSlice = atoi (tmpStr);
  RGEDEBUG(std::sprintf (debugbuf, "Images Per Slice = %d\n", hdr->imagesPerSlice); cerr << debugbuf; )

  /* Get the Slice Location from the IMAGE Header */
  // hack alert -- and this goes back to a hack in the original code
  // you read in an integer, but you DON'T byte swap it, and then pass
  // it into the MvtSunf function to get the floating point value.
  // to circumvent byte swapping in GetIntAt, use GetStringAt
  this->GetStringAt( f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_SLICELOC * 2,
                     (char *)&intTmp, sizeof( int ) );

  hdr->sliceLocation = MvtSunf (intTmp);

  RGEDEBUG(std::sprintf (debugbuf, "Location = %f\n", hdr->sliceLocation); cerr << debugbuf; )

  this->GetStringAt( f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_SLICE_THICK * 2,
                     (char *)&intTmp, sizeof( intTmp ) );

  hdr->sliceThickness = MvtSunf (intTmp);

  RGEDEBUG(std::sprintf (debugbuf, "Thickness = %f\n", hdr->sliceThickness); cerr << debugbuf; )

  /* Get the Slice Spacing from the IMAGE Header */
  this->GetStringAt( f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_SLICE_SPACING * 2,
                     (char *)&intTmp, sizeof( int ) );

  hdr->sliceGap = MvtSunf (intTmp);

  RGEDEBUG(std::sprintf (debugbuf, "Slice Gap = %f\n", hdr->sliceGap); cerr << debugbuf; )

  /* Get TR from the IMAGE Header */
  this->GetStringAt( f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_TR * 2,
                     (char *)&intTmp, sizeof( int ) );

  hdr->TR = MvtSunf (intTmp);

  RGEDEBUG(std::sprintf (debugbuf, "TR = %f\n", hdr->TR); cerr << debugbuf; )

  /* Get TE from the IMAGE Header */
  this->GetStringAt( f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_TE * 2,
                     (char *)&intTmp, sizeof( int ) );

  hdr->TE = MvtSunf (intTmp);
  //  RGEDEBUG(std::sprintf (debugbuf, "TE = %f\n", hdr->TE); cerr << debugbuf;)

  /* Get TI from the IMAGE Header */
  this->GetStringAt( f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_TI * 2,
                     (char *)&intTmp, sizeof( int ) );

  hdr->TI = MvtSunf (intTmp);
  RGEDEBUG(std::sprintf (debugbuf, "TI = %f\n", hdr->TI); cerr << debugbuf; )

  /* Get Number of Echos from the IMAGE Header */
  this->GetShortAt( f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_NUMECHOS * 2, &( hdr->numberOfEchoes ) );
  RGEDEBUG(std::sprintf (debugbuf, "Number of Echos = %d\n", hdr->numberOfEchoes); cerr << debugbuf; )

  /* Get Echo Number from the IMAGE Header */
  this->GetShortAt( f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_ECHONUM * 2, &( hdr->echoNumber ) );
  RGEDEBUG(std::sprintf (debugbuf, "Echo Number = %d\n", hdr->echoNumber); cerr << debugbuf; )

  /* Get PSD-Name from the IMAGE Header */
  this->GetStringAt(f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_PSD_NAME * 2, tmpStr, 12);
  tmpStr[12] = '\0';
  RGEDEBUG(std::sprintf (debugbuf, "PSD Name = %s\n", tmpStr); cerr << debugbuf; )

  /* Get X Pixel Dimension from the IMAGE Header */
  this->GetShortAt( f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_X_DIM * 2, &( hdr->imageXsize ) );
  RGEDEBUG(std::sprintf (debugbuf, "X Pixel Dimension = %d\n", hdr->imageXsize); cerr << debugbuf; )

  /* Get Y Pixel Dimension from the IMAGE Header */
  this->GetShortAt( f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_Y_DIM * 2, &( hdr->imageYsize ) );
  RGEDEBUG(std::sprintf (debugbuf, "Y Pixel Dimension = %d\n", hdr->imageYsize); cerr << debugbuf; )

  /* Get Pixel Size from the IMAGE Header */
  this->GetStringAt( f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_PIXELSIZE * 2,
                     (char *)&intTmp, sizeof( int ) );

  hdr->imageXres = MvtSunf (intTmp);
  hdr->imageYres = hdr->imageXres;
  RGEDEBUG(std::sprintf (debugbuf, "Pixel Size = %fx%f\n", hdr->imageXres, hdr->imageYres); cerr << debugbuf; )

  /* Get NEX from the IMAGE Header */
  this->GetStringAt( f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_NEX * 2,
                     (char *)&intTmp, sizeof( int ) );

  hdr->NEX = (short)MvtSunf (intTmp);
  RGEDEBUG(std::sprintf (debugbuf, "NEX = %d\n", hdr->NEX); cerr << debugbuf; )

  /* Get Flip Angle from the IMAGE Header */
  this->GetShortAt(f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_FLIP * 2, &tmpShort);

  if ( tmpShort > 0 )
    {
    hdr->flipAngle = (int)tmpShort;
    }
  else
    {
    hdr->flipAngle = 90;
    }
  RGEDEBUG(std::sprintf (debugbuf, "Flip Angle = %d\n", hdr->flipAngle); cerr << debugbuf; )

  //DEBUG: HACK -- what should pulse sequence be?  Is it valid for 4x filters
  // Just setting it to dummy value -- Hans
  //copy from ge5x strncpy (hdr->pulseSequence, &hdr[IM_HDR_START + IM_PSDNAME],
  // 31);
  strncpy (hdr->pulseSequence, "UNKNOWN_GE4x_PULSE_SEQUENCE", 31);
  hdr->pulseSequence[31] = '\0';

  /* Get the Number of Images from the IMAGE Header */
  this->GetShortAt( f, SIGNA_IHDR_START * 2 + SIGNA_IMHDR_NUMSLICES * 2, &( hdr->numberOfSlices ) );
  //  RGEDEBUG(std::sprintf (debugbuf, "Number of SLices = %d\n",
  // hdr->numberOfSlices); cerr << debugbuf;)

  //    status = stat (imageFile, &statBuf);
  //    if (status == -1)
  //      {
  //  return (ITK_NULLPTR);
  //      }
  //
  //    hdr->offset = statBuf.st_size - (hdr->imageXsize * hdr->imageYsize * 2);
  //
  // find file length in line ...
  SizeValueType file_length = itksys::SystemTools::FileLength(FileNameToRead);

  hdr->offset = file_length
                - ( hdr->imageXsize * hdr->imageYsize * 2 );
  return hdr;
}

float GE4ImageIO
::MvtSunf(int numb)
{
#define signbit 020000000000U
#define dmantissa 077777777U
#define dexponent 0177U
#define smantissa 037777777U
#define smantlen 23U
  ByteSwapper< int >::SwapFromSystemToBigEndian(&numb);
  unsigned int dg_exp = ( numb >> 24 ) & dexponent;
  unsigned int dg_sign = numb & signbit;
  unsigned int dg_mantissa = ( numb & dmantissa ) << 8;
  int sun_exp = 4 * ( dg_exp - 64 );
  while ( ( dg_mantissa & signbit ) == 0 && dg_mantissa != 0 )
    {
    sun_exp--;
    dg_mantissa = dg_mantissa << 1;
    }
  sun_exp += 126;
  if ( sun_exp < 0 )
    {
    sun_exp = 0;
    }
  else if ( sun_exp > 255 )
    {
    sun_exp = 255;
    }
  dg_mantissa = dg_mantissa << 1;
  int sun_num = dg_sign | ( sun_exp << smantlen ) | ( ( dg_mantissa >> 9 ) & smantissa );
  float x;
  memcpy ( &x, &sun_num, sizeof( x ) );
  return x;
}
} // end namespace itk
