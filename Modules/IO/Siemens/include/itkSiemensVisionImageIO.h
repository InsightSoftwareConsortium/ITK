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
/**
 * \file   itkSiemensVisionImageIO.h
 *
 *         Much of the code for this file reader/writer was taken from
 *         the University of Iowa Imaging Group library with the
 *         permission of the authors, Milan Sonka, Joseph Reinhardt,
 *         Ryan Long, Hans Johnson, Gary Christensen, and others.
 *         The specification for this file format is taken from the
 *         web site http://analyzedirect.com/support/10.0Documents/Analyze_Resource_01.pdf
 * \author Kent Williams
 *         The University of Iowa 2003
 * \brief This file was written as a modification to the itkMetaImageIO
 *        as a new method for reading in files from the GE4 scanner.
 */

#ifndef itkSiemensVisionImageIO_h
#define itkSiemensVisionImageIO_h
#include "ITKIOSiemensExport.h"


#include "itkIPLCommonImageIO.h"

namespace itk
{
/** \class SiemensVisionImageIO
 *
 * \author Hans J. Johnson
 * \brief Class that defines how to read SiemensVision file format.
 *
 * \ingroup IOFilters
 * \ingroup ITKIOSiemens
 */
class ITKIOSiemens_EXPORT SiemensVisionImageIO:public IPLCommonImageIO
{
public:
  /** Standard class typedefs. */
  typedef SiemensVisionImageIO Self;
  typedef IPLCommonImageIO     Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SiemensVisionImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
   * \author Hans J Johnson
   * \param FileNameToRead The name of the file to test for reading.
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can read the file specified.
   */
  virtual bool CanReadFile(const char *FileNameToRead) ITK_OVERRIDE;

  /* * Set the spacing and dimension information for the set filename. */
  // Implemented in superclass
  //      virtual void ReadImageInformation();

  /* * Get the type of the pixel.  */
  // Implemented in superclass
  //      virtual const std::type_info& GetPixelType() const;

  /* * Reads the data from disk into the memory buffer provided. */
  // Implemented in superclass
  //      virtual void Read(void* buffer);

  /* * Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a
   * component size of 1 byte. */
  // Implemented in superclass
  //      virtual unsigned int GetComponentSize() const;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /* * Determine if the file can be written with this ImageIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \author Hans J. Johnson
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can write the file specified.
   */
  // Implemented in superclass
  //      virtual bool CanWriteFile(const char * FileNameToWrite);

  /* * Set the spacing and dimension information for the set filename. */
  // Implemented in superclass
  //      virtual void WriteImageInformation();

  /* * Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  // Implemented in superclass
  //      virtual void Write(const void* buffer);

protected:
  SiemensVisionImageIO();
  ~SiemensVisionImageIO() ITK_OVERRIDE;
  // Implemented in superclass
  //      void PrintSelf(std::ostream& os, Indent indent) const;
  virtual GEImageHeader * ReadHeader(const char *FileNameToRead) ITK_OVERRIDE;

private:
  typedef enum {
    HDR_STUDY_YEAR = 0,      // Study date year, u_int
    HDR_STUDY_YEAR_LEN = 4,
    HDR_STUDY_MONTH = 4,     // Study date month, u_int
    HDR_STUDY_MONTH_LEN = 4,
    HDR_STUDY_DAY = 8,       // Study date day, u_int
    HDR_STUDY_DAY_LEN = 4,
    HDR_ACQ_YEAR = 12,       // Acquisition date year, u_int
    HDR_ACQ_YEAR_LEN = 4,
    HDR_ACQ_MONTH = 16,      // Acquisition date month, u_int
    HDR_ACQ_MONTH_LEN = 4,
    HDR_ACQ_DAY = 20,        // Acquisition date day, u_int
    HDR_ACQ_DAY_LEN = 4,
    HDR_IMAGE_YEAR = 24,     // Image date year, u_int
    HDR_IMAGE_YEAR_LEN = 4,
    HDR_IMAGE_MONTH = 28,    // Image date month, u_int
    HDR_IMAGE_MONTH_LEN = 4,
    HDR_IMAGE_DAY = 32,      // Image date day, u_int
    HDR_IMAGE_DAY_LEN = 4,
    HDR_STUDY_HOUR = 36,     // Study time hour, u_int
    HDR_STUDY_HOUR_LEN = 4,
    HDR_STUDY_MIN = 40,      // Study time minute, u_int
    HDR_STUDY_MIN_LEN = 4,
    HDR_STUDY_SEC = 44,      // Study time second, u_int
    HDR_STUDY_SEC_LEN = 4,
    HDR_ACQ_HOUR = 52,       // Acquisition time second, u_int
    HDR_ACQ_HOUR_LEN = 4,
    HDR_ACQ_MIN = 56,        // Acquisition time second, u_int
    HDR_ACQ_MIN_LEN = 4,
    HDR_ACQ_SEC = 60,        // Acquisition time second, u_int
    HDR_ACQ_SEC_LEN = 4,
    HDR_IMAGE_HOUR = 68,     // Image Creation time second, u_int
    HDR_IMAGE_HOUR_LEN = 4,
    HDR_IMAGE_MIN = 72,      // Image Creation time second, u_int
    HDR_IMAGE_MIN_LEN = 4,
    HDR_IMAGE_SEC = 76,      // Image Creation time second, u_int
    HDR_IMAGE_SEC_LEN = 4,
    HDR_MANUFAC = 96,        // Scanner Manufacturer, char
    HDR_MANUFAC_LEN = 7,
    HDR_INSTUTE_NAME = 105,  // Institution Name, char
    HDR_INSTUTE_NAME_LEN = 25,
    HDR_ANNOTATION = 186,    // Annotation, char
    HDR_ANNOTATION_LEN = 32,
    HDR_MODEL_NAME = 281,    // Scanner Model Name, char
    HDR_MODEL_NAME_LEN = 12,
    HDR_LMOVE_YEAR = 412,  // Date of Last Image Move - year, u_int
    HDR_LMOVE_YEAR_LEN = 4,
    HDR_LMOVE_MONTH = 416, // Date of Last Image Move - month, u_int
    HDR_LMOVE_MONTH_LEN = 4,
    HDR_LMOVE_DAY = 420,    // Date of Last Image Move - day, u_int
    HDR_LMOVE_DAY_LEN = 4,
    HDR_LMOVE_HOUR = 424,  // Date of Last Image Move - hour, u_int
    HDR_LMOVE_HOUR_LEN = 4,
    HDR_LMOVE_MIN = 428, // Date of Last Image Move - minute, u_int
    HDR_LMOVE_MIN_LEN = 4,
    HDR_LMOVE_SEC = 432, // Date of Last Image Move - second, u_int
    HDR_LMOVE_SEC_LEN = 4,
    HDR_PAT_NAME = 768,      // Patient Name, char
    HDR_PAT_NAME_LEN = 25,
    HDR_PAT_ID = 795,        // Patient ID Number, char
    HDR_PAT_ID_LEN = 12,
    HDR_DOB_YEAR = 808,      // Date of Birth year, u_int
    HDR_DOB_YEAR_LEN = 4,
    HDR_DOB_MONTH = 812,     // Date of Birth month, u_int
    HDR_DOB_MONTH_LEN = 4,
    HDR_DOB_DAY = 816,       // Date of Birth day, u_int
    HDR_DOB_DAY_LEN = 4,
    HDR_PAT_AGE = 851,       // Patient Age, char
    HDR_PAT_AGE_LEN = 3,
    HDR_AGE_UNIT = 854,      // Patient Age Unit, char
    HDR_AGE_UNIT_LEN = 1,
    HDR_REG_YEAR = 1052,     // Registration Date year, u_int
    HDR_REG_YEAR_LEN = 4,
    HDR_REG_MONTH = 1056,    // Registration Date month, u_int
    HDR_REG_MONTH_LEN = 4,
    HDR_REG_DAY = 1060,      // Registration Date day, u_int
    HDR_REG_DAY_LEN = 4,
    HDR_REG_HOUR = 1064,     // Registration Time hour, u_int
    HDR_REG_HOUR_LEN = 4,
    HDR_REG_MIN = 1068,      // Registration Time minute, u_int
    HDR_REG_MIN_LEN = 4,
    HDR_REG_SEC = 1072,      // Registration Time second, u_int
    HDR_REG_SEC_LEN = 4,
    HDR_SLICE_THCK = 1544,   // Slice thickness, double
    HDR_SLICE_THCK_LEN = 8,
    HDR_TR = 1560,           // TR, double
    HDR_TR_LEN = 8,
    HDR_TE = 1568,           // TE, double
    HDR_TE_LEN = 8,
    HDR_FREQ = 1592,         // Center Frequency, double
    HDR_FREQ_LEN = 8,
    HDR_STATION = 1639,      // Station Name, char
    HDR_STATION_LEN = 5,
    HDR_CAL_YEAR = 1712,     // Calibration Date - year, u_int
    HDR_CAL_YEAR_LEN = 4,
    HDR_CAL_MONTH = 1716,    // Calibration Date - month, u_int
    HDR_CAL_MONTH_LEN = 4,
    HDR_CAL_DAY = 1720,      // Calibration Date - day, u_int
    HDR_CAL_DAY_LEN = 4,
    HDR_CAL_HOUR = 1724,     // Calibration Time - hour, u_int
    HDR_CAL_HOUR_LEN = 4,
    HDR_CAL_MIN = 1728,      // Calibration Time - minute, u_int
    HDR_CAL_MIN_LEN = 4,
    HDR_CAL_SEC = 1732,      // Calibration Time - second, u_int
    HDR_CAL_SEC_LEN = 4,
    HDR_COIL = 1767,         // Receive Coil, char
    HDR_COIL_LEN = 16,
    HDR_IMAGE_NUC = 1828,    // Imaged Nucleus, char
    HDR_IMAGE_NUC_LEN = 4,
    HDR_FLIP_ANGLE = 2112,   // Flip Angle, double
    HDR_FLIP_ANGLE_LEN = 8,
    HDR_FIELD = 2560,        // Field Strength, double
    HDR_FIELD_LEN = 8,
    HDR_DISPLAY_SIZE = 2864, // Displayed Matrix Size, u_int
    HDR_DISPLAY_SIZE_LEN = 4,
    HDR_SEQPROG_NAME = 2944, // Pulse Sequence Program Name, cha
    HDR_SEQPROG_NAME_LEN = 65,
    HDR_WKC_NAME = 3009,     // Pulse Sequence Name, char
    HDR_WKC_NAME_LEN = 65,
    HDR_AUTHOR = 3074,       // Pulse Sequence Author, char
    HDR_AUTHOR_LEN = 9,
    HDR_SEQUENCE_TYPE = 3083, // Pulse Sequence Type, char
    HDR_SEQUENCE_TYPE_LEN = 8,
    HDR_FOV_ROW = 3744,      // Row FOV, double
    HDR_FOV_ROW_LEN = 8,
    HDR_FOV_COLUMN = 3752,   // Column FOV, double
    HDR_FOV_COLUMN_LEN = 8,
    HDR_CENTER_X = 3768,     // X Center Point, double
    HDR_CENTER_X_LEN = 8,
    HDR_CENTER_Y = 3776,     // Y Center Point, double
    HDR_CENTER_Y_LEN = 8,
    HDR_CENTER_Z = 3784,     // Z Center Point, double
    HDR_CENTER_Z_LEN = 8,
    HDR_NORMV_X = 3792,      // Nornal Vector X, double
    HDR_NORMV_X_LEN = 8,
    HDR_NORMV_Y = 3800,      // Nornal Vector Y, double
    HDR_NORMV_Y_LEN = 8,
    HDR_NORMV_Z = 3808,      // Nornal Vector Z, double
    HDR_NORMV_Z_LEN = 8,
    HDR_DIST_ISO = 3816,     // Distance from Isocenter, double
    HDR_DIST_ISO_LEN = 8,
    HDR_ROWV_X = 3832,       // Row vector X, double
    HDR_ROWV_X_LEN = 8,
    HDR_ROWV_Y = 3840,       // Row vector Y, double
    HDR_ROWV_Y_LEN = 8,
    HDR_ROWV_Z = 3848,       // Row vector Z, double
    HDR_ROWV_Z_LEN = 8,
    HDR_COLMNV_X = 3856,     // Column vector X, double
    HDR_COLMNV_X_LEN = 8,
    HDR_COLMNV_Y = 3864,     // Column vector Y, double
    HDR_COLMNV_Y_LEN = 8,
    HDR_COLMNV_Z = 3872,     // Column vector Z, double
    HDR_COLMNV_Z_LEN = 8,
    HDR_ORNT_SET1X = 3880,   // Orientation Set 1 - X, char
    HDR_ORNT_SET1X_LEN = 3,
    HDR_ORNT_SET1Y = 3884,   // Orientation Set 1 - Y, char
    HDR_ORNT_SET1Y_LEN = 3,
    HDR_ORNT_SET1Z = 3888,   // Orientation Set 1 - Z, char
    HDR_ORNT_SET1Z_LEN = 3,
    HDR_ORNT_SET2X = 3892,   // Orientation Set 2 - X, char
    HDR_ORNT_SET2X_LEN = 3,
    HDR_ORNT_SET2Y = 3896,   // Orientation Set 2 - Y, char
    HDR_ORNT_SET2Y_LEN = 3,
    HDR_ORNT_SET2Z = 3900,   // Orientation Set 2 - Z, char
    HDR_ORNT_SET2Z_LEN = 3,
    HDR_PROTOCOL_NAME = 2944, // Pulse Sequence Name, char
    HDR_PROTOCOL_NAME_LEN = 64,
    HDR_PIXELSIZE_ROW = 5000, // Row Pixel Size, double
    HDR_PIXELSIZE_ROW_LEN = 8,
    HDR_PIXELSIZE_CLMN = 5008, // Column Pixel Size, double
    HDR_PIXELSIZE_CLMN_LEN = 8,
    TEXT_PAT_ID = 5504,      // Text Patient ID, char
    TEXT_PAT_ID_LEN = 12,
    TEXT_PAT_SEX = 5517,     // Text Patient Sex, char
    TEXT_PAT_SEX_LEN = 1,
    TEXT_PAT_AGE = 5518,     // Text Patient Age, char
    TEXT_PAT_AGE_LEN = 3,
    TEXT_AGE_UNIT = 5521,   // Text Patient Age Unit (Y=year), char
    TEXT_AGE_UNIT_LEN = 1,
    TEXT_PAT_POS = 5529,     // Text Patient Position, char
    TEXT_PAT_POS_LEN = 7,
    TEXT_IMG_FLAG = 5541,    // Text Image Flag (IMAGE=image), char
    TEXT_IMG_FLAG_LEN = 5,
    TEXT_IMG_NUMBER = 5546,  // Text Image Number, char
    TEXT_IMG_NUMBER_LEN = 4,
    TEXT_IMG_DAY = 5559,     // Text Date - Day, char
    TEXT_IMG_DAY_LEN = 2,
    TEXT_IMG_MONTH = 5562,   // Text Date - Month, char
    TEXT_IMG_MONTH_LEN = 3,
    TEXT_IMG_YEAR = 5566,    // Text Date - Year, char
    TEXT_IMG_YEAR_LEN = 4,
    TEXT_IMG_HOUR = 5571,    // Text Time - hour, char
    TEXT_IMG_HOUR_LEN = 2,
    TEXT_IMG_MIN = 5574,     // Text Time - minute, char
    TEXT_IMG_MIN_LEN = 2,
    TEXT_ACQ_FLAG = 5577, // Text Acq. Time Flag (TA=time of acq), char
    TEXT_ACQ_FLAG_LEN = 2,
    TEXT_ACQ_MIN = 5583,     // Text Acq Time - minute, char
    TEXT_ACQ_MIN_LEN = 2,
    TEXT_ACQ_SEC = 5586,     // Text Acq Time - second, char
    TEXT_ACQ_SEC_LEN = 2,
    TEXT_ANNOTATION = 5601,  // Text Annotation, char
    TEXT_ANNOTATION_LEN = 32,
    TEXT_ORGANIZATION = 5655, // Text Organization, char
    TEXT_ORGANIZATION_LEN = 25,
    TEXT_STATION = 5682,     // Text Station Name, char
    TEXT_STATION_LEN = 5,
    TEXT_ACQ_MTRX_PHASE = 5695, // Acq. Matrix Phase Axis, char
    TEXT_ACQ_MTRX_PHASE_LEN = 3,
    TEXT_ACQ_PHASE_DIR = 5698, // Acq. phase direction (h=hor, v=vert), char
    TEXT_ACQ_PHASE_DIR_LEN = 1,
    TEXT_ACQ_MTRX_FREQ = 5700, // Acq. matrix Freq Axis, char
    TEXT_ACQ_MTRX_FREQ_LEN = 3,
    TEXT_ACQ_MTRX_FREQO = 5703, // Acq freq (o=o  " "=blank), char
    TEXT_ACQ_MTRX_FREQO_LEN = 1,
    TEXT_ACQ_MTRX_FREQS = 5704, // Acq freq (s=s  " "=blank), char
    TEXT_ACQ_MTRX_FREQS_LEN = 1,
    TEXT_SEQUENCE = 5706,    // Sequence Type, char
    TEXT_SEQUENCE_LEN = 8,
    TEXT_FLIP_ANGLE = 5714,  // Flip Angle, char
    TEXT_FLIP_ANGLE_LEN = 3,
    TEXT_SCAN_FLAG = 5718,   // Scan flag ("SCAN"), char */
    TEXT_SCAN_FLAG_LEN = 4,
    TEXT_SCANA_NUM = 5723,   // Scan Number A, char
    TEXT_SCANA_NUM_LEN = 3,
    TEXT_SCANB_NUM = 5726,   // Scan Number B, char
    TEXT_SCANB_NUM_LEN = 3,
    TEXT_TR_FLAG = 5730,     // TR flag ("TR"), char
    TEXT_TR_FLAG_LEN = 2,
    TEXT_TR = 5734,          // TR, char
    TEXT_TR_LEN = 7,
    TEXT_TE_FLAG = 5742,     // TE flag ("TE"), char
    TEXT_TE_FLAG_LEN = 2,
    TEXT_TE = 5746,          // TE, char
    TEXT_TE_LEN = 5,
    TEXT_ECHO_NUM = 5752,    // Echo number, char
    TEXT_ECHO_NUM_LEN = 1,
    TEXT_THCK_FLAG = 5790,   // SLice thickness flag ("SL"), char
    TEXT_THCK_FLAG_LEN = 2,
    TEXT_SLICE_THCK = 5794,  // Slice thickness
    TEXT_SLICE_THCK_LEN = 7,
    TEXT_POS_FLAG = 5802,    // Slice position flag ("SP"), char
    TEXT_POS_FLAG_LEN = 2,
    TEXT_SLICE_POS = 5806,   // Slice position, char
    TEXT_SLICE_POS_LEN = 7,
    TEXT_ANGLE_FLAG1 = 5814, // Angle flag 1 ("Sag","Cor",or,"Tra"), char
    TEXT_ANGLE_FLAG1_LEN = 3,
    TEXT_ANGLE_FLAG2 = 5817, // Angle flag 2 ("<", or ">"), char
    TEXT_ANGLE_FLAG2_LEN = 1,
    TEXT_ANGLE_FLAG3 = 5818, // Angle flag 3 ("Sag","Cor",or,"Tra"), char
    TEXT_ANGLE_FLAG3_LEN = 3,
    TEXT_ANGLE = 5821,       // Angle, char
    TEXT_ANGLE_LEN = 4,
    TEXT_FOV_FLAG = 5838,    // FOV flag ("FOV"), char
    TEXT_FOV_FLAG_LEN = 3,
    TEXT_FOVH = 5842,        // Horizontal FOV, char
    TEXT_FOVH_LEN = 3,
    TEXT_FOVV = 5846,        // Vertical FOV, char
    TEXT_FOVV_LEN = 3,
    TEXT_TABLEPOS_FLAG = 5874, // Table Position flag ("TP"), char
    TEXT_TABLEPOS_FLAG_LEN = 2,
    TEXT_TABLE_POS = 5878,   // Table position
    TEXT_TABLE_POS_LEN = 7,
    TEXT_STUDY_FLAG = 5938,  // Study flag ("STUDY"), char
    TEXT_STUDY_FLAG_LEN = 5,
    TEXT_STUDY_NUM = 5943,   // Study number, char
    TEXT_STUDY_NUM_LEN = 2,
    TEXT_DOB_DAY = 5956,     // Date of Birth - day, char
    TEXT_DOB_DAY_LEN = 2,
    TEXT_DOB_MONTH = 5959,   // Date of Birth - month, char
    TEXT_DOB_MONTH_LEN = 3,
    TEXT_DOB_YEAR = 5963,    // Date of Birth - year, char
    TEXT_DOB_YEAR_LEN = 4,
    TEXT_STUDY_FLAG2 = 5992, // Study flag 2 ("STU"), char
    TEXT_STUDY_FLAG2_LEN = 3,
    TEXT_IMAGE_FLAG2 = 5996, // Image flag 2 ("IMA"), char
    TEXT_IMAGE_FLAG2_LEN = 3,
    TEXT_STUDY_NUM2 = 5999,  // Study number 2, char
    TEXT_STUDY_NUM2_LEN = 2,
    TEXT_IMAGE_NUM2 = 6002,  // Image number 2, char
    TEXT_IMAGE_NUM2_LEN = 2,
    TEXT_IMAGE_NUM3 = 6013,  // Image number 3, char
    TEXT_IMAGE_NUM3_LEN = 5,
    TEXT_MODEL_NAME = 6031,  // Model Name, char
    TEXT_MODEL_NAME_LEN = 15,
    TEXT_PAT_NAME = 6058,    // Patient Name, char
    TEXT_PAT_NAME_LEN = 25,
    TEXT_START_HOUR = 6085,  // Scan start time - hour, char
    TEXT_START_HOUR_LEN = 2,
    TEXT_START_MIN = 6088,   // Scan start time - minute, char
    TEXT_START_MIN_LEN = 2,
    TEXT_START_SEC = 6091,   // Scan start time - second, char
    TEXT_START_SEC_LEN = 2,
    HDR_TOTAL_LENGTH = 6144 // Total length of Siemens Header
    } SiemensVisionHeaderInfo;
  ITK_DISALLOW_COPY_AND_ASSIGN(SiemensVisionImageIO);
};
} // end namespace itk

#endif // itkSiemensVisionImageIO_h
