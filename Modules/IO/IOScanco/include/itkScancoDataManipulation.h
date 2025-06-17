/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkScancoDataManipulation_h
#define itkScancoDataManipulation_h
#include "itkMacro.h"
#include <cstddef>
#include <array>

struct itkScancoPixelData
{
  int    m_Dimensions[3]; // Dimensions of the pixel data
  float  m_Origin[3];     // Origin of the pixel data in physical space
  double m_Spacing[3];    // Spacing between pixels in physical space
  int    m_ComponentType; // Data Type (e.g., unsigned char, short, float)
  int    m_PixelType;
};

struct itkScancoHeaderData
{
  char                  m_Version[18]; // Version string, e.g., "AIMDATA_V020   "
  char                  m_PatientName[42];
  int                   m_PatientIndex;
  int                   m_ScannerID;
  char                  m_CreationDate[32];
  char                  m_ModificationDate[32];
  int                   m_ScanDimensionsPixels[3];
  double                m_ScanDimensionsPhysical[3];
  double                m_SliceThickness; // Slice thickness in mm
  double                m_SliceIncrement; // Slice increment in mm
  double                m_StartPosition;
  double                m_EndPosition;
  double                m_ZPosition;
  std::array<double, 2> m_DataRange;
  double                m_MuScaling;
  int                   m_NumberOfSamples;
  int                   m_NumberOfProjections;
  double                m_ScanDistance;
  double                m_SampleTime;
  int                   m_ScannerType;
  int                   m_MeasurementIndex;
  int                   m_Site;
  int                   m_ReconstructionAlg;
  double                m_ReferenceLine;
  double                m_Energy;
  double                m_Intensity;
  int                   m_RescaleType;
  char                  m_RescaleUnits[18];
  char                  m_CalibrationData[66];
  double                m_RescaleSlope;
  double                m_RescaleIntercept;
  double                m_MuWater;
  char *                m_RawHeader;
  itkScancoPixelData    m_PixelData; // Pixel data information
};

#define ScancoGetConstMacro(name, type)                                  \
  virtual type Get##name() const { return this->m_HeaderData.m_##name; } \
  ITK_MACROEND_NOOP_STATEMENT

#define ScancoSetMacro(name, type)                   \
  virtual void Set##name(type _arg)                  \
  {                                                  \
    itkDebugMacro("setting " #name " to " << _arg);  \
    ITK_GCC_PRAGMA_PUSH                              \
    ITK_GCC_SUPPRESS_Wfloat_equal                    \
    if (this->m_HeaderData.m_##name != _arg)         \
    {                                                \
      this->m_HeaderData.m_##name = std::move(_arg); \
      this->Modified();                              \
    }                                                \
    ITK_GCC_PRAGMA_POP                               \
  }                                                  \
  ITK_MACROEND_NOOP_STATEMENT

constexpr int ScancoHeaderBlockSize = 512;

/** Check the file header to see what type of file it is.
 *
 *  \param header A pointer to the first 16 bytes of the file header.
 *  \return 0 if unrecognized, 1 if ISQ/RAD,
 *  2 if AIM 020, 3 if AIM 030.
 */
int
CheckVersion(const char header[16]);

/** Convert char data to 32-bit int (little-endian).
 *
 * \param data Pointer to a buffer of at least 4 bytes.
 * \return The decoded integer value.
 */
int
DecodeInt(const void * data);

/** Convert char data to 64-bit int (little-endian)
 *
 * \param data Pointer to a buffer of at least 4 bytes.
 * \return The decoded integer value.
 */
int64_t
DecodeInt64(const void * data);

/** Convert 32-bit int (little-endian) to char data.
 *
 * \param data The integer to convert.
 * \param target Pointer to a buffer of at least 4 bytes to store the result.
 */
void
EncodeInt(int data, void * target);

/** Convert 64-bit int (little-endian) to char data.
 *
 * \param data The integer to convert.
 * \param target Pointer to a buffer of at least 8 bytes to store the result.
 */
void
EncodeInt64(int64_t data, void * target);

/** Convert char data to float (single precision).
 *
 * \param data Pointer to a buffer of at least 4 bytes.
 * \return The decoded float value.
 */
float
DecodeFloat(const void * data);

/** Convert float data to char data.
 *
 * \param data The float to convert.
 * \param target Pointer to a buffer of at least 4 bytes to store the result.
 */
void
EncodeFloat(float data, void * target);

/** Convert char data to float (double precision).
 *
 * \param data Pointer to a buffer of at least 8 bytes.
 * \return The decoded double value.
 */
double
DecodeDouble(const void * data);

void
EncodeDouble(double data, void * target);

/** Convert a VMS timestamp to a calendar date.
 *
 * \param data Pointer to a buffer of at least 8 bytes containing the VMS timestamp.
 * \param year The extracted Gregorian year.
 * \param month The extracted Gregorian month (1-12).
 * \param day The extracted Gregorian day (1-31).
 * \param hour The extracted hour (0-23).
 * \param minute The extracted minute (0-59).
 * \param second The extracted second (0-59).
 * \param millis The extracted milliseconds (0-999).
 */
void
DecodeDate(const void * data, int & year, int & month, int & day, int & hour, int & minute, int & second, int & millis);

/** Formats a date into a string of the form DD-MMM-YYYY HH:MM:SS:mmm
 * \param target Pointer to a buffer of at least 32 bytes to store the formatted date string.
 * \param year The year.
 * \param month The month (1-12).
 * \param day The day (1-31).
 * \param hour The hour (0-23).
 * \param minute The minute (0-59).
 * \param second The second (0-59).
 * \param millis The milliseconds (0-999).
 */
void
DateToString(const void * target, int year, int month, int day, int hour, int minute, int second, int millis);

/** Get the current date and time as a string in the format "YYYY-MM-DD HH:MM:SS.mmm".
 * \param target Pointer to a buffer of at least 32 bytes to store the current date string.
 */
void
GetCurrentDateString(void * target);

/** Convert the current calendar date to a VMS timestamp and store in target
 *
 * \param target Pointer to a buffer of at least 8 bytes to store the VMS timestamp.
 */
void
EncodeCurrentDate(void * target);

/** Convert a calendar date to VMS timestamp
 *
 * \param target Pointer to a buffer of at least 8 bytes to store the timestamp.
 * \param dateString  A string in the format "YYYY-MM-DD HH:MM:SS.mmm"
 */
void
EncodeDateFromString(void * target, const char dateString[32]);

/** Strip a string by removing trailing whitespace.
 *
 *  \attention dest must have a size of at least l+1.
 *  \param dest The destination string to be stripped.
 *  \param source The source string to copy from.
 *  \param length The total length of the destination string.
 */
void
StripString(char * dest, const char * source, size_t length);

/** Pad a string with spaces to a specified length.
 *
 *  \attention dest must have a size of at least l+1.
 *  \param dest The destination string to be padded.
 *  \param source The source string to copy from.
 *  \param length The total length of the destination string after padding.
 */
void
PadString(char * dest, const char * source, size_t length);

#endif // itkScancoDataManipulation_h
