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
#include <cstddef>

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

/** Convert 32-bit int (little-endian) to char data.
 *
 * \param data The integer to convert.
 * \param target Pointer to a buffer of at least 4 bytes to store the result.
 */
void
EncodeInt(int data, void * target);

/** Convert char data to float (single precision).
 *
 * \param data Pointer to a buffer of at least 4 bytes.
 * \return The decoded float value.
 */
float
DecodeFloat(const void * data);

/** Convert char data to float (double precision).
 *
 * \param data Pointer to a buffer of at least 8 bytes.
 * \return The decoded double value.
 */
double
DecodeDouble(const void * data);

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
