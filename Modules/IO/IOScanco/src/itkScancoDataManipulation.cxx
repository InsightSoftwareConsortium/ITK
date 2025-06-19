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
#include "itkScancoDataManipulation.h"
#include <cstring>
#include <cstdint>
#include <ctime>
#include <map>
#include <cstdio>    // for sscanf
#include <stdexcept> // for std::runtime_error
#include <string>    // for std::string
#include <ctime>

/** VMS time conversion constants */
/** This is the offset between the astronomical "Julian day", which counts
 * days since January 1, 4713BC, and the "VMS epoch", which counts from
 * November 17, 1858:
 */
constexpr uint64_t julianOffset = 2400001;
constexpr uint64_t millisPerSecond = 1000;
constexpr uint64_t millisPerMinute = 60 * 1000;
constexpr uint64_t millisPerHour = 3600 * 1000;
constexpr uint64_t millisPerDay = 3600 * 24 * 1000;

const std::map<std::string, int> monthIndex{ { "XXX", 0 },  { "JAN", 1 },  { "FEB", 2 }, { "MAR", 3 }, { "APR", 4 },
                                             { "MAY", 5 },  { "JUN", 6 },  { "JUL", 7 }, { "AUG", 8 }, { "SEP", 9 },
                                             { "OCT", 10 }, { "NOV", 11 }, { "DEC", 12 } };

static constexpr const char * monthStrings[] = { "XXX", "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                                                 "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" };

int
CheckVersion(const char header[16])
{
  int fileType = 0;

  if (strncmp(header, "CTDATA-HEADER_V1", 16) == 0)
  {
    fileType = 1;
  }
  else if (strcmp(header, "AIMDATA_V030   ") == 0)
  {
    fileType = 3;
  }
  else
  {
    fileType = 2;
  }

  return fileType;
}

int
DecodeInt(const void * data)
{
  const auto * cp = static_cast<const unsigned char *>(data);
  return (cp[0] | (cp[1] << 8) | (cp[2] << 16) | (cp[3] << 24));
}


void
EncodeInt(int data, void * target)
{
  auto * targetAsUnsignedChar = static_cast<unsigned char *>(target);
  targetAsUnsignedChar[0] = (unsigned char)(data);
  targetAsUnsignedChar[1] = (unsigned char)(data >> 8);
  targetAsUnsignedChar[2] = (unsigned char)(data >> 16);
  targetAsUnsignedChar[3] = (unsigned char)(data >> 24);
}

void
EncodeInt64(int64_t data, void * target)
{
  auto * targetAsUnsignedChar = static_cast<unsigned char *>(target);
  targetAsUnsignedChar[0] = (unsigned char)(data);
  targetAsUnsignedChar[1] = (unsigned char)(data >> 8);
  targetAsUnsignedChar[2] = (unsigned char)(data >> 16);
  targetAsUnsignedChar[3] = (unsigned char)(data >> 24);
  targetAsUnsignedChar[4] = (unsigned char)(data >> 32);
  targetAsUnsignedChar[5] = (unsigned char)(data >> 40);
  targetAsUnsignedChar[6] = (unsigned char)(data >> 48);
  targetAsUnsignedChar[7] = (unsigned char)(data >> 56);
}

float
DecodeFloat(const void * data)
{
  const auto * cp = static_cast<const unsigned char *>(data);
  // different ordering and exponent bias than IEEE 754 float
  union
  {
    float        f;
    unsigned int i;
  } v;
  v.i = (cp[0] << 16) | (cp[1] << 24) | cp[2] | (cp[3] << 8);
  return 0.25 * v.f;
}

void
EncodeFloat(float data, void * target)
{
  // Reverse the scaling applied in DecodeFloat
  union
  {
    float        f;
    unsigned int i;
  } v;
  v.f = data / 0.25;

  auto * targetUnsigned = static_cast<unsigned char *>(target);
  targetUnsigned[0] = (unsigned char)(v.i >> 16);
  targetUnsigned[1] = (unsigned char)(v.i >> 24);
  targetUnsigned[2] = (unsigned char)(v.i);
  targetUnsigned[3] = (unsigned char)(v.i >> 8);
}


double
DecodeDouble(const void * data)
{
  // different ordering and exponent bias than IEEE 754 double
  const auto * cp = static_cast<const unsigned char *>(data);
  union
  {
    double   d;
    uint64_t l;
  } v;
  unsigned int l1, l2;
  l1 = (cp[0] << 16) | (cp[1] << 24) | cp[2] | (cp[3] << 8);
  l2 = (cp[4] << 16) | (cp[5] << 24) | cp[6] | (cp[7] << 8);
  v.l = (static_cast<uint64_t>(l1) << 32) | l2;
  return v.d * 0.25;
}

void
EncodeDouble(double data, void * target)
{
  // Reverse the scaling applied in DecodeDouble
  union
  {
    double   d;
    uint64_t l;
  } v;
  v.d = data / 0.25;

  unsigned int l1 = static_cast<unsigned int>(v.l >> 32);
  unsigned int l2 = static_cast<unsigned int>(v.l & 0xFFFFFFFF);

  auto * cp = static_cast<unsigned char *>(target);
  cp[0] = (unsigned char)(l1 >> 16);
  cp[1] = (unsigned char)(l1 >> 24);
  cp[2] = (unsigned char)(l1);
  cp[3] = (unsigned char)(l1 >> 8);

  cp[4] = (unsigned char)(l2 >> 16);
  cp[5] = (unsigned char)(l2 >> 24);
  cp[6] = (unsigned char)(l2);
  cp[7] = (unsigned char)(l2 >> 8);
}

/** This algorithm is from Henry F. Fliegel and Thomas C. Van Flandern
 * This uses the Gregorian calendar starting from October 15, 1582
 * \param julianDay The Julian day vaulue to convert
 * \param year The extracted Gregorian year
 * \param month The extracted Gregorian month
 * \param day The extracted Gregorian day
 */
void
GregorianDateFromJulian(int julianDay, int & year, int & month, int & day)
{
  int ell, n, i, j = 0;
  ell = julianDay + 68569;
  n = (4 * ell) / 146097;
  ell = ell - (146097 * n + 3) / 4;
  i = (4000 * (ell + 1)) / 1461001;
  ell = ell - (1461 * i) / 4 + 31;
  j = (80 * ell) / 2447;
  day = ell - (2447 * j) / 80;
  ell = j / 11;
  month = j + 2 - (12 * ell);
  year = 100 * (n - 49) + i + ell;
}

void
DecodeDate(const void * data, int & year, int & month, int & day, int & hour, int & minute, int & second, int & millis)
{
  // Read the date as a long integer with units of 1e-7 seconds
  int      d1 = DecodeInt(data);
  int      d2 = DecodeInt(static_cast<const char *>(data) + 4);
  uint64_t tVMS = d1 + (static_cast<uint64_t>(d2) << 32);
  uint64_t time = tVMS / 10000 + julianOffset * millisPerDay;

  // Extract the number of days since the Julian epoch
  int julianDay = static_cast<int>(time / millisPerDay);
  time -= millisPerDay * julianDay;

  GregorianDateFromJulian(julianDay, year, month, day);

  // Convert the remaining time into hours, minutes, seconds, and milliseconds
  hour = static_cast<int>(time / millisPerHour);
  time -= hour * millisPerHour;
  minute = static_cast<int>(time / millisPerMinute);
  time -= minute * millisPerMinute;
  second = static_cast<int>(time / millisPerSecond);
  time -= second * millisPerSecond;
  millis = static_cast<int>(time);
}

void
DateToString(const void * target, int year, int month, int day, int hour, int minute, int second, int millis)
{
  snprintf((char *)target,
           32,
           "%d-%s-%d %02d:%02d:%02d.%03d",
           (day % 100),
           monthStrings[month],
           (year % 10000),
           (hour % 100),
           (minute % 100),
           (second % 100),
           (millis % 1000));
}

void
GetCurrentDateString(void * target)
{
  time_t      rawtime;
  struct tm * timeinfo;

  std::time(&rawtime);
  timeinfo = localtime(&rawtime);
  // tm_year gives years since 1900
  DateToString(target,
               timeinfo->tm_year + 1900,
               timeinfo->tm_mon,
               timeinfo->tm_mday,
               timeinfo->tm_hour,
               timeinfo->tm_min,
               timeinfo->tm_sec,
               0);
}

void
EncodeCurrentDate(void * target)
{
  char * dateString = new char[32];
  GetCurrentDateString(dateString);
  EncodeDateFromString(target, dateString);
  delete[] dateString;
}

/** This algorithm is from Henry F. Fliegel and Thomas C. Van Flandern
 * It converts a Gregorian date to a Julian day number.
 *
 * \param year The year in the Gregorian calendar
 * \param month The month in the Gregorian calendar
 * \param day The day in the Gregorian calendar
 */
int
julianDayFromDate(int year, int month, int day)
{
  // If month is January or February, treat them as months 13 and 14 of the previous year
  if (month <= 2)
  {
    year -= 1;
    month += 12;
  }
  int a = year / 100;
  int b = 2 - a + (a / 4);
  int julianDay = static_cast<int>(365.25 * (year + 4716)) + static_cast<int>(30.6001 * (month + 1)) + day + b - 1524.5;
  return julianDay;
}

void
EncodeDateFromString(void * target, const char dateString[32])
{
  int  year, day, hour, minute, second, millis = 0;
  char monthStr[4];
  if (sscanf(dateString, "%d-%3s-%d %d:%d:%d.%d", &day, monthStr, &year, &hour, &minute, &second, &millis) != 7)
  {
    throw std::runtime_error("Invalid date string format. Expected format: YYYY-MMM-DD HH:MM:SS.mmm");
  }

  int        month = 0;
  const auto iter = monthIndex.find(monthStr);

  if (iter != monthIndex.cend())
  {
    month = iter->second;
  }
  else
  {
    month = 0;
  }

  // add time values to single time total in ms
  uint64_t timestamp = hour * millisPerHour + minute * millisPerMinute + second * millisPerSecond + millis;

  // Calculate the Julian day from the date
  int julianDay = julianDayFromDate(year, month, day);

  uint64_t time = static_cast<uint64_t>(julianDay * millisPerDay) + timestamp;
  uint64_t tVMS = (time - julianOffset * millisPerDay) * 10000; // Convert to VMS format

  int d1 = static_cast<int>(tVMS);
  int d2 = static_cast<int>(tVMS >> 32);
  EncodeInt(d1, target);
  EncodeInt(d2, static_cast<char *>(target) + 4);
}

void
StripString(char * dest, const char * source, size_t length)
{
  char * dp = dest;
  for (size_t i = 0; i < length && *source != '\0'; ++i)
  {
    *dp++ = *source++;
  }
  while (dp != dest && dp[-1] == ' ')
  {
    --dp;
  }
  *dp = '\0';
}

void
PadString(char * dest, const char * source, size_t length)
{
  for (size_t i = 0; i < length && *source != '\0'; ++i)
  {
    *dest++ = *source++;
  }
  for (size_t i = 0; i < length; ++i)
  {
    *dest++ = ' ';
  }
}
