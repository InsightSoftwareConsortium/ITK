/*
 * Copyright 2003,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
 
#include <log4cxx/helpers/exception.h>
#include <log4cxx/helpers/dateformat.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/absolutetimedateformat.h>
#include <iomanip> // for setw & setfill
#include <time.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

String AbsoluteTimeDateFormat::ISO8601_DATE_FORMAT = _T("ISO8601");
String AbsoluteTimeDateFormat::ABS_TIME_DATE_FORMAT = _T("ABSOLUTE");
String AbsoluteTimeDateFormat::DATE_AND_TIME_DATE_FORMAT = _T("DATE");

DateFormat::DateFormat(const String& dateFormat)
 : dateFormat(dateFormat), timeZone(TimeZone::getDefault())
{
  size_t pos = this->dateFormat.find(_T("%Q"));
  if (pos != String::npos)
  {
    this->dateFormat = this->dateFormat.substr(0, pos) +
      _T("%") + this->dateFormat.substr(pos);
  }
}

DateFormat::DateFormat(const String& dateFormat, const TimeZonePtr& timeZone)
 : dateFormat(dateFormat), timeZone(timeZone)
{
  size_t pos = this->dateFormat.find(_T("%Q"));
  if (pos != String::npos)
  {
    this->dateFormat = this->dateFormat.substr(0, pos) +
      _T("%") + this->dateFormat.substr(pos);
  }
}

void DateFormat::format(ostream& os, int64_t timeMillis) const
{
    TCHAR buffer[255];

  if (timeZone == 0)
  {
    throw NullPointerException(_T("timeZone is null"));
  }

  int64_t localTimeMillis = timeMillis + timeZone->getOffset(timeMillis);

  time_t time = (time_t)(localTimeMillis/1000);
  const tm * tm = ::gmtime(&time);

#ifdef UNICODE
  size_t len = ::wcsftime(buffer, 255, dateFormat.c_str(), tm);
#else
  size_t len = ::strftime(buffer, 255, dateFormat.c_str(), tm);
#endif

  buffer[len] = '\0';
  String result(buffer);

  size_t pos = result.find(_T("%Q"));
  if (pos != String::npos)
  {
    os << result.substr(0, pos)
       << std::setw(3) << std::setfill(_T('0')) << (long)(timeMillis % 1000)
       << result.substr(pos + 2);
  }
  else
  {
    os << result;
  }
}

String DateFormat::format(int64_t timeMillis) const
{
  StringBuffer sbuf;
  format(sbuf, timeMillis);
  return sbuf.str();
}
