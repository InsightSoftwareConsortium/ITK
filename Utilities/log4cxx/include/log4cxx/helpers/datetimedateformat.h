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
 
#ifndef _LOG4CXX_HELPERS_DATE_TIME_DATE_FORMAT_H
#define _LOG4CXX_HELPERS_DATE_TIME_DATE_FORMAT_H

#include <log4cxx/helpers/dateformat.h>

namespace log4cxx
{
  namespace helpers
  {
    /**
    Formats a date in the format <b>\%d-\%m-\%Y \%H:\%M:\%S,\%Q</b> for example,
     "06 Nov 1994 15:49:37,459".
    */
    class LOG4CXX_EXPORT DateTimeDateFormat : public DateFormat
    {
    public:
      DateTimeDateFormat(const TimeZonePtr& timeZone)
       : DateFormat(_T("%d %b %Y %H:%M:%S,%Q"), timeZone) {}
    };
  }  // namespace helpers
}; // namespace log4cxx

#endif // _LOG4CXX_HELPERS_DATE_TIME_DATE_FORMAT_H
