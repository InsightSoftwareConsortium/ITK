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
 
#ifndef _LOG4CXX_HELPERS_DATE_FORMAT_H
#define _LOG4CXX_HELPERS_DATE_FORMAT_H

#include <log4cxx/helpers/tchar.h>
#include <log4cxx/helpers/timezone.h>

namespace log4cxx
{
  namespace helpers
  {
    /** 
    Concrete class for formatting and parsing dates in a 
    locale-sensitive manner.
    */
    class LOG4CXX_EXPORT DateFormat
    {
    public:
      DateFormat(const String& dateFormat);
      DateFormat(const String& dateFormat, const TimeZonePtr& timeZone);
      virtual void format(ostream& os, int64_t time) const;
      String format(int64_t timeMillis) const;

    protected:
      TimeZonePtr timeZone;
      String dateFormat;
    };
  }  // namespace helpers
}; // namespace log4cxx

#endif //_LOG4CXX_HELPERS_DATE_FORMAT_H
