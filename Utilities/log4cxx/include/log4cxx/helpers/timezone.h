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
 
#ifndef _LOG4CXX_HELPERS_TIMEZONE_H
#define _LOG4CXX_HELPERS_TIMEZONE_H

#include <log4cxx/config.h>
#include <log4cxx/helpers/tchar.h>
#include <log4cxx/helpers/objectimpl.h>
#include <log4cxx/helpers/objectptr.h>
#include <map>

namespace log4cxx
{
  namespace helpers
  {
    class TimeZone;
    typedef helpers::ObjectPtrT<TimeZone> TimeZonePtr;

    class LOG4CXX_EXPORT TimeZone : public helpers::ObjectImpl
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(TimeZone)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(TimeZone)
      END_LOG4CXX_CAST_MAP()

      TimeZone(const String& ID);
      ~TimeZone();

      /**
      Returns the offset of this time zone from UTC at the specified
      date. If Daylight Saving Time is in effect at the specified date,
      the offset value is adjusted with the amount of daylight saving.

      @param date the date represented in milliseconds since January 1, 
      1970 00:00:00 GMT 
      @return the amount of time in milliseconds to add to UTC to get
      local time.
      */
      int getOffset(int64_t date) const;

      static TimeZonePtr getDefault();
      static TimeZonePtr getTimeZone(const String& ID);

      /** 
      Queries if the given date is in daylight savings time in this time
      zone.
      @param date the given Date. 
      @return true if the given date is in daylight savings time, false,
      otherwise.
      */
      bool inDaylightTime(int64_t date) const;

      /**
      Returns the amount of time to be added to local standard time to
      get  local wall clock time.

      @return the amount of saving time in milliseconds
      */
      inline int getDSTSavings() const
        { return DSTSavings; }

      /**
      Returns the amount of time in milliseconds to add to UTC to get
      standard time in this time zone. 
      
      Because this value is not affected by daylight saving time, it is
      called raw offset.

      @return the amount of raw offset time in milliseconds to add to
      UTC.
      */
      inline int getRawOffset() const
        { return rawOffset; }
        
      /**
      Queries if this time zone uses daylight savings time.
      @return true if this time zone uses daylight savings time, false,
      otherwise.
      */
      inline bool useDaylightTime() const
        { return DSTSavings != 0; }

    protected:
      String ID;
      int rawOffset;
      int DSTSavings;

      class Rule
      {
      public:
        Rule(int year);
        int year;
        int64_t startDate;
        int64_t endDate;
      };

      typedef std::map<long, Rule *> RuleMap;
      mutable RuleMap rules;
      
      static TimeZonePtr defaultTimeZone;
    };
  } 
} 

#endif //_LOG4CXX_HELPERS_TIMEZONE_H
