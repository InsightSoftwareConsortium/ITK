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
 
#ifndef _LOG4CXX_HELPERS_DATE_LAYOUT_H
#define _LOG4CXX_HELPERS_DATE_LAYOUT_H

#include <log4cxx/layout.h>

namespace log4cxx
{
  namespace helpers
  {
    class DateFormat;
    class TimeZone;
    typedef helpers::ObjectPtrT<TimeZone> TimeZonePtr;
  
    /**
    This abstract layout takes care of all the date related options and
    formatting work.
    */
     class LOG4CXX_EXPORT DateLayout : public Layout
    {
    public:
      /**
      String constant designating no time information. Current value of
      this constant is <b>NULL</b>.
      */
      static String NULL_DATE_FORMAT;

      /**
      String constant designating relative time. Current value of
      this constant is <b>RELATIVE</b>.
      */
      static String RELATIVE_TIME_DATE_FORMAT;

      static String DATE_FORMAT_OPTION;
      static String TIMEZONE_OPTION;

    private:
      String timeZoneID;
      String dateFormatOption;
      
    protected:
      DateFormat * dateFormat;

    public:
      DateLayout();
      virtual ~DateLayout();

      virtual void activateOptions();
      virtual void setOption(const String& option, const String& value);

      /**
      The value of the <b>DateFormat</b> option should be either an
      argument to the constructor of helpers::DateFormat or one of
      the srings <b>"NULL"</b>, <b>"RELATIVE"</b>, <b>"ABSOLUTE"</b>,
      <b>"DATE"</b> or <b>"ISO8601</b>.
      */
      inline void setDateFormat(const String& dateFormat)
        { this->dateFormatOption = dateFormat; }

      /**
      Returns value of the <b>DateFormat</b> option.
      */
      inline const String& getDateFormat() const
        { return dateFormatOption; }

      /**
      The <b>TimeZoneID</b> option is a time zone ID string in the format
      expected by the <code>locale</code> C++ standard class.
      */
      inline void setTimeZone(const String& timeZone)
        { this->timeZoneID = timeZone; }

      /**
      Returns value of the <b>TimeZone</b> option.
      */
      inline const String& getTimeZone() const
        { return timeZoneID; }
        
      void formatDate(ostream &os, const spi::LoggingEventPtr& event) const;

    protected:
      /**
      Sets the DateFormat used to format date and time in the time zone
      determined by <code>timeZone</code> parameter. The 
      helpers::DateFormat DateFormat used
      will depend on the <code>dateFormatType</code>.

      <p>The recognized types are #NULL_DATE_FORMAT, 
      #RELATIVE_TIME_DATE_FORMAT, 
      helpers::AbsoluteTimeDateFormat#ABS_TIME_DATE_FORMAT,
      helpers::AbsoluteTimeDateFormat#DATE_AND_TIME_DATE_FORMAT and 
      helpers::AbsoluteTimeDateFormat#ISO8601_DATE_FORMAT. If the
      <code>dateFormatType</code> is not one of the above, then the
      argument is assumed to be a date pattern for 
      helpers::DateFormat.
      */
      void setDateFormat(const String& dateFormatType, 
        const TimeZonePtr& timeZone);

     };
  }  // namespace helpers
}; // namespace log4cxx

#endif // _LOG4CXX_HELPERS_DATE_LAYOUT_H
