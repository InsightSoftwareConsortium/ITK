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
 
#ifndef _LOG4CXX_DAILY_ROLLING_FILE_APPENDER_H
#define _LOG4CXX_DAILY_ROLLING_FILE_APPENDER_H

#include <log4cxx/fileappender.h>
#include <log4cxx/helpers/timezone.h>

namespace log4cxx
{
  namespace helpers
  {
    class DateFormat;
  } 

  /**
  *  RollingCalendar is a helper class to DailyRollingFileAppender.
  *  Given a periodicity type and the current time, it computes the
  *  start of the next interval.  
  * */
  class LOG4CXX_EXPORT RollingCalendar
  {
  public:
    // The code assumes that the following constants are in a increasing
    // sequence.
    typedef enum PeriodicityType
    {
      TOP_OF_TROUBLE  =-1,
      TOP_OF_MINUTE  = 0,
      TOP_OF_HOUR    = 1,
      HALF_DAY    = 2,
      TOP_OF_DAY    = 3,
      TOP_OF_WEEK    = 4,
      TOP_OF_MONTH  = 5,
    };
    
    RollingCalendar();
    RollingCalendar(const helpers::TimeZonePtr& timeZone);

    inline void setType(PeriodicityType type)
      { this->type = type; }

    /**
    This method computes the roll over period by looping over the
    periods, starting with the shortest, and stopping when the r0 is
    different from from r1, where r0 is the epoch formatted according
    the datePattern (supplied by the user) and r1 is the
    epoch+nextMillis(i) formatted according to datePattern. All date
    formatting is done in GMT and not local format because the test
    logic is based on comparisons relative to 1970-01-01 00:00:00
    GMT (the epoch).
    */
    PeriodicityType computeTriggeringPeriod(const String& datePattern);
    void printPeriodicity();
    int64_t getNextCheckMillis(int64_t now);
    
    /**
    Gets the time zone.
    @return the time zone object associated with this calendar.
    */
    inline const helpers::TimeZonePtr getTimeZone() const
      { return timeZone; }  
      
    /**
    Sets the time zone with the given time zone value.
    @return value the given time zone.
    */
    inline void setTimeZone(const helpers::TimeZonePtr& timeZone)
      { this->timeZone = timeZone; }
    
  protected:
    PeriodicityType type;
    helpers::TimeZonePtr timeZone;
    static helpers::TimeZonePtr GMT_TIMEZONE;
  };
  
  class DailyRollingFileAppender;
  typedef helpers::ObjectPtrT<DailyRollingFileAppender> DailyRollingFileAppenderPtr;

  /**
  DailyRollingFileAppender extends {@link FileAppender} so that the
  underlying file is rolled over at a user chosen frequency.

  <p>The rolling schedule is specified by the <b>DatePattern</b>
  option. This pattern should follow the helpers::DateFormat
  conventions. In particular, you <em>must</em> escape literal text
  within a pair of single quotes. A formatted version of the date
  pattern is used as the suffix for the rolled file name.

  <p>For example, if the <b>File</b> option is set to
  <code>/foo/bar.log</code> and the <b>DatePattern</b> set to
  <code>'.'yyyy-MM-dd</code>, on 2001-02-16 at midnight, the logging
  file <code>/foo/bar.log</code> will be copied to
  <code>/foo/bar.log.2001-02-16</code> and logging for 2001-02-17
  will continue in <code>/foo/bar.log</code> until it rolls over
  the next day.

  <p>Is is possible to specify monthly, weekly, half-daily, daily,
  hourly, or minutely rollover schedules.

  <p><table border="1" cellpadding="2">
  <tr>
  <th>DatePattern</th>
  <th>Rollover schedule</th>
  <th>Example</th>

  <tr>
  <td><code>'.'yyyy-MM</code>
  <td>Rollover at the beginning of each month</td>

  <td>At midnight of May 31st, 2002 <code>/foo/bar.log</code> will be
  copied to <code>/foo/bar.log.2002-05</code>. Logging for the month
  of June will be output to <code>/foo/bar.log</code> until it is
  also rolled over the next month.

  <tr>
  <td><code>'.'yyyy-ww</code>

  <td>Rollover at the first day of each week. The first day of the
  week depends on the locale.</td>

  <td>Assuming the first day of the week is Sunday, on Saturday
  midnight, June 9th 2002, the file <i>/foo/bar.log</i> will be
  copied to <i>/foo/bar.log.2002-23</i>.  Logging for the 24th week
  of 2002 will be output to <code>/foo/bar.log</code> until it is
  rolled over the next week.

  <tr>
  <td><code>'.'yyyy-MM-dd</code>

  <td>Rollover at midnight each day.</td>

  <td>At 00:00 AM March 8th, 2002, <code>/foo/bar.log</code> will be
  copied to <code>/foo/bar.log.2002-03-07</code>. Logging for the 8th
  day of March will be output to <code>/foo/bar.log</code> until it
  is rolled over the next day.

  <tr>
  <td><code>'.'yyyy-MM-dd-a</code>

  <td>Rollover at midnight and midday of each day.</td>

  <td>At noon, on March 9th, 2002, <code>/foo/bar.log</code> will be
  copied to <code>/foo/bar.log.2002-03-09-AM</code>. Logging for the
  afternoon of the 9th will be output to <code>/foo/bar.log</code>
  until it is rolled over at midnight.

  <tr>
  <td><code>'.'yyyy-MM-dd-HH</code>

  <td>Rollover at the top of every hour.</td>

  <td>At approximately 11:00.000 o'clock on March 9th, 2002,
  <code>/foo/bar.log</code> will be copied to
  <code>/foo/bar.log.2002-03-09-10</code>. Logging for the 11th hour
  of the 9th of March will be output to <code>/foo/bar.log</code>
  until it is rolled over at the beginning of the next hour.


  <tr>
  <td><code>'.'yyyy-MM-dd-HH-mm</code>

  <td>Rollover at the beginning of every minute.</td>

  <td>At approximately 11:23,000, on March 9th, 2001,
  <code>/foo/bar.log</code> will be copied to
  <code>/foo/bar.log.2001-03-09-11-22</code>. Logging for the minute
  of 11:23 (9th of March) will be output to
  <code>/foo/bar.log</code> until it is rolled over the next minute.

  </table>

  <p>Do not use the colon ":" character in anywhere in the
  <b>DatePattern</b> option. The text before the colon is interpeted
  as the protocol specificaion of a URL which is probably not what
  you want.
  **/
  class LOG4CXX_EXPORT DailyRollingFileAppender : public FileAppender
  {
  public:
    DECLARE_LOG4CXX_OBJECT(DailyRollingFileAppender)
    BEGIN_LOG4CXX_CAST_MAP()
      LOG4CXX_CAST_ENTRY(DailyRollingFileAppender)
      LOG4CXX_CAST_ENTRY_CHAIN(FileAppender)
    END_LOG4CXX_CAST_MAP()

    /**
    The default constructor does nothing. */
    DailyRollingFileAppender();

    /**
    Instantiate a <code>DailyRollingFileAppender</code> and open the
    file designated by <code>filename</code>. The opened filename will
    become the ouput destination for this appender.
    */
    DailyRollingFileAppender(LayoutPtr& layout, 
      const String& filename, const String& datePattern);

    ~DailyRollingFileAppender();

    /**
    The <b>DatePattern</b> takes a string in the same format as
    expected by helpers::DateFormat. This options determines the
    rollover schedule.
    */
    inline void setDatePattern(const String& pattern)
      { datePattern = pattern; }

    /** Returns the value of the <b>DatePattern</b> option. */
    inline const String& getDatePattern() const
      { return datePattern; }

    void activateOptions();
    void setOption(const String& option,
      const String& value);

  protected:
    /**
    Rollover the current file to a new file.
    */
    void rollOver();


    /**
    * This method differentiates DailyRollingFileAppender from its
    * super class.
    *
    * <p>Before actually logging, this method will check whether it is
    * time to do a rollover. If it is, it will schedule the next
    * rollover time and then rollover.
    * */
    virtual void subAppend(const spi::LoggingEventPtr& event);

    /**
    The date pattern. By default, the pattern is set to
    "'.'yyyy-MM-dd" meaning daily rollover.
    */
    String datePattern;

    /**
    The log file will be renamed to the value of the
    scheduledFilename variable when the next interval is entered. For
    example, if the rollover period is one hour, the log file will be
    renamed to the value of "scheduledFilename" at the beginning of
    the next hour.

    The precise time when a rollover occurs depends on logging
    activity.
    */
    String scheduledFilename;

    /**
    The next time we estimate a rollover should occur. */
    int64_t nextCheck;
    int64_t now;
    helpers::DateFormat * df;
    RollingCalendar rc;
  };
} 

#endif //_LOG4CXX_DAILY_ROLLING_FILE_APPENDER_H
