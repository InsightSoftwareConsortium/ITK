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
 
#include <log4cxx/dailyrollingfileappender.h>
#include <log4cxx/helpers/system.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/dateformat.h>
#include <sys/stat.h>
#include <log4cxx/helpers/stringhelper.h>
#include <time.h>
#include <log4cxx/helpers/timezone.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

TimeZonePtr RollingCalendar::GMT_TIMEZONE = TimeZone::getTimeZone(_T("GMT"));

RollingCalendar::RollingCalendar()
: type(RollingCalendar::TOP_OF_TROUBLE),
timeZone(TimeZone::getDefault())
{
}

RollingCalendar::RollingCalendar(const helpers::TimeZonePtr& timeZone)
: type(RollingCalendar::TOP_OF_TROUBLE),
timeZone(timeZone)
{
}

RollingCalendar::PeriodicityType 
RollingCalendar::computeTriggeringPeriod(const String& datePattern)
{
  RollingCalendar rollingCalendar(GMT_TIMEZONE);
  // set date to 1970-01-01 00:00:00 GMT
  int64_t epoch = 0;
  if(!datePattern.empty())
  {
    DateFormat dateFormat(datePattern, GMT_TIMEZONE);
    putenv("TZ=GMT");
    tzset();
    for(int i = TOP_OF_MINUTE; i <= TOP_OF_MONTH; i++)
    {
      String r0 = dateFormat.format(epoch);
      rollingCalendar.setType((PeriodicityType)i);
      int64_t next = rollingCalendar.getNextCheckMillis(epoch);
      String r1 =  dateFormat.format(next);
      //tcout << _T("Type = ") << i << _T(", r0 = ") << r0 << _T(", r1 = ");
      //tcout << r1 << std::endl;
      if (!r0.empty() && !r1.empty() && r0 != r1)
      {
        return (PeriodicityType)i;
      }
    }
  }
  return TOP_OF_TROUBLE; // Deliberately head for trouble...
}

void RollingCalendar::printPeriodicity()
{
  switch(type)
  {
  case TOP_OF_MINUTE:
    LogLog::debug(_T("Rollover every minute."));
    break;
  case TOP_OF_HOUR:
    LogLog::debug(_T("Rollover at the top of every hour."));
    break;
  case HALF_DAY:
    LogLog::debug(_T("Rollover at midday and midnight."));
    break;
  case TOP_OF_DAY:
    LogLog::debug(_T("Rollover at midnight."));
    break;
  case TOP_OF_WEEK:
    LogLog::debug(_T("Rollover at start of week."));
    break;
  case TOP_OF_MONTH:
    LogLog::debug(_T("Rollover at start of every month."));
    break;
  default:
    LogLog::warn(_T("Unknown periodicity"));
  }
}

int64_t RollingCalendar::getNextCheckMillis(int64_t now)
{
  switch(type)
  {
  case TOP_OF_MINUTE:
    now += 60 * 1000;
    return now - (now % (60 * 1000));
  case TOP_OF_HOUR:
    now += 60 * 60 * 1000;
    return now - (now % (60 * 60 * 1000));
  case HALF_DAY:
    now += 12 * 60 * 60 * 1000 + timeZone->getOffset(now);
    now -= now % (12 * 60 * 60 * 1000);
    return now - timeZone->getOffset(now);
  case TOP_OF_DAY:
    now += 24 * 60 * 60 * 1000 + timeZone->getOffset(now);
    now -= now % (24 * 60 * 60 * 1000);
    return now - timeZone->getOffset(now);
  case TOP_OF_WEEK:
    now += 7 * 24 * 60 * 60 * 1000;
    return now - (now % (7 * 24 * 60 * 60 * 1000));
  case TOP_OF_MONTH:
    {
      now += timeZone->getOffset(now);
      time_t nowTime = (time_t)(now / 1000);
      struct tm * nextTime = gmtime(&nowTime);
      nextTime->tm_mday = 1;
      nextTime->tm_hour = 0;
      nextTime->tm_min = 0;
      nextTime->tm_sec = 0;
      nextTime->tm_mon += 1;
      nextTime->tm_isdst = 0;
      now = (int64_t)mktime(nextTime) * 1000;
      now -= timeZone->getOffset(now);
      return now;
    }
  default:
    throw RuntimeException(_T("Unknown periodicity type."));
  }

  return now;
}

IMPLEMENT_LOG4CXX_OBJECT(DailyRollingFileAppender)

DailyRollingFileAppender::DailyRollingFileAppender()
: datePattern(_T(".%Y-%m-%d")), df(0)
{
  nextCheck = System::currentTimeMillis() - 1;
}

DailyRollingFileAppender::DailyRollingFileAppender(LayoutPtr& layout,
  const String& filename, const String& datePattern)
  : FileAppender(layout, filename, true), datePattern(datePattern), df(0)
{
  nextCheck = System::currentTimeMillis() - 1;
  activateOptions();
}

DailyRollingFileAppender::~DailyRollingFileAppender()
{
  if (df != 0)
  {
    delete df;
  }
}

void DailyRollingFileAppender::activateOptions()
{
  FileAppender::activateOptions();

  if (!datePattern.empty() && !fileName.empty())
  {
    now = System::currentTimeMillis();
    df = new DateFormat(datePattern);
    rc.setType(rc.computeTriggeringPeriod(datePattern));
    rc.printPeriodicity();

    int64_t lastModified = 0;
    struct stat fileStats;
    USES_CONVERSION;
    if (::stat(T2A(fileName.c_str()), &fileStats) == 0)
    {
      lastModified = (int64_t)fileStats.st_mtime * 1000;
    }

    scheduledFilename = fileName + df->format(lastModified);
  } 
  else
  {
    LogLog::error(
      _T("Either File or DatePattern options are not set for appender [")
      + name + _T("]."));
  }
}

void DailyRollingFileAppender::rollOver()
{
  /* Compute filename, but only if datePattern is specified */
  if (datePattern.empty())
  {
    errorHandler->error(_T("Missing DatePattern option in rollOver()."));

    return;
  }

  String datedFilename = fileName + df->format(now);

  // It is too early to roll over because we are still within the
  // bounds of the current interval. Rollover will occur once the
  // next interval is reached.
  if (scheduledFilename == datedFilename)
  {
    return;
  }

  // close current file, and rename it to datedFilename
  this->closeWriter();

  USES_CONVERSION;
  remove(T2A(scheduledFilename.c_str()));

  std::string aFileName = T2A(fileName.c_str());
  std::string aScheduledFilename = T2A(scheduledFilename.c_str());
  if (rename(aFileName.c_str(), aScheduledFilename.c_str()) == 0)
  {
    LogLog::debug(fileName + _T(" -> ") + scheduledFilename);
  }
  else
  {
    LogLog::error(
      _T("Failed to rename [") + fileName + _T("] to [") +
      scheduledFilename + _T("]."));
  }

  try
  {
    // This will also close the file. This is OK since multiple
    // close operations are safe.
    this->setFile(fileName, false, this->bufferedIO, this->bufferSize);
  }
  catch (Exception&)
  {
    errorHandler->error(_T("setFile(") + fileName + _T(", false) call failed."));
  }

  scheduledFilename = datedFilename;
}

void DailyRollingFileAppender::subAppend(const spi::LoggingEventPtr& event)
{
  int64_t n = System::currentTimeMillis();

  if (n >= nextCheck) 
  {
    now = n;
    nextCheck = rc.getNextCheckMillis(now);

    try 
    {
      rollOver();
    } 
    catch (Exception& e)
    {
      LogLog::error(_T("rollOver() failed."), e);
    }
  }

  FileAppender::subAppend(event);
}

void DailyRollingFileAppender::setOption(const String& option,
  const String& value)
{
  if (StringHelper::equalsIgnoreCase(option, _T("datePattern")))
  {
    setDatePattern(value);
  }
  else
  {
    FileAppender::setOption(option, value);
  }
}
