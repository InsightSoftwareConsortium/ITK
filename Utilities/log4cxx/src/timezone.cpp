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
 
#include <log4cxx/helpers/timezone.h>
#include <locale>


int getYear(int64_t date)
{
  time_t d = (time_t)(date / 1000);
  return ::localtime(&d)->tm_year;
}

using namespace log4cxx;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(TimeZone)

TimeZonePtr TimeZone::defaultTimeZone = new TimeZone(_T(""));

TimeZone::TimeZone(const String& ID)
: ID(ID), rawOffset(0), DSTSavings(0)
{
  String timeZoneEnv = _T("TZ=") + ID;

  USES_CONVERSION;
  ::putenv((char *)T2A(timeZoneEnv.c_str()));
  tzset();

  time_t now = time(0);
  tm localNow = *::localtime(&now);
  tm utcNow = *::gmtime(&now);
  rawOffset = (int)::difftime(::mktime(&localNow), ::mktime(&utcNow)) * 1000;
  
  int year = localNow.tm_year;
  Rule * rule = new Rule(year);
  
  // we check if we found a daylight time
  if (rule->startDate != 0 && rule->endDate != 0)
  {
    // since we have computed a rule, we store it
    rules.insert(RuleMap::value_type(year, rule));
    DSTSavings = 3600 * 1000; // 1 hour
  }
  else
  {
    delete rule;
  }
}

TimeZone::~TimeZone()
{
  for (RuleMap::iterator it = rules.begin(); it != rules.end(); it++)
  {
    Rule * rule = it->second;
    delete rule;
  }
}

int TimeZone::getOffset(int64_t date) const
{
  if (inDaylightTime(date))
  {
    return rawOffset + DSTSavings;
  }
  else
  {
    return rawOffset;
  }
}

TimeZonePtr TimeZone::getDefault()
{
  return defaultTimeZone;
}

TimeZonePtr TimeZone::getTimeZone(const String& ID)
{
  return new TimeZone(ID);
}

bool TimeZone::inDaylightTime(int64_t date) const
{
  if (!useDaylightTime())
  {
    return false;
  }
  
  time_t d = (time_t)(date / 1000);
  int year = ::localtime(&d)->tm_year;

  RuleMap::iterator it = rules.find(year);
  if (it == rules.end())
  {
    synchronized sync(this);

    it = rules.find(year);
    if (it == rules.end())
    {
      it = rules.insert(RuleMap::value_type(
        year, new Rule(year))).first;
    }
  }

  Rule * rule = it->second;
  return (date >= rule->startDate && date < rule->endDate);
}

TimeZone::Rule::Rule(int year)
: startDate(0), endDate(0)
{
  tm tm;
  memset (&tm, 0, sizeof (tm));
  tm.tm_mday = 1;
  tm.tm_year = year;
  
  time_t t = ::mktime(&tm);
  int isDST, day, hour, min;

  for (day = 0; day < 365; day++)
  {
    t += 60 * 60 * 24;
    
    isDST = ::localtime(&t)->tm_isdst;
    if (startDate == 0)
    {
      if (isDST > 0)
      {
        t -= 60 * 60 * 24;
  
        for (hour = 0; hour < 24; hour++)
        {
          t += 60 * 60;
  
          isDST = ::localtime(&t)->tm_isdst;
          if (isDST > 0)
          {
            t -= 60 * 60;
            for (min = 0; min < 60; min++)
            {
              t += 60;
              isDST = ::localtime(&t)->tm_isdst;
              if (isDST > 0)
              {
                startDate = (int64_t)t * 1000;
                break;
              }
            }
            
            break;
          }
        }
      }
    }
    else if (isDST == 0)
    {
      t -= 60 * 60 * 24;

      for (hour = 0; hour < 24; hour++)
      {
        t += 60 * 60;

        isDST = ::localtime(&t)->tm_isdst;
        if (isDST == 0)
        {
          t -= 60 * 60;
          for (min = 0; min < 60; min++)
          {
            t += 60;
            isDST = ::localtime(&t)->tm_isdst;
            if (isDST == 0)
            {
              endDate = (int64_t)t * 1000;
              break;
            }
          }
          
          break;
        }
      }
      
      if (endDate != 0)
      {
        break;
      }
    }
  }
}


