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
 
#include <log4cxx/varia/stringmatchfilter.h>
#include <log4cxx/spi/loggingevent.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/helpers/optionconverter.h>

using namespace log4cxx;
using namespace log4cxx::varia;
using namespace log4cxx::spi;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(StringMatchFilter)

String StringMatchFilter::STRING_TO_MATCH_OPTION = _T("StringToMatch");
String StringMatchFilter::ACCEPT_ON_MATCH_OPTION = _T("AcceptOnMatch");

StringMatchFilter::StringMatchFilter() : acceptOnMatch(true)
{
}

void StringMatchFilter::setOption(const String& option,
  const String& value)
{
  if (StringHelper::equalsIgnoreCase(option, STRING_TO_MATCH_OPTION))
  {
    stringToMatch = value;
  }
  else if (StringHelper::equalsIgnoreCase(option, ACCEPT_ON_MATCH_OPTION))
  {
    acceptOnMatch = OptionConverter::toBoolean(value, acceptOnMatch);
  }
}

Filter::FilterDecision StringMatchFilter::decide(
  const log4cxx::spi::LoggingEventPtr& event) const
{
  const String& msg = event->getRenderedMessage();

  if(msg.empty() || stringToMatch.empty())
  {
    return Filter::NEUTRAL;
  }


  if( msg.find(stringToMatch) == String::npos )
  {
    return Filter::NEUTRAL;
  }
  else
  { // we've got a match
    if(acceptOnMatch)
    {
      return Filter::ACCEPT;
    }
    else
    {
      return Filter::DENY;
    }
  }
}

