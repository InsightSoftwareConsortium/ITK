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
 
#include <log4cxx/patternlayout.h>
#include <log4cxx/helpers/patternparser.h>
#include <log4cxx/helpers/patternconverter.h>
#include <log4cxx/helpers/stringhelper.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;

IMPLEMENT_LOG4CXX_OBJECT(PatternLayout)

/** Default pattern string for log output. Currently set to the
string <b>"%m%n"</b> which just prints the application supplied
message. */
String PatternLayout::DEFAULT_CONVERSION_PATTERN = _T("%m%n");

/** A conversion pattern equivalent to the TTCCCLayout.
Current value is <b>%r [%t] %p %c %x - %m%n</b>. */
String PatternLayout::TTCC_CONVERSION_PATTERN = _T("%r [%t] %p %c %x - %m%n");

int PatternLayout::BUF_SIZE = 256;
int PatternLayout::MAX_CAPACITY = 1024;

PatternLayout::PatternLayout()
{
}

/**
Constructs a PatternLayout using the supplied conversion pattern.
*/
PatternLayout::PatternLayout(const String& pattern) : pattern(pattern)
{
  activateOptions();
}

void PatternLayout::setConversionPattern(const String& conversionPattern)
{
  pattern = conversionPattern;
  activateOptions();
}

void PatternLayout::format(ostream& output, const spi::LoggingEventPtr& event) const
{
  PatternConverterPtr c = head;
  
  while(c != 0)
  {
    c->format(output, event);
    c = c->next;
  }
}

PatternConverterPtr PatternLayout::createPatternParser(const String& pattern)
{
  return PatternParser(pattern, timeZone).parse();
}

void PatternLayout::setOption(const String& option, const String& value)
{
  if (StringHelper::equalsIgnoreCase(option, _T("conversionpattern")))
  {
    pattern = value;
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("TimeZone")))
  {
    timeZone = value;
  }
}

void PatternLayout::activateOptions()
{
  if (pattern.empty())
  {
    pattern = DEFAULT_CONVERSION_PATTERN;
  }

  head = createPatternParser(pattern);
}






