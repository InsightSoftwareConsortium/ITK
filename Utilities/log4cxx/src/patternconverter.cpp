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
 
#include <log4cxx/helpers/patternconverter.h>
#include <log4cxx/helpers/formattinginfo.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(PatternConverter)

PatternConverter::PatternConverter() : minChar(-1), maxChar(0x7FFFFFFF), leftAlign(false)
{
}

PatternConverter::PatternConverter(const FormattingInfo& fi)
{
  minChar = fi.minChar;
  maxChar = fi.maxChar;
  leftAlign = fi.leftAlign;
}

/**
A template method for formatting in a converter specific way.
*/
void PatternConverter::format(ostream& sbuf, const spi::LoggingEventPtr& e) const
{
  if (minChar == -1 && maxChar == 0x7FFFFFFF)
  {
    convert(sbuf, e);
  }
  else
  {
    os.seekp(0);
    convert(os, e);
    String s = os.str();

    if (s.empty())
    {
      if(0 < minChar)
        sbuf << String(minChar, _T(' '));
      return;
    }

    int len = s.size();

    if (len > maxChar)
    {
      sbuf << (s.substr(len-maxChar));
    }
    else if (len < minChar)
    {
      if (leftAlign)
      {
        sbuf << s;
        sbuf << String(minChar-len, _T(' '));
      }
      else
      {
        sbuf << String(minChar-len, _T(' '));
        sbuf << s;
      }
    }
    else
      sbuf << s;
  }
}


