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
 
#include <log4cxx/helpers/transform.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

String Transform::CDATA_START  = _T("<![CDATA[");
String Transform::CDATA_END    = _T("]]>");
String Transform::CDATA_PSEUDO_END = _T("]]&gt;");
String Transform::CDATA_EMBEDED_END = CDATA_END + CDATA_PSEUDO_END + CDATA_START;
String::size_type Transform::CDATA_END_LEN = CDATA_END.length();


void Transform::appendEscapingTags(
  ostream& buf, const String& input)
{
  //Check if the string is zero length -- if so, return
  //what was sent in.

  if(input.length() == 0 )
  {
    return;
  }

  String::const_iterator it = input.begin();
  String::const_iterator itEnd = input.end();
  TCHAR ch;
  while(it != itEnd)
  {
    ch = *it++;
    if(ch == _T('<'))
    {
      buf << _T("&lt;");
    }
    else if(ch == _T('>'))
    {
      buf << _T("&gt;");
    }
    else
    {
      buf.put(ch);
    }
  }
}

void Transform::appendEscapingCDATA(
  ostream& buf, const String& input)
{
  if(input.length() == 0 )
  {
    return;
  }

  String::size_type end = input.find(CDATA_END);
  if (end == String::npos)
  {
    buf << input;
    return;
  }

  String::size_type start = 0;
  while (end != String::npos)
  {
    buf << input.substr(start, end-start);
    buf << CDATA_EMBEDED_END;
    start = end + CDATA_END_LEN;
    if (start < input.length())
    {
      end = input.find(CDATA_END, start);
    }
    else
    {
      return;
    }
  }

  buf << input.substr(start);
}

