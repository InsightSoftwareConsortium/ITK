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
 
#include <log4cxx/helpers/stringhelper.h>
#include <algorithm>
#include <vector>

using namespace log4cxx;
using namespace log4cxx::helpers;

String StringHelper::toUpperCase(const String& s)
{
  String d;
  std::transform(s.begin(), s.end(),
    std::insert_iterator<String>(d, d.begin()), totupper);
  return d;
}

String StringHelper::toLowerCase(const String& s)
{
  String d;
  std::transform(s.begin(), s.end(),
    std::insert_iterator<String>(d, d.begin()), totlower);
  return d;
}

String StringHelper::trim(const String& s)
{
  String::size_type pos = s.find_first_not_of(_T(' '));
  if (pos == String::npos)
  {
    return String();
  }

  String::size_type n = s.find_last_not_of(_T(' ')) - pos + 1;
  return s.substr(pos, n);
}

bool StringHelper::equalsIgnoreCase(const String& s1, const String& s2)
{
  return toLowerCase(s1) == toLowerCase(s2);
}

bool StringHelper::endsWith(const String& s, const String& suffix)
{
  return (s.length() - s.rfind(suffix)) == suffix.length();
}

String StringHelper::format(const String& pattern, va_list argList)
{
  int args = 0;
  const TCHAR * pch = pattern.c_str();  
  while (*pch != _T('\0'))
  {
    if (pch[0] == _T('{') && pch[1] >= _T('0')
      && pch[1] <= _T('9') && pch[2] == _T('}'))
    {
      int arg = pch[1] - '0' + 1;
      if (arg > args)
      {
        args = arg;
      }
      
      pch += 3;
    }
    else
    {
      ++pch;
    }
  }
  
  std::vector<TCHAR *> params(args);
  for (int arg = 0; arg < args; arg++)
  {
    params[arg] = va_arg(argList, TCHAR *);
  }
  
  StringBuffer result;
  
  pch = pattern.c_str();  
  while (*pch != _T('\0'))
  {
    if (pch[0] == _T('{') && pch[1] >= _T('0')
      && pch[1] <= _T('9') && pch[2] == _T('}'))
    {
      int arg = pch[1] - '0';
      
      result << params[arg];
      pch += 3;
    }
    else
    {
      result.put(*pch);
      ++pch;
    }
  }
  
  return result.str();    
}


