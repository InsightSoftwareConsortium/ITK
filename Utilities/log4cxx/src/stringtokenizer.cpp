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
 
#include <log4cxx/helpers/stringtokenizer.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

StringTokenizer::StringTokenizer(const String& str, const String& delim)
: delim(delim), state(0)
{
  this->str = new TCHAR[str.length() + 1];

#ifdef UNICODE
  wcscpy(this->str, str.c_str());
#ifdef WIN32
  token = wcstok(this->str, this->delim.c_str());
#else
  token = wcstok(this->str, this->delim.c_str(), &state);
#endif
#else
  strcpy(this->str, str.c_str());
  token = strtok(this->str, this->delim.c_str());
#endif
}

StringTokenizer::~StringTokenizer()
{
  delete this->str;
}

bool StringTokenizer::hasMoreTokens() const
{
  return (token != 0);
}

String StringTokenizer::nextToken()
{
  if (token == 0)
  {
    throw NoSuchElementException();
  }

  String currentToken = token;

#ifdef UNICODE
#ifdef WIN32
  token = wcstok(0, delim.c_str());
#else
  token = wcstok(0, delim.c_str(), &state);
#endif
#else
  token = strtok(0, delim.c_str());
#endif

  return currentToken;
}
