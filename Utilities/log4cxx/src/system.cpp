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
 
#include <log4cxx/helpers/system.h>

#if defined(HAVE_FTIME)
#include <sys/timeb.h>
#endif

#if defined(HAVE_GETTIMEOFDAY)
#include <sys/time.h>
#endif

#include <time.h>
#include <log4cxx/helpers/properties.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

int64_t System::currentTimeMillis()
{
#if defined(HAVE_GETTIMEOFDAY)
    timeval tp;
    ::gettimeofday(&tp, 0);

    return ((int64_t)tp.tv_sec * 1000) + (int64_t)(tp.tv_usec / 1000);
#elif defined(HAVE_FTIME)
    struct timeb tp;
    ::ftime(&tp);

    return ((int64_t)tp.time * 1000) + (int64_t)tp.millitm;
#else
    return (int64_t)::time(0) * 1000;
#endif
}

String System::getProperty(const String& key)
{
  if (key.empty())
  {
    throw IllegalArgumentException(_T("key is empty"));
  }
  
  USES_CONVERSION;
  char * value = ::getenv(T2A(key.c_str()));
  if (value == 0)
  {
    return String();
  }
  else
  {
    return A2T(value);
  }
}

void System::setProperty(const String& key, const String& value)
{
  if (key.empty())
  {
    throw IllegalArgumentException(_T("key is empty"));
  }
  
#ifndef HAVE_SETENV
  String strEnv = key + _T("=") + value;
  USES_CONVERSION;
  ::putenv((char *)T2A(strEnv.c_str()));
#else
  /* WARNING ! 
  We don't use putenv with glibc, because it doesn't make
  a copy of the string, but try to keep the pointer
  cf. man 3 putenv.
  */
  USES_CONVERSION;
  std::string name = T2A(key.c_str());
  std::string val = T2A(value.c_str());
  ::setenv(name.c_str(), val.c_str(), 1);
#endif
}

void System::setProperties(const Properties& props)
{
  std::vector<String> propertyNames = props.propertyNames();
  
  for (std::vector<String>::iterator it = propertyNames.begin();
  it != propertyNames.end(); it++)
  {
    const String& propertyName = *it;
    setProperty(propertyName, props.getProperty(propertyName));
  }
}
