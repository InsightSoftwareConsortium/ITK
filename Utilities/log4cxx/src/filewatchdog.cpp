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
 
#include <log4cxx/helpers/filewatchdog.h>
#include <log4cxx/helpers/loglog.h>
#include <sys/stat.h>
#include <errno.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

long FileWatchdog::DEFAULT_DELAY = 60000; 

FileWatchdog::FileWatchdog(const String& filename)
 : filename(filename), lastModif(0), delay(DEFAULT_DELAY),
warnedAlready(false), interrupted(false)
{
}

void FileWatchdog::checkAndConfigure()
{
  struct stat fileStats;

  USES_CONVERSION
  if (::stat(T2A(filename.c_str()), &fileStats) != 0)
  {
    if (errno == ENOENT)
    {
      if(!warnedAlready) 
      {
        LogLog::debug(_T("[")+filename+_T("] does not exist."));
        warnedAlready = true;
      }
    }
    else
    {
      LogLog::warn(_T("Was not able to read check file existance, file:[")+
        filename+_T("]."));
      interrupted = true; // there is no point in continuing
    }
  }
  else
  {
    if (fileStats.st_mtime > lastModif)
    { 
      lastModif = fileStats.st_mtime;
      doOnChange();
      warnedAlready = false;
    }
  }
}

void FileWatchdog::run()
{    
    while(!interrupted) 
  {
    Thread::sleep(delay);
    checkAndConfigure();
    }
}

void FileWatchdog::start()
{
  checkAndConfigure();
  Thread::start();
}

