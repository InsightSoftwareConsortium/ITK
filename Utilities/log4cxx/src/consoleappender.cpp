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
 
#include <log4cxx/level.h>
#include <log4cxx/consoleappender.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/helpers/loglog.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(ConsoleAppender)

String ConsoleAppender::SYSTEM_OUT = _T("System.out");
String ConsoleAppender::SYSTEM_ERR = _T("System.err");

ConsoleAppender::ConsoleAppender()
 : target(SYSTEM_OUT)
{
  os = &tcout;
}

ConsoleAppender::ConsoleAppender(const LayoutPtr& layout)
 : target(SYSTEM_OUT)
{
  this->layout = layout;
  os = &tcout;
}

ConsoleAppender::ConsoleAppender(const LayoutPtr& layout, const String& target)
 : target(SYSTEM_OUT)
{
  this->layout = layout;

  setTarget(target);
  activateOptions();
}

ConsoleAppender::~ConsoleAppender()
{
  finalize();
}

void ConsoleAppender::setTarget(const String& value)
{
  String v = StringHelper::trim(value);

  if (StringHelper::equalsIgnoreCase(SYSTEM_OUT, v))
  {
    target = SYSTEM_OUT;
  }
  else if (StringHelper::equalsIgnoreCase(SYSTEM_ERR, v))
  {
    target = SYSTEM_ERR;
  }
  else
  {
    targetWarn(value);
  }
}

const String& ConsoleAppender::getTarget() const
{
  return target;
}

void ConsoleAppender::targetWarn(const String& val)
{
  LogLog::warn(_T("[")+val+_T("] should be system.out or system.err."));
  LogLog::warn(_T("Using previously set target, System.out by default."));
}

void ConsoleAppender::activateOptions()
{
  if(StringHelper::equalsIgnoreCase(SYSTEM_OUT, target))
  {
    os = &tcout;
  }
  else if (StringHelper::equalsIgnoreCase(SYSTEM_ERR, target))
  {
    os = &tcerr;
  }
}

void ConsoleAppender::setOption(const String& option, const String& value)
{
  if (StringHelper::equalsIgnoreCase(_T("target"), option))
  {
    setTarget(value);
  }
  else
  {
    AppenderSkeleton::setOption(option, value);
  }
}






