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
 
#include <log4cxx/xml/xmllayout.h>
#include <log4cxx/spi/loggingevent.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/level.h>
#include <log4cxx/helpers/transform.h>
#include <log4cxx/helpers/iso8601dateformat.h>
#include <log4cxx/helpers/stringhelper.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;
using namespace log4cxx::xml;

IMPLEMENT_LOG4CXX_OBJECT(XMLLayout)

String XMLLayout::LOCATION_INFO_OPTION = _T("LocationInfo");

XMLLayout::XMLLayout()
: locationInfo(false)
{
}

void XMLLayout::setOption(const String& option,
  const String& value)
{
  if (StringHelper::equalsIgnoreCase(option, LOCATION_INFO_OPTION))
  {
    setLocationInfo(OptionConverter::toBoolean(value, false));
  }
}

void XMLLayout::format(ostream& output, const spi::LoggingEventPtr& event) const
{
  output << _T("<log4j:event logger=\"");
  output << event->getLoggerName();
  output << _T("\" timestamp=\"");
  output << event->getTimeStamp();
  output << _T("\" level=\"");
  output << event->getLevel()->toString();
  output << _T("\" thread=\"");
  output << event->getThreadId();
  output << _T("\">") << std::endl;

  output << _T("<log4j:message><![CDATA[");
  // Append the rendered message. Also make sure to escape any
  // existing CDATA sections.
  Transform::appendEscapingCDATA(output, event->getRenderedMessage());
  output << _T("]]></log4j:message>") << std::endl;

  const String& ndc = event->getNDC();
  if(ndc.length() != 0)
  {
    output << _T("<log4j:NDC><![CDATA[");
    output << ndc;
    output << _T("]]></log4j:NDC>") << std::endl;
  }

    std::set<String> mdcKeySet = event->getMDCKeySet();

    if(!mdcKeySet.empty())
    {
    /**
    * Normally a sort isn't required, but for Test Case purposes
    * we need to guarantee a particular order.
    *
    * Besides which, from a human readable point of view, the sorting
    * of the keys is kinda nice..
    */

    output << _T("<log4j:MDC>") << std::endl;
    for (std::set<String>::iterator i = mdcKeySet.begin();
      i != mdcKeySet.end(); i++)
    {
      String propName = *i;
      String propValue = event->getMDC(propName);
      output << _T("    <log4j:data name=\"") << propName;
      output << _T("\" value=\"") << propValue;
      output << _T("\"/>") << std::endl;
    }
    output << _T("</log4j:MDC>") << std::endl;
    }

  if(locationInfo)
  {
    USES_CONVERSION;
    output << _T("<log4j:locationInfo file=\"");
    output << A2T(event->getFile());
    output << _T("\" line=\"");
    output << event->getLine();
    output << _T("\"/>") << std::endl;
  }

    std::set<String> propertySet = event->getPropertyKeySet();

    if (!propertySet.empty())
  {
    output << _T("<log4j:properties>\n");
    for (std::set<String>::iterator i = propertySet.begin();
      i != propertySet.end(); i++)
    {
      String propName = *i;
      output << _T("<log4j:data name=\"") << propName;
      String propValue = event->getProperty(propName);
      output << _T("\" value=\"") << propValue;
      output << _T("\"/>") << std::endl;
    }
    output << _T("</log4j:properties>") << std::endl;
    }

  output << _T("</log4j:event>") << std::endl;
}

