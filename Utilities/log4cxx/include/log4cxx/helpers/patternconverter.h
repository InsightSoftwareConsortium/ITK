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
 
#ifndef _LOG4CXX_HELPER_PATTERN_CONVERTER_H
#define _LOG4CXX_HELPER_PATTERN_CONVERTER_H

#include <log4cxx/helpers/tchar.h>
#include <log4cxx/helpers/objectptr.h>
#include <log4cxx/helpers/objectimpl.h>

namespace log4cxx
{
  namespace spi
  {
    class LoggingEvent;
    typedef helpers::ObjectPtrT<LoggingEvent> LoggingEventPtr;
  } 

  namespace helpers
  {
    class FormattingInfo;

    class PatternConverter;
    typedef ObjectPtrT<PatternConverter> PatternConverterPtr;

    /**
    <p>PatternConverter is an abtract class that provides the
    formatting functionality that derived classes need.

    <p>Conversion specifiers in a conversion patterns are parsed to
    individual PatternConverters. Each of which is responsible for
    converting a logging event in a converter specific manner.
    */
    class LOG4CXX_EXPORT PatternConverter : public ObjectImpl
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(PatternConverter)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(PatternConverter)
      END_LOG4CXX_CAST_MAP()

      PatternConverterPtr next;
      int minChar;
      int maxChar;
      bool leftAlign;
      mutable StringBuffer os;

    protected:
      PatternConverter();
      PatternConverter(const FormattingInfo& fi);

      /**
      Derived pattern converters must override this method in order to
      convert conversion specifiers in the correct way.
      */
      virtual void convert(ostream& sbuf, 
        const spi::LoggingEventPtr& event) const = 0;

    public:
      /**
      A template method for formatting in a converter specific way.
      */
      virtual void format(ostream& sbuf, const spi::LoggingEventPtr& e) const;

    }; // class PatternConverter
  }  // namespace helpers
}; // namespace log4cxx

#endif // _LOG4CXX_HELPER_PATTERN_CONVERTER_H
