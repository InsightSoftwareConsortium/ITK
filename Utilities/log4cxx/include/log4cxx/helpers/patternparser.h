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
 
#ifndef _LOG4CXX_HELPER_PATTERN_PARSER_H
#define _LOG4CXX_HELPER_PATTERN_PARSER_H

#include <log4cxx/helpers/tchar.h>
#include <log4cxx/helpers/objectptr.h>
#include <log4cxx/helpers/objectimpl.h>
#include <log4cxx/helpers/formattinginfo.h>
#include <log4cxx/helpers/patternconverter.h>

namespace log4cxx
{
  namespace spi
  {
    class LoggingEvent;
    typedef helpers::ObjectPtrT<LoggingEvent> LoggingEventPtr;
  } 

  namespace helpers
  {

    class DateFormat;

  /**
  Most of the work of the PatternLayout class
  is delegated to the PatternParser class.
  
  <p>It is this class that parses conversion patterns and creates
  a chained list of {@link helpers::OptionConverter OptionConverters}.
  */
    class LOG4CXX_EXPORT PatternParser
    {
    protected:
      int state;
      StringBuffer currentLiteral;
      int patternLength;
      int i;
      PatternConverterPtr head;
      PatternConverterPtr tail;
      FormattingInfo formattingInfo;
      String pattern;
      String timeZone;

    public:
      PatternParser(const String& pattern, const String& timeZone);
      
    private:
      void addToList(PatternConverterPtr& pc);
      
    protected:
      String extractOption();
      
      /**
      The option is expected to be in decimal and positive. In case of
      error, zero is returned.  */
      int extractPrecisionOption();
      
    public:
      PatternConverterPtr parse();
      
    protected:
      void finalizeConverter(TCHAR c);

      void addConverter(PatternConverterPtr& pc);

    // ---------------------------------------------------------------------
    //                      PatternConverters
    // ---------------------------------------------------------------------
    private:
      class LOG4CXX_EXPORT BasicPatternConverter : public PatternConverter
      {
      private:
        int type;
      public:
        BasicPatternConverter(const FormattingInfo& formattingInfo, int type);
        virtual void convert(ostream& sbuf, 
          const spi::LoggingEventPtr& event) const;
      };

      class LOG4CXX_EXPORT LiteralPatternConverter : public PatternConverter
      {
      private:
        String literal;

      public:
        LiteralPatternConverter(const String& value);
        virtual void format(StringBuffer& sbuf, 
          const spi::LoggingEventPtr& e) const;
        virtual void convert(ostream& sbuf, 
          const spi::LoggingEventPtr& event) const;
      };

      class LOG4CXX_EXPORT DatePatternConverter : public PatternConverter
      {
      private:
        DateFormat * df;

      public:
        DatePatternConverter(const FormattingInfo& formattingInfo, 
          DateFormat * df);
        ~DatePatternConverter();
        
      public:
        virtual void convert(ostream& sbuf, 
          const spi::LoggingEventPtr& event) const;
      };

      class LOG4CXX_EXPORT MDCPatternConverter : public PatternConverter
      {
      private:
        String key;
      
      public:
        MDCPatternConverter(const FormattingInfo& formattingInfo,
          const String& key);
        virtual void convert(ostream& sbuf, 
          const spi::LoggingEventPtr& event) const;
      };

      class LOG4CXX_EXPORT LocationPatternConverter : public PatternConverter
      {
      private:
        int type;
      
      public:
        LocationPatternConverter(const FormattingInfo& formattingInfo, int type);
        virtual void convert(ostream& sbuf, 
          const spi::LoggingEventPtr& event) const;
      };

      class LOG4CXX_EXPORT CategoryPatternConverter : public PatternConverter
      {
      private:
        int precision;
      
      public:
        CategoryPatternConverter(const FormattingInfo& formattingInfo, 
          int precision);
        virtual void convert(ostream& sbuf, 
          const spi::LoggingEventPtr& event) const;
      };
    }; // class PatternParser
  }  // namespace helpers
}; // namespace log4cxx

#endif //_LOG4CXX_HELPER_PATTERN_PARSER_H
