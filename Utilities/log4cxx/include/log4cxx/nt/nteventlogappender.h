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
 
#ifndef _LOG4CXX_NT_EVENT_LOG_APPENDER_HEADER_
#define _LOG4CXX_NT_EVENT_LOG_APPENDER_HEADER_

#include <log4cxx/appenderskeleton.h>

typedef void * HANDLE;
struct HKEY__; 
struct _SID;
typedef struct HKEY__ *HKEY;
typedef struct _SID SID;

namespace log4cxx
{
  namespace nt
  {
    /**
     * Appends log events to NT EventLog. 
     */
    class LOG4CXX_EXPORT NTEventLogAppender : public AppenderSkeleton
    {
    public:
    DECLARE_LOG4CXX_OBJECT(NTEventLogAppender)
    BEGIN_LOG4CXX_CAST_MAP()
      LOG4CXX_CAST_ENTRY(NTEventLogAppender)
      LOG4CXX_CAST_ENTRY_CHAIN(AppenderSkeleton)
    END_LOG4CXX_CAST_MAP()

      NTEventLogAppender();
      NTEventLogAppender(const String& server, const String& log,
        const String& source, const LayoutPtr& layout);

      virtual ~NTEventLogAppender();

      virtual void activateOptions();
      virtual void close();
      virtual void setOption(const String& option, const String& value);

        /**
        * The SocketAppender does not use a layout. Hence, this method
        * returns <code>false</code>.
        * */
        bool requiresLayout() const
          { return true; }

      void setSource(const String& source)
        { this->source = source; }
      
      const String& getSource() const
        { return source; }

      void setLog(const String& log)
        { this->log = log; }
      
      const String& getLog() const
        { return log; }

      void setServer(const String& server)
        { this->server = server; }
      
      const String& getServer() const
        { return server; }

    protected:
      virtual void append(const spi::LoggingEventPtr& event);
      HKEY regGetKey(const String& subkey, unsigned long *disposition);
      void regSetString(HKEY hkey, const String& name, const String& value);
      void regSetDword(HKEY hkey, const String& name, unsigned long value);
      unsigned short getEventType(const spi::LoggingEventPtr& event);
      unsigned short getEventCategory(const spi::LoggingEventPtr& event);
      /*
       * Add this source with appropriate configuration keys to the registry.
       */
      void addRegistryInfo();

      // Data
      String server;
      String log;
      String source;
      HANDLE hEventLog;
      SID * pCurrentUserSID;
    }; // class NTEventLogAppender
    }  // namespace nt
}; // namespace log4cxx

#endif //_LOG4CXX_NT_EVENT_LOG_APPENDER_HEADER_
