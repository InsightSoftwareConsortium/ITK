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
 
#ifndef _LOG4CXX_NET_SYSLOG_APPENDER_H
#define _LOG4CXX_NET_SYSLOG_APPENDER_H
 
#include <log4cxx/appenderskeleton.h>
#include <log4cxx/helpers/syslogwriter.h>

#ifndef HAVE_SYSLOG
#endif

namespace log4cxx
{
  namespace net
  {
    class SyslogAppender;
    typedef helpers::ObjectPtrT<SyslogAppender> SyslogAppenderPtr;

    /** Use SyslogAppender to send log messages to a remote syslog daemon.*/
    class LOG4CXX_EXPORT SyslogAppender : public AppenderSkeleton
    {
    public:
      DECLARE_LOG4CXX_OBJECT(SyslogAppender)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(SyslogAppender)
        LOG4CXX_CAST_ENTRY_CHAIN(AppenderSkeleton)
      END_LOG4CXX_CAST_MAP()



      SyslogAppender();
      SyslogAppender(const LayoutPtr& layout, int syslogFacility);
      SyslogAppender(const LayoutPtr& layout,
        const String& syslogHost, int syslogFacility);
      ~SyslogAppender();
      /** Release any resources held by this SyslogAppender.*/
      void close();

      /**
      Returns the specified syslog facility as a lower-case String,
      e.g. "kern", "user", etc.
      */
      static String getFacilityString(int syslogFacility);

      /**
      Returns the integer value corresponding to the named syslog
      facility, or -1 if it couldn't be recognized.
      @param facilityName one of the strings KERN, USER, MAIL, DAEMON,
      AUTH, SYSLOG, LPR, NEWS, UUCP, CRON, AUTHPRIV, FTP, LOCAL0,
      LOCAL1, LOCAL2, LOCAL3, LOCAL4, LOCAL5, LOCAL6, LOCAL7.
      The matching is case-insensitive.
      */
      static int getFacility(const String &facilityName);

      void append(const spi::LoggingEventPtr& event);

      /**
      This method returns immediately as options are activated when they
      are set.
      */
      void activateOptions();
      void setOption(const String& option, const String& value);

      /**
      The SyslogAppender requires a layout. Hence, this method returns
      <code>true</code>.
      */
      virtual bool requiresLayout() const
        { return true; }

      /**
      The <b>SyslogHost</b> option is the name of the the syslog host
      where log output should go.
      <b>WARNING</b> If the SyslogHost is not set, then this appender
      will fail.
      */
      void setSyslogHost(const String& syslogHost);

      /**
      Returns the value of the <b>SyslogHost</b> option.
      */
      inline const String& getSyslogHost() const
        { return syslogHost; }

      /**
      Set the syslog facility. This is the <b>Facility</b> option.

      <p>The <code>facilityName</code> parameter must be one of the
      strings KERN, USER, MAIL, DAEMON, AUTH, SYSLOG, LPR, NEWS, UUCP,
      CRON, AUTHPRIV, FTP, LOCAL0, LOCAL1, LOCAL2, LOCAL3, LOCAL4,
      LOCAL5, LOCAL6, LOCAL7. Case is unimportant.
      */
      void setFacility(const String& facilityName);

      /**
      Returns the value of the <b>Facility</b> option.
      */
      inline String getFacility() const
        { return getFacilityString(syslogFacility); }

      /**
      If the <b>FacilityPrinting</b> option is set to true, the printed
      message will include the facility name of the application. It is
      <em>false</em> by default.
      */
      inline void setFacilityPrinting(bool facilityPrinting)
        { this->facilityPrinting = facilityPrinting; }

      /**
      Returns the value of the <b>FacilityPrinting</b> option.
      */
      inline bool getFacilityPrinting() const
        { return facilityPrinting; }

    protected:
      void initSyslogFacilityStr();

      int syslogFacility; // Have LOG_USER as default
      String facilityStr;
      bool facilityPrinting;
      helpers::SyslogWriter * sw;
      String syslogHost;
    }; // class SyslogAppender
    } // namespace net
}; // namespace log4cxx

#endif // _LOG4CXX_NET_SYSLOG_APPENDER_H

