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
 
#include <log4cxx/net/syslogappender.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/helpers/datagramsocket.h>
#include <log4cxx/spi/loggingevent.h>
#include <log4cxx/level.h>

#ifdef HAVE_SYSLOG
  #include <syslog.h>  
#else
  /* facility codes */
  #define  LOG_KERN  (0<<3)  /* kernel messages */
  #define  LOG_USER  (1<<3)  /* random user-level messages */
  #define  LOG_MAIL  (2<<3)  /* mail system */
  #define  LOG_DAEMON  (3<<3)  /* system daemons */
  #define  LOG_AUTH  (4<<3)  /* security/authorization messages */
  #define  LOG_SYSLOG  (5<<3)  /* messages generated internally by syslogd */
  #define  LOG_LPR    (6<<3)  /* line printer subsystem */
  #define  LOG_NEWS  (7<<3)  /* network news subsystem */
  #define  LOG_UUCP  (8<<3)  /* UUCP subsystem */
  #define  LOG_CRON  (9<<3)  /* clock daemon */
  #define  LOG_AUTHPRIV  (10<<3)  /* security/authorization messages (private) */
  #define  LOG_FTP    (11<<3)  /* ftp daemon */
  
    /* other codes through 15 reserved for system use */
  #define  LOG_LOCAL0  (16<<3)  /* reserved for local use */
  #define  LOG_LOCAL1  (17<<3)  /* reserved for local use */
  #define  LOG_LOCAL2  (18<<3)  /* reserved for local use */
  #define  LOG_LOCAL3  (19<<3)  /* reserved for local use */
  #define  LOG_LOCAL4  (20<<3)  /* reserved for local use */
  #define  LOG_LOCAL5  (21<<3)  /* reserved for local use */
  #define  LOG_LOCAL6  (22<<3)  /* reserved for local use */
  #define  LOG_LOCAL7  (23<<3)  /* reserved for local use */
#endif

#define LOG_UNDEF -1

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::net;

IMPLEMENT_LOG4CXX_OBJECT(SyslogAppender)

SyslogAppender::SyslogAppender()
: syslogFacility(LOG_USER), facilityPrinting(false), sw(0)
{
  this->initSyslogFacilityStr();

}

SyslogAppender::SyslogAppender(const LayoutPtr& layout,
  int syslogFacility)
: syslogFacility(syslogFacility), facilityPrinting(false), sw(0)
{
  this->layout = layout;
  this->initSyslogFacilityStr();
}

SyslogAppender::SyslogAppender(const LayoutPtr& layout,
  const String& syslogHost, int syslogFacility)
: syslogFacility(syslogFacility), facilityPrinting(false), sw(0)
{
  this->layout = layout;
  this->initSyslogFacilityStr();
  setSyslogHost(syslogHost);
}

SyslogAppender::~SyslogAppender()
{
  finalize();
}

/** Release any resources held by this SyslogAppender.*/
void SyslogAppender::close()
{
  closed = true;
  if (sw != 0)
  {
    delete sw;
    sw = 0;
  }
}

void SyslogAppender::initSyslogFacilityStr()
{
  facilityStr = getFacilityString(this->syslogFacility);

  if (facilityStr.empty())
  {
    LOGLOG_ERROR(_T("\"") << syslogFacility <<
          _T("\" is an unknown syslog facility. Defaulting to \"USER\"."));
    this->syslogFacility = LOG_USER;
    facilityStr = _T("user:");
  }
  else
  {
    facilityStr += _T(":");
  }
}

/**
Returns the specified syslog facility as a lower-case String,
e.g. "kern", "user", etc.
*/
String SyslogAppender::getFacilityString(
  int syslogFacility)
{
  switch(syslogFacility)
  {
  case LOG_KERN:      return _T("kern");
  case LOG_USER:      return _T("user");
  case LOG_MAIL:      return _T("mail");
  case LOG_DAEMON:    return _T("daemon");
  case LOG_AUTH:      return _T("auth");
  case LOG_SYSLOG:    return _T("syslog");
  case LOG_LPR:       return _T("lpr");
  case LOG_NEWS:      return _T("news");
  case LOG_UUCP:      return _T("uucp");
  case LOG_CRON:      return _T("cron");
#ifdef LOG_AUTHPRIV
  case LOG_AUTHPRIV:  return _T("authpriv");
#endif
#ifdef LOG_FTP
  case LOG_FTP:       return _T("ftp");
#endif
  case LOG_LOCAL0:    return _T("local0");
  case LOG_LOCAL1:    return _T("local1");
  case LOG_LOCAL2:    return _T("local2");
  case LOG_LOCAL3:    return _T("local3");
  case LOG_LOCAL4:    return _T("local4");
  case LOG_LOCAL5:    return _T("local5");
  case LOG_LOCAL6:    return _T("local6");
  case LOG_LOCAL7:    return _T("local7");
  default:            return String();
  }
}

int SyslogAppender::getFacility(
  const String &facilityName)
{
  String s = StringHelper::toUpperCase(StringHelper::trim(facilityName));

  if (s == _T("KERN"))
  {
    return LOG_KERN;
  }
  else if (s == _T("USER"))
  {
    return LOG_USER;
  }
  else if (s == _T("MAIL"))
  {
    return LOG_MAIL;
  }
  else if (s == _T("DAEMON"))
  {
    return LOG_DAEMON;
  }
  else if (s == _T("AUTH"))
  {
    return LOG_AUTH;
  }
  else if (s == _T("SYSLOG"))
  {
    return LOG_SYSLOG;
  }
  else if (s == _T("LPR"))
  {
    return LOG_LPR;
  }
  else if (s == _T("NEWS"))
  {
    return LOG_NEWS;
  }
  else if (s == _T("UUCP"))
  {
    return LOG_UUCP;
  }
  else if (s == _T("CRON"))
  {
    return LOG_CRON;
  }
#ifdef LOG_AUTHPRIV
  else if (s == _T("AUTHPRIV"))
  {
    return LOG_AUTHPRIV;
  }
#endif
#ifdef LOG_FTP
  else if (s == _T("FTP"))
  {
    return LOG_FTP;
  }
#endif
  else if (s == _T("LOCAL0"))
  {
    return LOG_LOCAL0;
  }
  else if (s == _T("LOCAL1"))
  {
    return LOG_LOCAL1;
  }
  else if (s == _T("LOCAL2"))
  {
    return LOG_LOCAL2;
  }
  else if (s == _T("LOCAL3"))
  {
    return LOG_LOCAL3;
  }
  else if (s == _T("LOCAL4"))
  {
    return LOG_LOCAL4;
  }
  else if (s == _T("LOCAL5"))
  {
    return LOG_LOCAL5;
  }
  else if (s == _T("LOCAL6"))
  {
    return LOG_LOCAL6;
  }
  else if (s == _T("LOCAL7"))
  {
    return LOG_LOCAL7;
  }
  else
  {
    return LOG_UNDEF;
  }
}

void SyslogAppender::append(const spi::LoggingEventPtr& event)
{
  if  (!isAsSevereAsThreshold(event->getLevel()))
    return;

// On the local host, we can directly use the system function 'syslog'
// if it is available
#ifdef HAVE_SYSLOG  
  if (sw == 0)
  {
      StringBuffer sbuf;
     layout->format(sbuf, event);
    USES_CONVERSION;
    
    // use of "%s" to avoid a security hole
        ::syslog(syslogFacility | event->getLevel()->getSyslogEquivalent(), 
      "%s", T2A(sbuf.str().c_str()));
      
    return;
  }
#endif

  // We must not attempt to append if sw is null.
  if(sw == 0)
  {
    errorHandler->error(_T("No syslog host is set for SyslogAppedender named \"")+
      this->name+_T("\"."));
    return;
  }

  StringBuffer sbuf;

  sbuf << _T("<") << (syslogFacility | event->getLevel()->getSyslogEquivalent()) << _T(">");
  if (facilityPrinting)
  {
    sbuf << facilityStr;
  }
  layout->format(sbuf, event);
  //LogLog::debug(sbuf.str());
  sw->write(sbuf.str());
}

void SyslogAppender::activateOptions()
{
}

void SyslogAppender::setOption(const String& option, const String& value)
{
  if (StringHelper::equalsIgnoreCase(option, _T("sysloghost")))
  {
    setSyslogHost(value);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("facility")))
  {
    setFacility(value);
  }
  else
  {
    AppenderSkeleton::setOption(name, value);
  }
}

void SyslogAppender::setSyslogHost(const String& syslogHost)
{
  if (this->sw != 0)
  {
    delete this->sw;
    this->sw = 0;
  }
  
// On the local host, we can directly use the system function 'syslog'
// if it is available (cf. append)
#ifdef HAVE_SYSLOG  
  if (syslogHost != _T("localhost") && syslogHost != _T("127.0.0.1")
  && !syslogHost.empty())
#endif    
    this->sw = new SyslogWriter(syslogHost);
    
  this->syslogHost = syslogHost;
}


void SyslogAppender::setFacility(const String& facilityName)
{
  if (facilityName.empty())
  {
    return;
  }

  syslogFacility = getFacility(facilityName);
  if (syslogFacility == LOG_UNDEF)
  {
    LogLog::error(_T("[")+facilityName +
        _T("] is an unknown syslog facility. Defaulting to [USER]."));
    syslogFacility = LOG_USER;
  }

  this->initSyslogFacilityStr();
}

