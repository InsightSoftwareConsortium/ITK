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
 
#include <log4cxx/config.h>

#ifdef HAVE_SMTP

#include <log4cxx/net/smtpappender.h>
#include <log4cxx/level.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/spi/loggingevent.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/helpers/stringtokenizer.h>

extern "C" {
#include <libsmtp.h>
#include <libsmtp_mime.h>
}

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::net;
using namespace log4cxx::spi;

IMPLEMENT_LOG4CXX_OBJECT(DefaultEvaluator)
IMPLEMENT_LOG4CXX_OBJECT(SMTPAppender)

bool DefaultEvaluator::isTriggeringEvent(const spi::LoggingEventPtr& event)
{
  return event->getLevel()->isGreaterOrEqual(Level::ERROR);
}

SMTPAppender::SMTPAppender()
: bufferSize(512), locationInfo(false), cb(bufferSize),
evaluator(new DefaultEvaluator()), session(0),
encoding(_T("7bit")), charset(_T("us-ascii"))
{
}

/**
Use <code>evaluator</code> passed as parameter as the
TriggeringEventEvaluator for this SMTPAppender.  */
SMTPAppender::SMTPAppender(spi::TriggeringEventEvaluatorPtr evaluator)
: bufferSize(512), locationInfo(false), cb(bufferSize),
evaluator(evaluator), session(0),
encoding(_T("7bit")), charset(_T("us-ascii"))
{
}

SMTPAppender::~SMTPAppender()
{
  finalize();
}

void SMTPAppender::setOption(const String& option,
  const String& value)
{
  if (StringHelper::equalsIgnoreCase(option, _T("buffersize")))
  {
    setBufferSize(OptionConverter::toInt(value, 512));
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("evaluatorclass")))
  {
    setEvaluatorClass(value);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("from")))
  {
    setFrom(value);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("smtphost")))
  {
    setSMTPHost(value);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("subject")))
  {
    setSubject(value);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("to")))
  {
    setTo(value);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("charset")))
  {
    setCharset(value);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("encoding")))
  {
    setEncoding(value);
  }
  else
  {
    AppenderSkeleton::setOption(name, value);
  }
}

/**
Activate the specified options, such as the smtp host, the
recipient, from, etc. */
void SMTPAppender::activateOptions()
{
  USES_CONVERSION;

  session = ::libsmtp_session_initialize();
  if (session == 0)
  {
    LogLog::error(_T("Could not intialize session."));
    return;
  }

  char * ansiFrom = T2A((TCHAR *)from.c_str());
  char * ansiSubject = T2A((TCHAR *)subject.c_str());
  ::libsmtp_set_environment(
    ansiFrom,
    ansiSubject,
    0,
    (libsmtp_session_struct *)session);

  std::vector<String> recipients = parseAddress(to);
  std::vector<String>::iterator i;
  for (i = recipients.begin(); i != recipients.end(); i++)
  {
    if (::libsmtp_add_recipient(LIBSMTP_REC_TO,
      (TCHAR *)T2A(i->c_str()),
      (libsmtp_session_struct *)session) != 0)
    {
      LogLog::error(_T("Could not add recipient ")+*i+_T("."));
      return;
    }
  }

  // MIMEPART
  if (layout != 0)
  {
    int mimeType = 0;
    String contentType = layout->getContentType();
    if (contentType == _T("text/plain"))
    {
      mimeType = LIBSMTP_MIME_SUB_PLAIN;
    }
    else if (contentType == _T("text/html"))
    {
      mimeType = LIBSMTP_MIME_SUB_HTML;
    }
    else
    {
      LogLog::error(_T("invalid layout content type: ")+contentType+_T("."));
      return;
    }

     int charset = 0;
    if (this->charset == _T("us-ascii"))
    {
       charset = LIBSMTP_CHARSET_USASCII;
    }
    else if (this->charset == _T("iso8859_1"))
    {
       charset = LIBSMTP_CHARSET_ISO8859_1;
    }
    else if (this->charset == _T("iso8859_2"))
    {
       charset = LIBSMTP_CHARSET_ISO8859_2;
    }
    else if (this->charset == _T("iso8859_3"))
    {
       charset = LIBSMTP_CHARSET_ISO8859_3;
    }
    else
    {
      LogLog::error(_T("invalid charset: ")+this->charset+_T("."));
      return;
    }

     int encoding = 0;
    if (this->encoding == _T("7bit"))
    {
      encoding = LIBSMTP_ENC_7BIT;
    }
    else if (this->encoding == _T("8bit"))
    {
      encoding = LIBSMTP_ENC_8BIT;
    }
    else if (this->encoding == _T("binary"))
    {
      encoding = LIBSMTP_ENC_BINARY;
    }
    else if (this->encoding == _T("base64"))
    {
      encoding = LIBSMTP_ENC_BASE64;
    }
    else if (this->encoding == _T("quoted"))
    {
      encoding = LIBSMTP_ENC_QUOTED;
    }
    else
    {
      LogLog::error(_T("invalid encoding: ")+this->encoding+_T("."));
      return;
    }

    libsmtp_part_struct * part = 0;
    part = ::libsmtp_part_new(
      0,
      LIBSMTP_MIME_TEXT,
      mimeType,
      encoding,
      charset,
      "content",
      (libsmtp_session_struct *)session);
    if (part == 0)
    {
      LogLog::error(_T("Error adding part."));
    }
  }
  else
  {
    LogLog::error(_T("Layout not set !"));
  }
}

/**
Perform SMTPAppender specific appending actions, mainly adding
the event to a cyclic buffer and checking if the event triggers
an e-mail to be sent. */
void SMTPAppender::append(const spi::LoggingEventPtr& event)
{
  if(!checkEntryConditions())
  {
    return;
  }

  event->getNDC();

/*  if(locationInfo)
  {
    event.getLocationInformation();
  }*/

  cb.add(event);

  if(evaluator->isTriggeringEvent(event))
  {
    sendBuffer();
  }
}

/**
This method determines if there is a sense in attempting to append.
<p>It checks whether there is a set output target and also if
there is a set layout. If these checks fail, then the boolean
value <code>false</code> is returned. */
bool SMTPAppender::checkEntryConditions()
{
  if(to.empty() || from.empty() || subject.empty() || smtpHost.empty())
  {
    errorHandler->error(_T("Message not configured."));
    return false;
  }

  if(evaluator == 0)
  {
    errorHandler->error(_T("No TriggeringEventEvaluator is set for appender [")+
      name+_T("]."));
    return false;
  }


  if(layout == 0)
  {
    errorHandler->error(_T("No layout set for appender named [")+name+_T("]."));
    return false;
  }
  return true;
}

void SMTPAppender::close()
{
  synchronized sync(this);
  if (!this->closed && session != 0)
  {
    ::libsmtp_free((libsmtp_session_struct *)session);
    session = 0;
  }

  this->closed = true;
}

std::vector<String> SMTPAppender::parseAddress(const String& addressStr)
{
  std::vector<String> addresses;

  StringTokenizer st(addressStr, _T(","));
  while (st.hasMoreTokens())
  {
    addresses.push_back(st.nextToken());
  }

  return addresses;
}

/**
Send the contents of the cyclic buffer as an e-mail message.
*/
void SMTPAppender::sendBuffer()
{
  // Note: this code already owns the monitor for this
  // appender. This frees us from needing to synchronize on 'cb'.
  try
  {
    StringBuffer sbuf;
    layout->appendHeader(sbuf);

    int len = cb.length();
    for(int i = 0; i < len; i++)
    {
        //sbuf.append(MimeUtility.encodeText(layout.format(cb.get())));
      LoggingEventPtr event = cb.get();
      layout->format(sbuf, event);
    }

    layout->appendFooter(sbuf);

    /* This starts the SMTP connection */
    if (::libsmtp_connect(
      T2A((TCHAR *)smtpHost.c_str()),
      0,
      0,
      (libsmtp_session_struct *)session) != 0)
    {
      LogLog::error(_T("Error occured while starting the SMTP connection."));
      return;
    }

    /* This will conduct the SMTP dialogue */
    if (::libsmtp_dialogue((libsmtp_session_struct *)session) != 0)
    {
      LogLog::error(_T("Error occured while conducting the SMTP dialogue."));
      return;
    }

    /* Now lets send the headers */
    if (::libsmtp_headers((libsmtp_session_struct *)session) != 0)
    {
      LogLog::error(_T("Error occured while sending the headers."));
      return;
    }

    /* Now lets send the MIME headers */
    if (::libsmtp_mime_headers((libsmtp_session_struct *)session) != 0)
    {
      LogLog::error(_T("Error occured while sending the MIME headers."));
      return;
    }

    String s = sbuf.str();
    if (::libsmtp_part_send(
      T2A((TCHAR *)s.c_str()),
      s.length(),
      (libsmtp_session_struct *)session) != 0)
    {
      LogLog::error(_T("Error occured while sending the message body."));
    }

    /* This ends the body part */
    if (::libsmtp_body_end((libsmtp_session_struct *)session) != 0)
    {
      LogLog::error(_T("Error occured while ending the body part."));
      return;
    }

    /* This ends the connection gracefully */
    if (::libsmtp_quit((libsmtp_session_struct *)session) != 0)
    {
      LogLog::error(_T("Error occured while ending the connection."));
      return;
    }

  }
  catch(Exception& e)
  {
    LogLog::error(_T("Error occured while sending e-mail notification."), e);
  }
}

/**
Returns value of the <b>EvaluatorClass</b> option.
*/
String SMTPAppender::getEvaluatorClass()
{
  return evaluator == 0 ? String() : evaluator->getClass().getName();
}

/**
The <b>BufferSize</b> option takes a positive integer
representing the maximum number of logging events to collect in a
cyclic buffer. When the <code>BufferSize</code> is reached,
oldest events are deleted as new events are added to the
buffer. By default the size of the cyclic buffer is 512 events.
*/
void SMTPAppender::setBufferSize(int bufferSize)
{
  this->bufferSize = bufferSize;
  cb.resize(bufferSize);
}

/**
The <b>EvaluatorClass</b> option takes a string value
representing the name of the class implementing the {@link
TriggeringEventEvaluator} interface. A corresponding object will
be instantiated and assigned as the triggering event evaluator
for the SMTPAppender.
*/
void SMTPAppender::setEvaluatorClass(const String& value)
{
  evaluator = OptionConverter::instantiateByClassName(value,
    TriggeringEventEvaluator::getStaticClass(), evaluator);
}

#endif //HAVE_SMTP

