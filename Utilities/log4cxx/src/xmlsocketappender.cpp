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

#include <log4cxx/net/xmlsocketappender.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/socketoutputstream.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/xml/xmllayout.h>
#include <log4cxx/level.h>
#include <log4cxx/helpers/transform.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::net;
using namespace log4cxx::xml;

IMPLEMENT_LOG4CXX_OBJECT(XMLSocketAppender)

// The default port number of remote logging server (4560)
int XMLSocketAppender::DEFAULT_PORT                 = 4560;

// The default reconnection delay (30000 milliseconds or 30 seconds).
int XMLSocketAppender::DEFAULT_RECONNECTION_DELAY   = 30000;

const int XMLSocketAppender::MAX_EVENT_LEN         = 1024;

XMLSocketAppender::XMLSocketAppender()
: port(DEFAULT_PORT), reconnectionDelay(DEFAULT_RECONNECTION_DELAY), 
locationInfo(false), connector(0)
{
  layout = new XMLLayout();
  memset(zeroBuffer, 0, MAX_EVENT_LEN);
}

XMLSocketAppender::XMLSocketAppender(unsigned long address, int port)
: port(port), reconnectionDelay(DEFAULT_RECONNECTION_DELAY), 
locationInfo(false), connector(0)
{
  layout = new XMLLayout();
  memset(zeroBuffer, 0, MAX_EVENT_LEN);
  this->address.address = address;
  remoteHost = this->address.getHostName();
  connect();
}

XMLSocketAppender::XMLSocketAppender(const String& host, int port)
: address(InetAddress::getByName(host)), port(port),
reconnectionDelay(DEFAULT_RECONNECTION_DELAY), locationInfo(false),
remoteHost(host), connector(0)
{
  layout = new XMLLayout();
  memset(zeroBuffer, 0, MAX_EVENT_LEN);
  connect();
}

XMLSocketAppender::~XMLSocketAppender()
{
  finalize();
}

void XMLSocketAppender::activateOptions()
{
  connect();
}

void XMLSocketAppender::setOption(const String& option,
  const String& value)
{
  if (StringHelper::equalsIgnoreCase(option, _T("remotehost")))
  {
    setRemoteHost(value);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("port")))
  {
    setPort(OptionConverter::toInt(value, DEFAULT_PORT));
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("locationinfo")))
  {
    setLocationInfo(OptionConverter::toBoolean(value, false));
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("reconnectiondelay")))
  {
    setReconnectionDelay(OptionConverter::toInt(value, DEFAULT_RECONNECTION_DELAY));
  }
  else
  {
    AppenderSkeleton::setOption(name, value);
  }
}

void XMLSocketAppender::close()
{
  synchronized sync(this);

  if(closed)
  {
    return;
  }

  closed = true;
  cleanUp();
}

void XMLSocketAppender::cleanUp()
{
  if(os != 0)
  {
    try
    {
      os->close();
    }
    catch(IOException& e)
    {
      LogLog::error(_T("Could not close socket :"), e);
    }
    
    os = 0;
  }
  
  if(connector != 0)
  {
    //LogLog::debug(_T("Interrupting the connector."));
    connector->interrupted = true;
    connector = 0;
  }
  
}

void XMLSocketAppender::connect()
{
  if(address.address == 0)
  {
    return;
  }
  
  try
  {
    // First, close the previous connection if any.
    cleanUp();
    
    SocketPtr socket = new Socket(address, port);
    os = socket->getOutputStream();
  }
  catch(SocketException& e)
  {
    String msg = _T("Could not connect to remote log4cxx server at [")

      +address.getHostName()+_T("].");
      
    if(reconnectionDelay > 0)
    {
      msg += _T(" We will try again later. ");
    }

    fireConnector(); // fire the connector thread
    
    LogLog::error(msg, e);
  }
}

void XMLSocketAppender::append(const spi::LoggingEventPtr& event)
{
  if(address.address == 0)
  {
    errorHandler->error(
      _T("No remote host is set for XMLSocketAppender named \"") +
      name+_T("\"."));
    return;
  }

  if(os != 0) try
  {
/*
    if(locationInfo)
    {
      event.getLocationInformation();
    }
*/
    StringBuffer output;

    this->layout->format(output, event);

    String sz = output.str();
    USES_CONVERSION;
    os->write((void *)T2A(sz.c_str()), sz.length());

    // flush to socket
    os->flush();
  }
  catch(SocketException& e)
  {
    os = 0;
    LogLog::warn(_T("Detected problem with connection: "), e);

    if(reconnectionDelay > 0)
    {
      fireConnector();
    }

  }
}

void XMLSocketAppender::fireConnector()
{
  if(connector == 0)
  {
    LogLog::debug(_T("Starting a new connector thread."));
    connector = new Connector(this);
    connector->setPriority(Thread::MIN_PRIORITY);
    connector->start();
  }
}

XMLSocketAppender::Connector::Connector(XMLSocketAppenderPtr socketAppender)
: interrupted(false), socketAppender(socketAppender)
{
}

void XMLSocketAppender::Connector::run()
{
  SocketPtr socket;
  while(!interrupted)
  {
    try
    {
      sleep(socketAppender->reconnectionDelay);
      LogLog::debug(_T("Attempting connection to ")
        +socketAppender->address.getHostName());
      socket = new Socket(socketAppender->address, socketAppender->port);
      
      synchronized sync(this);
      {
        socketAppender->os = socket->getOutputStream();
        socketAppender->connector = 0;
        LogLog::debug(_T("Connection established. Exiting connector thread."));
        break;
      }
    }
    catch(InterruptedException&)
    {
      LogLog::debug(_T("Connector interrupted. Leaving loop."));
      return;
    }
    catch(ConnectException&)
    {
      LogLog::debug(_T("Remote host ")
        +socketAppender->address.getHostName()
        +_T(" refused connection."));
    }
    catch(IOException& e)
    {
      LogLog::debug(_T("Could not connect to ")
         +socketAppender->address.getHostName()
         +_T(". Exception is ") + e.getMessage());
    }
  }
  
  LogLog::debug(_T("Exiting Connector.run() method."));
}
