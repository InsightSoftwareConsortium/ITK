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
 
#include <log4cxx/net/sockethubappender.h>

#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/socketoutputstream.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/helpers/serversocket.h>
#include <log4cxx/spi/loggingevent.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::net;
using namespace log4cxx::spi;

IMPLEMENT_LOG4CXX_OBJECT(SocketHubAppender)

int SocketHubAppender::DEFAULT_PORT = 4560;

SocketHubAppender::~SocketHubAppender()
{
  finalize();
}

SocketHubAppender::SocketHubAppender()
 : port(DEFAULT_PORT), locationInfo(false)
{
}

SocketHubAppender::SocketHubAppender(int port)
 : port(port), locationInfo(false)
{
  startServer();
}

void SocketHubAppender::activateOptions()
{
  startServer();
}

void SocketHubAppender::setOption(const String& option,
  const String& value)
{
  if (StringHelper::equalsIgnoreCase(option, _T("port")))
  {
    setPort(OptionConverter::toInt(value, DEFAULT_PORT));
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("locationinfo")))
  {
    setLocationInfo(OptionConverter::toBoolean(value, true));
  }
  else
  {
    AppenderSkeleton::setOption(name, value);
  }
}


void SocketHubAppender::close()
{
  synchronized sync(this);

  if(closed)
  {
    return;
  }
  
  LOGLOG_DEBUG(_T("closing SocketHubAppender ") << getName());
  closed = true;
  cleanUp();
  LOGLOG_DEBUG(_T("SocketHubAppender ") << getName() << _T(" closed"));
}

void SocketHubAppender::cleanUp()
{
  // stop the monitor thread
  LOGLOG_DEBUG(_T("stopping ServerSocket"));
  serverMonitor->stopMonitor();
  serverMonitor = 0;
  
  // close all of the connections
  LOGLOG_DEBUG(_T("closing client connections"));
  while (!oosList.empty())
  {
    SocketOutputStreamPtr oos = oosList.at(0);
    if(oos != 0)
    {
      try
      {
        oos->close();
      }
      catch(SocketException& e)
      {
        LogLog::error(_T("could not close oos: "), e);
      }
      
      oosList.erase(oosList.begin());     
    }
  }
}

void SocketHubAppender::append(const spi::LoggingEventPtr& event)
{

  // if no open connections, exit now
  if(oosList.empty())
  {
    return;
  }
  
/*  // set up location info if requested
  if (locationInfo)
  {
    event.getLocationInformation();  
  } */
  
  // loop through the current set of open connections, appending the event to each
  std::vector<SocketOutputStreamPtr>::iterator it = oosList.begin();
  std::vector<SocketOutputStreamPtr>::iterator itEnd = oosList.end();
  while(it != itEnd)
  {   
    SocketOutputStreamPtr oos = *it;
    
    // list size changed unexpectedly? Just exit the append.
    if (oos == 0)
    {
      break;
    }
    
    try 
    {
      event->write(oos);
      oos->flush();
      it++;
    }
    catch(SocketException&)
    {
      // there was an io exception so just drop the connection
      it = oosList.erase(it);
      LOGLOG_DEBUG(_T("dropped connection"));
    }
  }
}

void SocketHubAppender::startServer()
{
  serverMonitor = new ServerMonitor(port, oosList);
}

SocketHubAppender::ServerMonitor::ServerMonitor(int port, const std::vector<helpers::SocketOutputStreamPtr>& oosList)
: port(port), oosList(oosList), keepRunning(true)
{
  monitorThread = new Thread(this);
  monitorThread->start();
}

void SocketHubAppender::ServerMonitor::stopMonitor()
{
  synchronized sync(this);

  if (keepRunning)
  {  
    LogLog::debug(_T("server monitor thread shutting down"));
    keepRunning = false;
    try
    {  
      monitorThread->join();
    }
    catch (InterruptedException&)
    {
      // do nothing?
    }
    
    // release the thread
    monitorThread = 0;
    LogLog::debug(_T("server monitor thread shut down"));
  }
}

void SocketHubAppender::ServerMonitor::run()
{
  ServerSocket * serverSocket = 0;

  try
  {
    serverSocket = new ServerSocket(port);
    serverSocket->setSoTimeout(1000);
  }
  catch (SocketException& e)
  {
    LogLog::error(_T("exception setting timeout, shutting down server socket."), e);
    keepRunning = false;
    return;
  }
  
  try
  {
    serverSocket->setSoTimeout(1000);
  }
  catch (SocketException& e)
  {
    LogLog::error(_T("exception setting timeout, shutting down server socket."), e);
    return;
  }
  
  while (keepRunning)
  {
    SocketPtr socket;
    try
    {
      socket = serverSocket->accept();
    }
    catch (InterruptedIOException&)
    {
      // timeout occurred, so just loop
    }
    catch (SocketException& e)
    {
      LogLog::error(_T("exception accepting socket, shutting down server socket."), e);
      keepRunning = false;
    }
    catch (IOException& e)
    {
      LogLog::error(_T("exception accepting socket."), e);
    }
    
    // if there was a socket accepted
    if (socket != 0)
    {
      try
      {
        InetAddress remoteAddress = socket->getInetAddress();
        LOGLOG_DEBUG(_T("accepting connection from ") << remoteAddress.getHostName() 
          << _T(" (") + remoteAddress.getHostAddress() + _T(")"));
        
        // create an ObjectOutputStream
        SocketOutputStreamPtr oos = socket->getOutputStream();
        
        // add it to the oosList.  OK since Vector is synchronized.
        oosList.push_back(oos);
      }
      catch (IOException& e)
      {
        LogLog::error(_T("exception creating output stream on socket."), e);
      }
    }
  }

  delete serverSocket;
}

