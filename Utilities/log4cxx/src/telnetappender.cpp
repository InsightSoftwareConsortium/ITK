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
 
#include <log4cxx/net/telnetappender.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/socketoutputstream.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/helpers/stringhelper.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::net;

IMPLEMENT_LOG4CXX_OBJECT(TelnetAppender)

int TelnetAppender::DEFAULT_PORT = 23;


TelnetAppender::TelnetAppender() : port(23), sh(0)
{
}

TelnetAppender::~TelnetAppender()
{
  finalize();
}

void TelnetAppender::activateOptions() 
{
  try 
  {
    sh = new SocketHandler(port);
    sh->start();
  }
  catch(Exception& e)
  {
    LogLog::error(_T("Caught exception"), e);
  }
}

void TelnetAppender::setOption(const String& option,
  const String& value)
{
  if (StringHelper::equalsIgnoreCase(option, _T("port")))
  {
    setPort(OptionConverter::toInt(value, DEFAULT_PORT));
  }
  else
  {
    AppenderSkeleton::setOption(name, value);
  }
}

void TelnetAppender::close() 
{
  if (sh != 0)
  {
    sh->finalize();
  }
}

void TelnetAppender::append(const spi::LoggingEventPtr& event) 
{
  if (sh != 0)
  {
    StringBuffer os;

    this->layout->format(os, event);

    sh->send(os.str());
  }
}

TelnetAppender::SocketHandler::SocketHandler(int port)
: done(false), MAX_CONNECTIONS(20), serverSocket(port)
{
}

void TelnetAppender::SocketHandler::finalize()
{
  std::vector<helpers::SocketPtr>::iterator it, itEnd = connections.end();

  for (it = connections.begin(); it != itEnd; it++)
  {
    try 
    {
      (*it)->close();
    }
    catch(Exception&)
    {
    }
  }

  try
  {
    serverSocket.close();
  } 
  catch(Exception&)
  {
  }
  done = true;
}

void TelnetAppender::SocketHandler::send(const String& message)
{
  std::vector<helpers::SocketOutputStreamPtr>::iterator it, itEnd;
  std::vector<helpers::SocketPtr>::iterator itc, itEndc;

  it = writers.begin();
  itEnd = writers.end();
  itc = connections.begin();
  itEndc = connections.end();

  bool bRemove = false;

  while (it != itEnd/*| itc != itEndc*/)
  {
    SocketPtr& sock = *itc;
    SocketOutputStreamPtr& writer = *it;

    try
    {
      print(writer, message);
      print(writer, _T("\r\n"));
      writer->flush();
      itc++;
      it++;
    }
    catch(Exception&)
    {
      // The client has closed the connection, remove it from our list:
      itc = connections.erase(itc);
      it = writers.erase(it);
      itEnd = writers.end();
      itEndc = connections.end();
    }
  }
}

void TelnetAppender::SocketHandler::run()
{
  while(!done)
  {
    try 
    {
      SocketPtr newClient = serverSocket.accept();
      SocketOutputStreamPtr os = newClient->getOutputStream();
      if(connections.size() < MAX_CONNECTIONS)
      {
        connections.push_back(newClient);
        writers.push_back(os);

        StringBuffer oss;
        oss << _T("TelnetAppender v1.0 (") << connections.size()
          << _T(" active connections)\r\n\r\n");
        print(os, oss.str());
        os->flush();
      } 
      else
      {
        print(os, _T("Too many connections.\r\n"));
        os->flush();
        newClient->close();
      }
    } 
    catch(Exception& e) 
    {
      LogLog::error(_T("Encountered error while in SocketHandler loop."), e);
    }
  }
}

void TelnetAppender::SocketHandler::print(helpers::SocketOutputStreamPtr& os, const String& sz)
{
  USES_CONVERSION;
  os->write((void *)T2A(sz.c_str()), sz.length());
}




