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

#ifdef WIN32
#include <windows.h>
#endif

#include <log4cxx/logger.h>
#include <log4cxx/helpers/serversocket.h>
#include <log4cxx/helpers/socket.h>
#include <log4cxx/net/socketnode.h>
#include <log4cxx/xml/domconfigurator.h>
#include <log4cxx/propertyconfigurator.h>
#include <log4cxx/helpers/thread.h>
#include <log4cxx/logmanager.h>
#include <log4cxx/level.h>
#include <log4cxx/helpers/stringhelper.h>

using namespace log4cxx;
#ifdef HAVE_XML
using namespace log4cxx::xml;
#endif
using namespace log4cxx::net;
using namespace log4cxx::helpers;

int port = 0;

void usage(const String& msg)
{
  tcout << msg << std::endl;
  tcout << _T("Usage: simpleocketServer port configFile") << std::endl;
}

void init(const String& portStr, const String& configFile)
{
  USES_CONVERSION;
  port = ttol(portStr.c_str());

#ifdef HAVE_XML
  // tests if configFile ends with ".xml"
  if (StringHelper::endsWith(configFile, _T(".xml")))
  {
    DOMConfigurator::configure(configFile);
  } 
  else
#endif
  {
    PropertyConfigurator::configure(configFile);
  }
}

int main(int argc, char * argv[])
{
  if(argc == 3)
  {
    USES_CONVERSION;
    init(A2T(argv[1]), A2T(argv[2]));
  }
  else
  {
    USES_CONVERSION;
    usage(_T("Wrong number of arguments."));
    return 1;
  }

  try
  {
    LoggerPtr logger = Logger::getLogger(_T("SimpleSocketServer"));
    
    LOG4CXX_INFO(logger, _T("Listening on port ") << port);
  
    ServerSocket serverSocket(port);
    while(true)
    {
      LOG4CXX_INFO(logger, _T("Waiting to accept a new client."));
      SocketPtr socket = serverSocket.accept();
      
      LOG4CXX_INFO(logger, _T("Connected to client at ")
        << socket->getInetAddress().toString());
      LOG4CXX_INFO(logger, _T("Starting new socket node."));
      
      Thread * thread = new Thread(new SocketNode(socket,
        LogManager::getLoggerRepository()));
      thread->start();
    }
  }
  catch(SocketException& e)
  {
    tcout << _T("SocketException: ") << e.getMessage() << std::endl;
  }

  return 0;
}

