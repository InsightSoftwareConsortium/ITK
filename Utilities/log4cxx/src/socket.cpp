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
#include <winsock.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netdb.h>
#endif

#include <log4cxx/helpers/socket.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/socketoutputstream.h>
#include <log4cxx/helpers/socketinputstream.h>

#include <string.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(Socket)

/** Creates an unconnected socket.
*/
Socket::Socket()
{
}

/** Creates a stream socket and connects it to the specified port
number at the specified IP address.
*/
Socket::Socket(InetAddress address, int port)
{
  socketImpl = new SocketImpl();
  socketImpl->create(true);
  socketImpl->connect(address, port);
}

/** Creates a socket and connects it to the specified remote
address on the specified remote port.
*/
Socket::Socket(InetAddress address, int port,
  InetAddress localAddr, int localPort)
{
  socketImpl = new SocketImpl();
  socketImpl->create(true);
  socketImpl->connect(address, port);
  socketImpl->bind(localAddr, localPort);
}

/** Creates an unconnected Socket
with a user-specified SocketImpl.
*/
Socket::Socket(SocketImplPtr impl) : socketImpl(impl)
{
}


/** Creates a stream socket and connects it to the specified
port number on the named host.
*/
Socket::Socket(const String& host, int port)
{
  socketImpl = new SocketImpl();
  socketImpl->create(true);
  socketImpl->connect(host, port);
}

/**  Creates a socket and connects it to the specified remote
host on the specified remote port.
*/
Socket::Socket(const String& host, int port,
  InetAddress localAddr, int localPort)
{
  socketImpl = new SocketImpl();
  socketImpl->create(true);
  socketImpl->connect(host, port);
  socketImpl->bind(localAddr, localPort);
}

/**  Returns an output stream for this socket. */
SocketOutputStreamPtr Socket::getOutputStream()
{
  return new SocketOutputStream(this);
}

/**  Returns an input stream for this socket. */
SocketInputStreamPtr Socket::getInputStream()
{
  return new SocketInputStream(this);
}


