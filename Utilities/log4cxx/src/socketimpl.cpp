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
#include <sys/time.h>
#include <sys/types.h>
#endif

#include <log4cxx/helpers/socketimpl.h>
#include <log4cxx/helpers/loglog.h>
#include <errno.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(SocketImpl)

#include <string.h>
#include <assert.h>

#ifdef WIN32
namespace {
    class WinSockInitializer {
    public:
        WinSockInitializer() {
            WSAStartup(MAKEWORD(1, 1), &wsa);
        }
        ~WinSockInitializer() {
            WSACleanup();
        }

        WSADATA wsa;
    } winSockInitializer;

}
#endif

SocketException::SocketException()
{
#ifdef WIN32
  TCHAR messageBuffer[256];
  DWORD dwError = ::WSAGetLastError();

  if (dwError != 0)
  {
    
    DWORD dw = ::FormatMessage(
      FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS |
        FORMAT_MESSAGE_MAX_WIDTH_MASK,
      NULL,
      dwError,
      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
      messageBuffer, 
      sizeof(messageBuffer)/sizeof(messageBuffer[0]),
      NULL);

    if (dw != 0)
    {
      message = messageBuffer;

      // on retire les retours chariots en fin de chaîne
      message = message.substr(0, message.find_last_not_of(_T("\n\r")) + 1);
    }
    else
    {
      itot(::WSAGetLastError(), messageBuffer, 10);
      message = messageBuffer;
    }
  }
#else
  USES_CONVERSION;
  message = A2T(strerror(errno));
#endif
};

SocketImpl::SocketImpl() : fd(0), localport(-1), port(0), timeout(-1)
{
}

SocketImpl::~SocketImpl()
{
  try
  {
    close();
  }
  catch(SocketException&)
  {
  }
}

/** Accepts a connection. */
void SocketImpl::accept(SocketImplPtr s)
{
  sockaddr_in client_addr;
#if defined(WIN32) || defined(__hpux)
  int client_len;
#else
  socklen_t client_len;
#endif

  client_len = sizeof(client_addr);

  if (timeout > 0)
  {
    // convert timeout in milliseconds to struct timeval
    timeval tv;
    tv.tv_sec = timeout / 1000;
    tv.tv_usec = (timeout % 1000) * 1000;

    fd_set rfds;
    FD_ZERO(&rfds);
    FD_SET(this->fd, &rfds);

    int retval = ::select(this->fd+1, &rfds, NULL, NULL, &tv);
    if (retval == 0)
    {
      throw SocketTimeoutException();
    }

    assert(FD_ISSET(this->fd, &rfds));
  }

  int fdClient = ::accept(this->fd, (sockaddr *)&client_addr, &client_len);

  if (fdClient < 0)
  {
    throw SocketException();
  }

  s->address.address = ntohl(client_addr.sin_addr.s_addr);
  s->fd = fdClient;
  s->port = ntohs(client_addr.sin_port);
}

/** Returns the number of bytes that can be read from this socket
without blocking.
*/
int SocketImpl::available()
{
  // TODO
  return 0;
}

/** Binds this socket to the specified port number
on the specified host.
*/
void SocketImpl::bind(InetAddress host, int port)
{
  struct sockaddr_in server_addr;
  int server_len = sizeof(server_addr);
  
  server_addr.sin_family = AF_INET;
  server_addr.sin_addr.s_addr = htonl(host.address);
  server_addr.sin_port = htons(port);

  if (::bind(fd, (sockaddr *)&server_addr, server_len) == -1)
  {
    throw BindException();
  }

  this->localport = port;
}

/** Closes this socket. */
void SocketImpl::close()
{
  if (fd != 0)
  {
    LOGLOG_DEBUG(_T("closing socket"));
#ifdef WIN32
    if (::closesocket(fd) == -1)
#else
    if (::close(fd) == -1)
#endif
    {
      throw SocketException();
    }
  
    address.address = 0;
    fd = 0;
    port = 0;
    localport = -1;
  }
}

/**  Connects this socket to the specified port number
on the specified host.
*/
void SocketImpl::connect(InetAddress address, int port)
{
  sockaddr_in client_addr;
  int client_len = sizeof(client_addr);

  client_addr.sin_family = AF_INET;
  client_addr.sin_addr.s_addr = htonl(address.address);
  client_addr.sin_port = htons(port);

  if (::connect(fd, (sockaddr *)&client_addr, client_len) == -1)
  {
    throw ConnectException();
  }

  this->address = address;
  this->port = port;
}

/** Connects this socket to the specified port on the named host. */
void SocketImpl::connect(const String& host, int port)
{
  connect(InetAddress::getByName(host), port);
}

/** Creates either a stream or a datagram socket. */
void SocketImpl::create(bool stream)
{
  if ((fd = ::socket(AF_INET, stream ? SOCK_STREAM : SOCK_DGRAM, 0)) == -1)
  {
    throw SocketException();
  }
}

/** Sets the maximum queue length for incoming connection
indications (a request to connect) to the count argument.
*/
void SocketImpl::listen(int backlog)
{
  if (::listen(fd, backlog) == -1)
  {
    throw SocketException();
  }
}

/** Returns the address and port of this socket as a String.
*/
String SocketImpl::toString() const
{
  StringBuffer oss;
  oss << address.getHostAddress() << _T(":") << port;
  return oss.str();
}

// thanks to Yves Mettier (ymettier@libertysurf.fr) for this routine
size_t SocketImpl::read(void * buf, size_t len) const
{
//  LOGLOG_DEBUG(_T("SocketImpl::reading ") << len << _T(" bytes."));
  int len_read = 0;
  unsigned char * p = (unsigned char *)buf;

  while ((size_t)(p - (unsigned char *)buf) < len)
  {
#ifdef WIN32
    len_read = ::recv(fd, (char *)p, len - (p - (unsigned char *)buf), 0);
#else
    len_read = ::read(fd, p, len - (p - (unsigned char *)buf));
#endif
    if (len_read < 0)
    {
      throw SocketException();
    }
    if (len_read == 0)
    {
      break;
    }
    p += len_read;
  }

  return (p - (const unsigned char *)buf);
}

// thanks to Yves Mettier (ymettier@libertysurf.fr) for this routine
size_t SocketImpl::write(const void * buf, size_t len)
{
//  LOGLOG_DEBUG(_T("SocketImpl::writing ") << len << _T(" bytes."));

  int len_written = 0;
  const unsigned char * p = (const unsigned char *)buf;

  while ((size_t)(p - (const unsigned char *)buf) < len)
  {
#ifdef WIN32
    len_written = ::send(fd, (const char *)p, len - (p - (const unsigned char *)buf), 0);
#else
    len_written = ::write(fd, p, len - (p - (const unsigned char *)buf));
#endif
    if (len_written < 0)
    {
      throw SocketException();
    }
    if (len_written == 0)
    {
      break;
    }
    p += len_written;
  }

  return (p - (const unsigned char *)buf);
}

/** Retrive setting for SO_TIMEOUT.
*/
int SocketImpl::getSoTimeout() const
{
  return timeout;
}

/** Enable/disable SO_TIMEOUT with the specified timeout, in milliseconds.
*/
void SocketImpl::setSoTimeout(int timeout)
{
  this->timeout = timeout;
}
