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
 
#ifndef _LOG4CXX_HELPERS_SERVER_SOCKET_H
#define _LOG4CXX_HELPERS_SERVER_SOCKET_H

#include <log4cxx/helpers/socket.h>
#include <log4cxx/helpers/exception.h>

namespace log4cxx
{
  namespace helpers
  {
    class LOG4CXX_EXPORT ServerSocket
    {
    public:
      /**  Creates a server socket on a specified port.
      */
      ServerSocket(int port);

      /** Creates a server socket and binds it to the specified local
      port number, with the specified backlog.
      */
      ServerSocket(int port, int backlog);

      /** Create a server with the specified port, listen backlog,

      and local IP address to bind to.
      */
      ServerSocket(int port, int backlog, InetAddress bindAddr);

      ~ServerSocket();

      /** Listens for a connection to be made to this socket and
      accepts it
      */
      SocketPtr accept();

      /** Closes this socket.
      */
      inline void close()
        { socketImpl->close(); }

      /** Returns the local address of this server socket.
      */
      inline InetAddress getInetAddress() const
        { return socketImpl->getInetAddress(); }

      /** Returns the port on which this socket is listening.
      */
      inline int getLocalPort() const
        { return socketImpl->getLocalPort(); }

      /** Returns the implementation address and implementation
      port of this socket as a String
      */
      inline String toString() const
        { return socketImpl->toString(); }

      /** Retrive setting for SO_TIMEOUT.
      */
      int getSoTimeout() const;

      /** Enable/disable SO_TIMEOUT with the specified timeout, in milliseconds.
      */
      void setSoTimeout(int timeout);

    protected:
      SocketImplPtr socketImpl;
    };
  }  // namespace helpers
}; // namespace log4cxx

#endif //_LOG4CXX_HELPERS_SERVER_SOCKET_H
