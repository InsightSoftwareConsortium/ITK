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
 
#ifndef _LOG4CXX_HELPERS_SOCKET_IMPL
#define _LOG4CXX_HELPERS_SOCKET_IMPL

#include <log4cxx/helpers/objectimpl.h>
#include <log4cxx/helpers/objectptr.h>
#include <log4cxx/helpers/inetaddress.h>
#include <log4cxx/helpers/exception.h>

namespace log4cxx
{
  namespace helpers
  {
    /** Thrown to indicate that there is an error in the underlying
    protocol, such as a TCP error.
    */
    class LOG4CXX_EXPORT SocketException : public IOException
    {
    public:
      SocketException();
    };

    /** Signals that an error occurred while attempting to connect a socket
    to a remote address and port. Typically, the connection was refused
    remotely (e.g., no process is listening on the remote address/port).
    */
    class LOG4CXX_EXPORT ConnectException : public SocketException
    {
    };

    /** Signals that an error occurred while attempting to bind a socket to
    a local address and port. Typically, the port is in use, or the
    requested local address could not be assigned.
    */
    class LOG4CXX_EXPORT BindException : public SocketException
    {
    };

    /** Signals that an I/O operation has been interrupted. An
    InterruptedIOException is thrown to indicate that an input or output
    transfer has been terminated because the thread performing it was
    interrupted. The field bytesTransferred  indicates how many bytes were
    successfully transferred before the interruption occurred.
    */
    class LOG4CXX_EXPORT InterruptedIOException : public IOException
    {
    };

    /** Signals that a timeout has occurred on a socket read or accept.
    */
    class LOG4CXX_EXPORT SocketTimeoutException : public InterruptedIOException
    {
    };

    class SocketImpl;
    typedef helpers::ObjectPtrT<SocketImpl> SocketImplPtr;

    /** @brief Default Socket Implementation.

    This implementation does not implement any security check.
    */
    class LOG4CXX_EXPORT SocketImpl : public helpers::ObjectImpl
    {
    protected:
      /** The IP address of the remote end of this socket. */
      InetAddress address;

      /** The file descriptor object for this socket. */
      int fd;

      /** The local port number to which this socket is connected. */
      int localport;

      /** The port number on the remote host to which
      this socket is connected. */
      int port;

      int timeout;

    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(SocketImpl)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(SocketImpl)
      END_LOG4CXX_CAST_MAP()

      SocketImpl();
      ~SocketImpl();
      
      /** @brief Accepts a connection.
      @param s the connection
      @throw SocketTimeoutException if a timeout was previously set with
      setSoTimeout and the timeout has been reached.
      @throw SocketException if an I/O error occurs when accepting the
      connection
      */
      void accept(SocketImplPtr s);

      /** @brief Returns the number of bytes that can be read from this socket
      without blocking.
      @return the number of bytes that can be read from this socket
      without blocking.
      */
      int available();

      /** Binds this socket to the specified port number
      on the specified host.
      @param host the host address
        @param port the port number.
      @exception BindException if an I/O error occurs when binding this socket.
      */
      void bind(InetAddress host, int port);

      /** Closes this socket. */
      void close();

      /**  Connects this socket to the specified port number
      on the specified host.
      */
       void connect(InetAddress address, int port);

      /** Connects this socket to the specified port on the named host. */
      void connect(const String& host, int port);

      /** Creates either a stream or a datagram socket. */
      void create(bool stream);

      /** Returns the value of this socket's fd field. */
      inline int getFileDescriptor() const
        { return fd; }

      /** Returns the value of this socket's address field. */
      inline InetAddress getInetAddress() const
        { return address; }

      /** Returns the value of this socket's localport field. */
      inline int getLocalPort() const
        { return localport; }

      /** Returns the value of this socket's port field. */
      inline int getPort() const
        { return port; }

      /** Sets the maximum queue length for incoming connection
      indications (a request to connect) to the count argument.
      */
      void listen(int backlog);

      /** Returns the address and port of this socket as a String.
      */
      String toString() const;

      size_t read(void * buf, size_t len) const;
      size_t write(const void * buf, size_t len);

      /** Retrive setting for SO_TIMEOUT.
      */
      int getSoTimeout() const;

      /** Enable/disable SO_TIMEOUT with the specified timeout, in milliseconds.
      */
      void setSoTimeout(int timeout);
    };
  }  // namespace helpers
}; // namespace log4cxx

#endif // _LOG4CXX_HELPERS_SOCKET_IMPL
