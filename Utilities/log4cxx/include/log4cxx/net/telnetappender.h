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
 
#ifndef _LOG4CXX_NET_TELNET_APPENDER_H
#define _LOG4CXX_NET_TELNET_APPENDER_H

#include <log4cxx/appenderskeleton.h>
#include <log4cxx/helpers/socket.h>
#include <log4cxx/helpers/serversocket.h>
#include <log4cxx/helpers/thread.h>
#include <vector>

namespace log4cxx
{
  namespace helpers
  {
    class SocketOutputStream;
    typedef helpers::ObjectPtrT<SocketOutputStream> SocketOutputStreamPtr;
  } 
  
  namespace net
  {
/**
<p>The TelnetAppender is a log4cxx appender that specializes in
writing to a read-only socket.  The output is provided in a
telnet-friendly way so that a log can be monitored over TCP/IP.
Clients using telnet connect to the socket and receive log data.
This is handy for remote monitoring, especially when monitoring a
servlet.

<p>Here is a list of the available configuration options:

<table border=1>
<tr>
<td align=center><b>Name</b></td>
<td align=center><b>Requirement</b></td>
<td align=center><b>Description</b></td>
<td align=center><b>Sample Value</b></td>
</tr>

<tr>
<td>Port</td>
<td>optional</td>
<td>This parameter determines the port to use for announcing log events.  The default port is 23 (telnet).</td>
<td>5875</td>
</table>
*/
        class LOG4CXX_EXPORT TelnetAppender : public AppenderSkeleton
    {
    class SocketHandler;
    friend class SocketHandler;
    private:
      static int DEFAULT_PORT;
      SocketHandler * sh;
      int port;

    public:      
      DECLARE_LOG4CXX_OBJECT(TelnetAppender)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(TelnetAppender)
        LOG4CXX_CAST_ENTRY_CHAIN(AppenderSkeleton)
      END_LOG4CXX_CAST_MAP()

      TelnetAppender();
      ~TelnetAppender();

      /** 
      This appender requires a layout to format the text to the
      attached client(s). */
      virtual bool requiresLayout() const
        { return true; }
      
      /** all of the options have been set, create the socket handler and
      wait for connections. */
      void activateOptions();

        /**
        Set options
        */
      virtual void setOption(const String& option, const String& value);

        /**
        Returns value of the <b>Port</b> option.
        */
      int getPort() const
        { return port; }
      
        /**
        The <b>Port</b> option takes a positive integer representing
        the port where the server is waiting for connections.
        */
      void setPort(int port)
      { this->port = port; }
      
      
      /** shuts down the appender. */
      void close();

    protected:
      /** Handles a log event.  For this appender, that means writing the
      message to each connected client.  */
      virtual void append(const spi::LoggingEventPtr& event) ;
      
      //---------------------------------------------------------- SocketHandler:
      
    private:
      /** The SocketHandler class is used to accept connections from
      clients.  It is threaded so that clients can connect/disconnect
      asynchronously. */
      class LOG4CXX_EXPORT SocketHandler : public helpers::Thread
      {
      private:
        bool done;
        std::vector<helpers::SocketOutputStreamPtr> writers;
        std::vector<helpers::SocketPtr> connections;
        helpers::ServerSocket serverSocket;
        std::vector<helpers::SocketPtr>::size_type MAX_CONNECTIONS;

      public:
        SocketHandler(int port);
        
        /** make sure we close all network connections when this handler
        is destroyed. */
        void finalize();
        
        /** sends a message to each of the clients in telnet-friendly output. */
        void send(const String& message);
        
        /** 
        Continually accepts client connections.  Client connections
        are refused when MAX_CONNECTIONS is reached. 
        */
        virtual void run();

      protected:
        void print(helpers::SocketOutputStreamPtr& os, const String& sz);
      }; // class SocketHandler
    }; // class TelnetAppender
    } // namespace net
}; // namespace log4cxx

#endif // _LOG4CXX_NET_TELNET_APPENDER_H

