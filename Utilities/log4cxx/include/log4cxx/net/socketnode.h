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
 
#ifndef _LOG4CXX_NET_SOCKET_NODE_H
#define _LOG4CXX_NET_SOCKET_NODE_H

#include <log4cxx/helpers/thread.h>
#include <log4cxx/helpers/objectptr.h>
#include <log4cxx/helpers/objectimpl.h>

namespace log4cxx
{
  namespace helpers
  {
    class Socket;
    typedef ObjectPtrT<Socket> SocketPtr;

    class SocketInputStream;
    typedef ObjectPtrT<SocketInputStream> SocketInputStreamPtr;
  } 
  
  namespace spi
  {
    class LoggerRepository;
    typedef helpers::ObjectPtrT<LoggerRepository> LoggerRepositoryPtr;
  } 

  namespace net
  {
    class SocketNode;
    typedef helpers::ObjectPtrT<SocketNode> SocketNodePtr;
        
    /**
        Read {@link spi::LoggingEvent LoggingEvent} objects sent from a remote
    client using Sockets (TCP). These logging events are logged according
    to local policy, as if they were generated locally.

        <p>For example, the socket node might decide to log events to a
        local file and also resent them to a second socket node.
        */
        class LOG4CXX_EXPORT SocketNode :
      public virtual helpers::Runnable,
        public virtual helpers::ObjectImpl
    {
    protected:
      helpers::SocketInputStreamPtr is;
      spi::LoggerRepositoryPtr hierarchy;

    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(SocketNode)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(SocketNode)
        LOG4CXX_CAST_ENTRY(helpers::Runnable)
      END_LOG4CXX_CAST_MAP()

      SocketNode(helpers::SocketPtr& socket, 
        spi::LoggerRepositoryPtr& hierarchy);
      virtual void run();
    };
  }  // namespace net
}; // namespace log4cxx

#endif // _LOG4CXX_NET_SOCKET_NODE_H
