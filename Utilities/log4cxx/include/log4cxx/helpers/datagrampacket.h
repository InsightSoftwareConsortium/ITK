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
 
#ifndef _LOG4CXX_HELPERS_DATAGRAM_PACKET
#define _LOG4CXX_HELPERS_DATAGRAM_PACKET

#include <log4cxx/helpers/objectimpl.h>
#include <log4cxx/helpers/objectptr.h>
#include <log4cxx/helpers/inetaddress.h>

namespace log4cxx
{
  namespace helpers
  {
    class DatagramPacket;
    typedef helpers::ObjectPtrT<DatagramPacket> DatagramPacketPtr;

    /** This class represents a datagram packet.
    <p>Datagram packets are used to implement a connectionless packet
    delivery service. Each message is routed from one machine to another
    based solely on information contained within that packet. Multiple
    packets sent from one machine to another might be routed differently,
    and might arrive in any order.
    */
    class LOG4CXX_EXPORT DatagramPacket : public helpers::ObjectImpl
    {
    protected:
      /** the data for this packet. */
      void * buf;

      /** The offset of the data for this packet. */
      int offset;

      /** The length of the data for this packet. */
      int length;

      /** The IP address for this packet. */
      InetAddress address;

      /** The UDP port number of the remote host. */
      int port;

    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(DatagramPacket)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(DatagramPacket)
      END_LOG4CXX_CAST_MAP()

      /** Constructs a DatagramPacket for receiving packets of length
      <code>length</code>. */
      DatagramPacket(void * buf, int length);

      /** Constructs a datagram packet for sending packets of length
      <code>length</code> to the specified port number on the specified
      host. */
      DatagramPacket(void * buf, int length, InetAddress address, int port);

      /** Constructs a DatagramPacket for receiving packets of length
      <code>length</code>, specifying an offset into the buffer. */
      DatagramPacket(void * buf, int offset, int length);

      /** Constructs a datagram packet for sending packets of length
      <code>length</code> with offset <code>offset</code> to the
      specified port number on the specified host. */
      DatagramPacket(void * buf, int offset, int length, InetAddress address,
        int port);

      ~DatagramPacket();

      /** Returns the IP address of the machine to which this datagram
      is being sent or from which the datagram was received. */
      inline InetAddress getAddress() const
        { return address; }

      /** Returns the data received or the data to be sent. */
      inline void * getData() const
        { return buf; }

      /** Returns the length of the data to be sent or the length of the
      data received. */
      inline int getLength() const
        { return length; }

      /** Returns the offset of the data to be sent or the offset of the
      data received. */
      inline int getOffset() const
        { return offset; }

      /** Returns the port number on the remote host to which this
       datagram is being sent or from which the datagram was received. */
      inline int getPort() const
        { return port; }

      inline void setAddress(InetAddress address)
        { this->address = address; }

      /** Set the data buffer for this packet. */
      inline void setData(void * buf)
        { this->buf = buf; }

      /** Set the data buffer for this packet. */
      inline void setData(void * buf, int offset, int length)
        { this->buf = buf; this->offset = offset; this->length = length; }

      /** Set the length for this packet. */
      inline void setLength(int length)
        { this->length = length; }

      inline void setPort(int port)
        { this->port = port; }

    }; // class DatagramSocketImpl
  }  // namespace helpers
}; // namespace log4cxx

#endif // _LOG4CXX_HELPERS_DATAGRAM_SOCKET_IMPL
