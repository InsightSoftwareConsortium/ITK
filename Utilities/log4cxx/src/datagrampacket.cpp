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
 
#include <log4cxx/helpers/datagrampacket.h>

using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(DatagramPacket);

/** Constructs a DatagramPacket for receiving packets of length
<code>length</code>. */
DatagramPacket::DatagramPacket(void * buf, int length)
: buf(buf), offset(0), length(length), port(0)
{
}

/** Constructs a datagram packet for sending packets of length
<code>length/<code> to the specified port number on the specified
host. */
DatagramPacket::DatagramPacket(void * buf, int length, InetAddress address,
int port)
: buf(buf), offset(0), length(length), address(address), port(port)
{
}

/** Constructs a DatagramPacket for receiving packets of length
<code>length</code>, specifying an offset into the buffer. */
DatagramPacket::DatagramPacket(void * buf, int offset, int length)
: buf(buf), offset(offset), length(length), port(0)
{
}
/** Constructs a datagram packet for sending packets of length
<code>length</code> with offset <code>offset</code> to the
specified port number on the specified host. */
DatagramPacket::DatagramPacket(void * buf, int offset, int length,
InetAddress address, int port)
: buf(buf), offset(offset), length(length), address(address), port(port)
{
}

DatagramPacket::~DatagramPacket()
{
}
