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
#include <string.h>
#endif
 
#include <log4cxx/helpers/inetaddress.h>
#include <log4cxx/helpers/loglog.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

InetAddress::InetAddress() : address(0)
{
}
 
/** Returns the raw IP address of this InetAddress  object.
*/
int InetAddress::getAddress() const
{
  return address;
}

/** Determines all the IP addresses of a host, given the host's name.
*/
std::vector<InetAddress> InetAddress::getAllByName(const String& host)
{
  struct hostent * hostinfo;

  USES_CONVERSION;
  hostinfo = ::gethostbyname(T2A(host.c_str()));

  if (hostinfo == 0)
  {
    LogLog::error(_T("Cannot get information about host :") + host);
    return std::vector<InetAddress>();
  }
  else
  {
    std::vector<InetAddress> addresses;
    InetAddress address;
    char ** addrs = hostinfo->h_addr_list;

    while(*addrs != 0)
    {
      address.address = ntohl(((in_addr *)*addrs)->s_addr);
      addresses.push_back(address);
    }
    
    return addresses;
  }

}

/** Determines the IP address of a host, given the host's name.
*/
InetAddress InetAddress::getByName(const String& host)
{
  struct hostent * hostinfo;
  InetAddress address;

  USES_CONVERSION;
  hostinfo = ::gethostbyname(T2A(host.c_str()));

  if (hostinfo == 0)
  {
    LogLog::error(_T("Cannot get information about host: ") + host);
    throw UnknownHostException();
  }
  else
  {
    address.address = ntohl(((in_addr *)*hostinfo->h_addr_list)->s_addr);
  }

  return address;
}

/** Returns the IP address string "%d.%d.%d.%d".
*/
String InetAddress::getHostAddress() const
{
  USES_CONVERSION;
  in_addr addr;
  addr.s_addr = htonl(address);
  return A2T(::inet_ntoa(addr));
}

/** Gets the host name for this IP address.
*/
String InetAddress::getHostName() const
{
  String hostName;
  struct hostent * hostinfo;

  in_addr addr;
  addr.s_addr = htonl(address);
  hostinfo = ::gethostbyaddr((const char *)&addr, sizeof(addr), AF_INET);

  if (hostinfo != 0)
  {
    USES_CONVERSION;
    hostName = A2T(hostinfo->h_name);
  }
  else
  {
    StringBuffer oss;
    oss << _T("Cannot get host name: ") << address;
    LogLog::error(oss.str());
  }

  return hostName;
}

/** Returns the local host.
*/
InetAddress InetAddress::getLocalHost()
{
  InetAddress address;
  address.address = ntohl(inet_addr("127.0.0.1"));
  return address;
}

/** Utility routine to check if the InetAddress is an IP multicast address.
IP multicast address is a Class D address
i.e first four bits of the address are 1110.
*/
bool InetAddress::isMulticastAddress() const
{
  return (address & 0xF000) == 0xE000;
}

/** Converts this IP address to a String.
*/
String InetAddress::toString() const
{
  return getHostName() + _T("/") + getHostAddress();
}
