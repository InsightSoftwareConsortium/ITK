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
 
#ifndef _LOG4CXX_HELPER_INETADDRESS_H
#define _LOG4CXX_HELPER_INETADDRESS_H

#include <log4cxx/helpers/tchar.h>
#include <vector>
#include <log4cxx/helpers/exception.h>

namespace log4cxx
{
  namespace helpers
  {
    class LOG4CXX_EXPORT UnknownHostException : public Exception
    {
    };

    class LOG4CXX_EXPORT InetAddress
    {
    public:
      InetAddress();
      
      /** Returns the raw IP address of this InetAddress  object.
      */
      int getAddress() const;

      /** Determines all the IP addresses of a host, given the host's name.
      */
      static std::vector<InetAddress> getAllByName(const String& host);

      /** Determines the IP address of a host, given the host's name.
      */
      static InetAddress getByName(const String& host);

      /** Returns the IP address string "%d.%d.%d.%d".
      */
      String getHostAddress() const;

      /** Gets the host name for this IP address.
      */
      String getHostName() const;

      /** Returns the local host.
      */
      static InetAddress  getLocalHost();

      /** Utility routine to check if the InetAddress is an IP multicast address.
      */
      bool isMulticastAddress() const;

      /** Converts this IP address to a String.
      */
      String toString() const; 

      int address;
    }; // class InetAddress
  }  // namespace helpers
}; // namespace log4cxx

#endif // _LOG4CXX_HELPER_INETADDRESS_H
