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
 
#include <log4cxx/helpers/syslogwriter.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/inetaddress.h>
#include <log4cxx/helpers/datagramsocket.h>
#include <log4cxx/helpers/datagrampacket.h>
#include <log4cxx/helpers/socketimpl.h>

#define SYSLOG_PORT 514

using namespace log4cxx;
using namespace log4cxx::helpers;

SyslogWriter::SyslogWriter(const String& syslogHost)
: syslogHost(syslogHost)
{
  try
  {
    this->address = InetAddress::getByName(syslogHost);
  }
  catch(UnknownHostException& e)
  {
    LogLog::error(_T("Could not find ") + syslogHost +
      _T(". All logging will FAIL."), e);
  }

  try
  {
    this->ds = new DatagramSocket();
  }
  catch (SocketException& e)
  {
    LogLog::error(_T("Could not instantiate DatagramSocket to ") + syslogHost +
        _T(". All logging will FAIL."), e);
  }
}

void SyslogWriter::write(const String& string)
{
  USES_CONVERSION;
  const char * bytes = T2A(string.c_str());
  DatagramPacketPtr packet = new DatagramPacket((void *)bytes, string.length() + 1,
            address, SYSLOG_PORT);

  if(this->ds != 0)
  {
    ds->send(packet);
  }

}
