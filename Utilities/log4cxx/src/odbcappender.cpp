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
#endif

#include <log4cxx/db/odbcappender.h>

#ifdef HAVE_ODBC

#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/patternlayout.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::db;
using namespace log4cxx::spi;

IMPLEMENT_LOG4CXX_OBJECT(ODBCAppender)

ODBCAppender::ODBCAppender()
: connection(SQL_NULL_HDBC), env(SQL_NULL_HENV), bufferSize(1)
{
}

ODBCAppender::~ODBCAppender()
{
  finalize();
}

void ODBCAppender::setOption(const String& option,
  const String& value)
{
  if (StringHelper::equalsIgnoreCase(option, _T("buffersize")))
  {
    setBufferSize((size_t)OptionConverter::toInt(value, 1));
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("password")))
  {
    setPassword(value);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("sql")))
  {
    setSql(value);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("url"))
    || StringHelper::equalsIgnoreCase(option, _T("dns")))
  {
    setURL(value);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("user")))
  {
    setUser(value);
  }
  else
  {
    AppenderSkeleton::setOption(name, value);
  }
}

void ODBCAppender::append(const spi::LoggingEventPtr& event)
{
  buffer.push_back(event);
  
  if (buffer.size() >= bufferSize)
    flushBuffer();
}

String ODBCAppender::getLogStatement(const spi::LoggingEventPtr& event) const
{
  StringBuffer sbuf;
  getLayout()->format(sbuf, event);
  return sbuf.str();
}

void ODBCAppender::execute(const String& sql)
{
  SQLRETURN ret;
  SQLHDBC con = SQL_NULL_HDBC;
  SQLHSTMT stmt = SQL_NULL_HSTMT;

  try
  {
    con = getConnection();
    
    ret = SQLAllocHandle(SQL_HANDLE_STMT, con, &stmt);
    if (ret < 0)
    {
      throw SQLException(ret);
    }

#if defined(HAVE_MS_ODBC)
    ret = SQLExecDirect(stmt, (SQLTCHAR *)sql.c_str(), SQL_NTS);
#else
    USES_CONVERSION;
    ret = SQLExecDirect(stmt, (SQLCHAR *)T2A(sql.c_str()), SQL_NTS);
#endif
    if (ret < 0)
    {
      throw SQLException(ret);
    }
  } 
  catch (SQLException& e)
  {
    if (stmt != SQL_NULL_HSTMT)
    {
      SQLFreeHandle(SQL_HANDLE_STMT, stmt);
    }

    throw e;
  }
  SQLFreeHandle(SQL_HANDLE_STMT, stmt);
  closeConnection(con);
  
  //tcout << _T("Execute: ") << sql << std::endl;
}

/* The default behavior holds a single connection open until the appender
is closed (typically when garbage collected).*/
void ODBCAppender::closeConnection(SQLHDBC con)
{
}

SQLHDBC ODBCAppender::getConnection()
{
  SQLRETURN ret;

  if (env == SQL_NULL_HENV)
  {
    ret = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env);
    if (ret < 0)
    {
      env = SQL_NULL_HENV;
      throw SQLException(ret);
    }
    
    ret = SQLSetEnvAttr(env, SQL_ATTR_ODBC_VERSION, (SQLPOINTER) SQL_OV_ODBC3, SQL_IS_INTEGER);
    if (ret < 0)
    {
      SQLFreeHandle(SQL_HANDLE_ENV, env);
      env = SQL_NULL_HENV;
      throw SQLException(ret);
    }
  }
  
  if (connection == SQL_NULL_HDBC)
  {
    ret = SQLAllocHandle(SQL_HANDLE_DBC, env, &connection);
    if (ret < 0)
    {
      connection = SQL_NULL_HDBC;
      throw SQLException(ret);
    }


#if defined(HAVE_MS_ODBC)
    ret = SQLConnect(connection,
      (SQLTCHAR *)databaseURL.c_str(), SQL_NTS,
      (SQLTCHAR *)databaseUser.c_str(), SQL_NTS,
      (SQLTCHAR *)databasePassword.c_str(), SQL_NTS);
#else
    USES_CONVERSION;
    std::string URL = T2A(databaseURL.c_str());
    std::string user = T2A(databaseUser.c_str());
    std::string password = T2A(databasePassword.c_str());
    ret = SQLConnect(connection,
      (SQLCHAR *)URL.c_str(), SQL_NTS,
      (SQLCHAR *)user.c_str(), SQL_NTS,
      (SQLCHAR *)password.c_str(), SQL_NTS);
#endif
    if (ret < 0)
    {
      SQLFreeHandle(SQL_HANDLE_DBC, connection);
      connection = SQL_NULL_HDBC;
      throw SQLException(ret);
    }
  }
  
  return connection;
}

void ODBCAppender::close()
{
  try
  {
    flushBuffer();
  } 
  catch (SQLException& e)
  {
    errorHandler->error(_T("Error closing connection"), 
      e, ErrorCode::GENERIC_FAILURE);
  }

  if (connection != SQL_NULL_HDBC)
  {
    SQLDisconnect(connection);
    SQLFreeHandle(SQL_HANDLE_DBC, connection);
  }
  
  if (env != SQL_NULL_HENV)
  {
    SQLFreeHandle(SQL_HANDLE_ENV, env);
  }
  
  this->closed = true;
}

void ODBCAppender::flushBuffer()
{
  //Do the actual logging
  //removes.ensureCapacity(buffer.size());

  std::list<spi::LoggingEventPtr>::iterator i;
  for (i = buffer.begin(); i != buffer.end(); i++)
  {
    try
    {
      const LoggingEventPtr& logEvent = *i;
      String sql = getLogStatement(logEvent);
      execute(sql);
    }
    catch (SQLException& e)
    {
      errorHandler->error(_T("Failed to excute sql"), e,
        ErrorCode::FLUSH_FAILURE);
    }
  }
  
  // clear the buffer of reported events
  buffer.clear();
}

void ODBCAppender::setSql(const String& s)
{
  sqlStatement = s;
  if (getLayout() == 0)
  {
    this->setLayout(new PatternLayout(s));
  }
  else
  {
    PatternLayoutPtr patternLayout = this->getLayout();
    if (patternLayout != 0)
    {
      patternLayout->setConversionPattern(s);
    }
  }
}

#endif //HAVE_ODBC
