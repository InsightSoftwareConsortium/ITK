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
 
#ifndef _LOG4CXX_ROLLING_FILE_APPENDER_H
#define _LOG4CXX_ROLLING_FILE_APPENDER_H

#include <log4cxx/fileappender.h>
#include <log4cxx/helpers/optionconverter.h>

namespace log4cxx
{
  /**
  RollingFileAppender extends FileAppender to backup the log files when
  they reach a certain size.
  */
  class LOG4CXX_EXPORT RollingFileAppender : public FileAppender
  {
  protected:
    /**
    The default maximum file size is 10MB.
    */
    long maxFileSize;

    /**
    There is one backup file by default.
    */
    int  maxBackupIndex;

  public:
    DECLARE_LOG4CXX_OBJECT(RollingFileAppender)
    BEGIN_LOG4CXX_CAST_MAP()
      LOG4CXX_CAST_ENTRY(RollingFileAppender)
      LOG4CXX_CAST_ENTRY_CHAIN(FileAppender)
    END_LOG4CXX_CAST_MAP()
    /**
    The default constructor simply calls its {@link
    FileAppender#FileAppender parents constructor}.  */
    RollingFileAppender();

    /**
    Instantiate a RollingFileAppender and open the file designated by
    <code>filename</code>. The opened filename will become the ouput
    destination for this appender.

    <p>If the <code>append</code> parameter is true, the file will be
    appended to. Otherwise, the file desginated by
    <code>filename</code> will be truncated before being opened.
    */
    RollingFileAppender(const LayoutPtr& layout, const String& fileName, bool append);

    /**
    Instantiate a FileAppender and open the file designated by
    <code>filename</code>. The opened filename will become the output
    destination for this appender.
    <p>The file will be appended to.  */
    RollingFileAppender(const LayoutPtr& layout, const String& fileName);
    
    ~RollingFileAppender();

    /**
    Returns the value of the <b>MaxBackupIndex</b> option.
    */
    inline int getMaxBackupIndex() const
      { return maxBackupIndex; }

    /**
    Get the maximum size that the output file is allowed to reach
    before being rolled over to backup files.
    */
    inline long getMaximumFileSize() const
      { return maxFileSize; }

    /**
    Implements the usual roll over behaviour.

    <p>If <code>MaxBackupIndex</code> is positive, then files
    {<code>File.1</code>, ..., <code>File.MaxBackupIndex -1</code>}
    are renamed to {<code>File.2</code>, ...,
    <code>File.MaxBackupIndex</code>}. Moreover, <code>File</code> is
    renamed <code>File.1</code> and closed. A new <code>File</code> is
    created to receive further log output.

    <p>If <code>MaxBackupIndex</code> is equal to zero, then the
    <code>File</code> is truncated with no backup files created.
    */
    // synchronization not necessary since doAppend is alreasy synched
    void rollOver();

    /**
    Set the maximum number of backup files to keep around.

    <p>The <b>MaxBackupIndex</b> option determines how many backup
    files are kept before the oldest is erased. This option takes
    a positive integer value. If set to zero, then there will be no
    backup files and the log file will be truncated when it reaches
    <code>MaxFileSize</code>.
    */
    inline void setMaxBackupIndex(int maxBackupIndex)
      { this->maxBackupIndex = maxBackupIndex; }

    /**
    Set the maximum size that the output file is allowed to reach
    before being rolled over to backup files.

    <p>In configuration files, the <b>MaxFileSize</b> option takes an
    long integer in the range 0 - 2^63. You can specify the value
    with the suffixes "KB", "MB" or "GB" so that the integer is
    interpreted being expressed respectively in kilobytes, megabytes
    or gigabytes. For example, the value "10KB" will be interpreted
    as 10240.
    */
    inline void setMaxFileSize(const String& value)
      { maxFileSize = helpers::OptionConverter::toFileSize(
        value, maxFileSize + 1); }


    virtual void setOption(const String& option, const String& value);
      
  protected:
    /**
    This method differentiates RollingFileAppender from its parent
    class.
    */
    virtual void subAppend(const spi::LoggingEventPtr& event);
  }; // class RollingFileAppender
}  // namespace log4cxx

#endif //_LOG4CXX_ROLLING_FILE_APPENDER_H
