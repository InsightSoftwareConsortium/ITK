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
 
#ifndef _LOG4CXX_WRITER_APPENDER_H
#define _LOG4CXX_WRITER_APPENDER_H

#include <log4cxx/appenderskeleton.h>

namespace log4cxx
{
  class WriterAppender;
  typedef helpers::ObjectPtrT<WriterAppender> WriterAppenderPtr;

  /**
  WriterAppender appends log events to a standard output stream
  (ostream or wostream)
  */
  class LOG4CXX_EXPORT WriterAppender : public AppenderSkeleton
  {
  protected:
    /**
    Immediate flush means that the underlying writer or output stream
    will be flushed at the end of each append operation. Immediate
    flush is slower but ensures that each append request is actually
    written. If <code>immediateFlush</code> is set to



    <code>false</code>, then there is a good chance that the last few
    logs events are not actually written to persistent media if and
    when the application crashes.

    <p>The <code>immediateFlush</code> variable is set to
    <code>true</code> by default.

    */
    bool immediateFlush;
    
    /**
    The encoding to use when opening an input stream.  
    <p>The <code>encoding</code> variable is set to <code>""</code> by
    default which results in the utilization of the system's default
    encoding.  */
    String encoding;
    
    /** This is the output stream where we will write to.*/
    ostream * os;
  
  
  public:
    DECLARE_ABSTRACT_LOG4CXX_OBJECT(WriterAppender)
    BEGIN_LOG4CXX_CAST_MAP()
      LOG4CXX_CAST_ENTRY(WriterAppender)
      LOG4CXX_CAST_ENTRY_CHAIN(AppenderSkeleton)
    END_LOG4CXX_CAST_MAP()

    /**
    This default constructor does nothing.*/
    WriterAppender();
  
    /**
    Instantiate a WriterAppender and set the output destination to
    <code>os</code>.*/
    WriterAppender(const LayoutPtr& layout, ostream * os);

    ~WriterAppender();
    
    /**
    If the <b>ImmediateFlush</b> option is set to
    <code>true</code>, the appender will flush at the end of each
    write. This is the default behavior. If the option is set to
    <code>false</code>, then the underlying stream can defer writing
    to physical medium to a later time.
  
    <p>Avoiding the flush operation at the end of each append results in
    a performance gain of 10 to 20 percent. However, there is safety
    tradeoff involved in skipping flushing. Indeed, when flushing is
    skipped, then it is likely that the last few log events will not
    be recorded on disk when the application exits. This is a high
    price to pay even for a 20% performance gain.
    */
    void setImmediateFlush(bool value) { immediateFlush = value; }
  
    /**
    Returns value of the <b>ImmediateFlush</b> option.
    */
    bool getImmediateFlush() const { return immediateFlush; }
  
    /**
    This method is called by the AppenderSkeleton#doAppend
    method.

    <p>If the output stream exists and is writable then write a log
    statement to the output stream. Otherwise, write a single warning
    message to <code>stderr</code>.

    <p>The format of the output will depend on this appender's
    layout.

    */
    virtual void append(const spi::LoggingEventPtr& event);
  
  protected:
    /**
    This method determines if there is a sense in attempting to append.

    <p>It checks whether there is a set output target and also if
    there is a set layout. If these checks fail, then the boolean
    value <code>false</code> is returned. */
    virtual bool checkEntryConditions() const;
  
  
  public:
    /**
    Close this appender instance. The underlying stream or writer is
    also closed.

    <p>Closed appenders cannot be reused.
    */
    virtual void close();

  
  protected:
    /**
    * Close the underlying output stream.
    * */
    virtual void closeWriter() = 0;
  
  
  public:
    const String& getEncoding() const { return encoding; }
    void setEncoding(const String& value) { encoding = value; }
  
  protected:
    /**

    Actual writing occurs here.
  
    <p>Most subclasses of <code>WriterAppender</code> will need to
    override this method.
    */
    virtual void subAppend(const spi::LoggingEventPtr& event);
  
  /**
  The WriterAppender requires a layout. Hence, this method returns
  <code>true</code>.
  */
  public:
    virtual bool requiresLayout() const { return true; }
  
  /**
  Clear internal references to the writer and other variables.
  
    Subclasses can override this method for an alternate closing
  behavior.  */

  protected:
    virtual void reset();  
  
  /**
  Write a footer as produced by the embedded layout's 
  Layout#appendFooter method.  */
  protected:
    virtual void writeFooter();
  
  /**
  Write a header as produced by the embedded layout's 
  Layout#appendHeader method.  */
  protected:
    virtual void writeHeader();
  };
}  //namespace log4cxx

#endif //_LOG4CXX_WRITER_APPENDER_H
