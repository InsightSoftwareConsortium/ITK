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
 
#ifndef _LOG4CXX_ASYNC_APPENDER_H
#define _LOG4CXX_ASYNC_APPENDER_H

#include <log4cxx/appenderskeleton.h>
#include <log4cxx/helpers/appenderattachableimpl.h>
#include <log4cxx/helpers/thread.h>

namespace log4cxx
{
  namespace helpers
  {
    class BoundedFIFO;
    typedef ObjectPtrT<BoundedFIFO> BoundedFIFOPtr;
  } 

  class Dispatcher;
  typedef helpers::ObjectPtrT<Dispatcher> DispatcherPtr;

  class AsyncAppender;
  typedef helpers::ObjectPtrT<AsyncAppender> AsyncAppenderPtr;
  
  /**
  The AsyncAppender lets users log events asynchronously. It uses a
  bounded buffer to store logging events.

  <p>The AsyncAppender will collect the events sent to it and then
  dispatch them to all the appenders that are attached to it. You can
  attach multiple appenders to an AsyncAppender.

  <p>The AsyncAppender uses a separate thread to serve the events in
  its bounded buffer.

  <p><b>Important note:</b> The <code>AsyncAppender</code> can only
  be script configured using the {@link xml::DOMConfigurator DOMConfigurator}.
  */
  class LOG4CXX_EXPORT AsyncAppender :
    public virtual spi::AppenderAttachable,
    public virtual AppenderSkeleton
  {
  friend class Dispatcher;

  public:
    DECLARE_LOG4CXX_OBJECT(AsyncAppender)
    BEGIN_LOG4CXX_CAST_MAP()
      LOG4CXX_CAST_ENTRY(AsyncAppender)
      LOG4CXX_CAST_ENTRY_CHAIN(AppenderSkeleton)
      LOG4CXX_CAST_ENTRY(spi::AppenderAttachable)
    END_LOG4CXX_CAST_MAP()

    /** The default buffer size is set to 128 events. */
    static int DEFAULT_BUFFER_SIZE;

    helpers::BoundedFIFOPtr bf;
    helpers::AppenderAttachableImplPtr aai;
    DispatcherPtr dispatcher;
    bool locationInfo;
    bool interruptedWarningMessage;

    AsyncAppender();
    virtual ~AsyncAppender();

    void addAppender(const AppenderPtr& newAppender);

    void append(const spi::LoggingEventPtr& event);

    /**
    Close this <code>AsyncAppender</code> by interrupting the
    dispatcher thread which will process all pending events before
    exiting.
    */
    void close();

    AppenderList getAllAppenders() const;
    AppenderPtr getAppender(const String& name) const;

    /**
    Returns the current value of the <b>LocationInfo</b> option.
    */
    inline bool getLocationInfo() const
      { return locationInfo; }

    /**
    Is the appender passed as parameter attached to this asyncappender?
    */
    bool isAttached(const AppenderPtr& appender) const;

    void removeAllAppenders();
    void removeAppender(const AppenderPtr& appender);
    void removeAppender(const String& name);

    /**
    The <code>AsyncAppender</code> does not require a layout. Hence,
    this method always returns <code>false</code>.
    */
    virtual bool requiresLayout() const
      { return false; }

    /**
    * The <b>LocationInfo</b> option takes a boolean value. By default,
    * it is set to false which means there will be no effort to extract
    * the location information related to the event. As a result, the
    * event that will be ultimately logged will likely to contain the
    * wrong location information (if present in the log format).
    *
    * <p>Location information extraction is comparatively very slow and
    * should be avoided unless performance is not a concern.
    * */
    inline void setLocationInfo(bool flag)
      { locationInfo = flag; }

    /**
    * The <b>BufferSize</b> option takes a non-negative integer value.
    * This integer value determines the maximum size of the bounded
    * buffer. Increasing the size of the buffer is always
    * safe. However, if an existing buffer holds unwritten elements,
    * then <em>decreasing the buffer size will result in event
    * loss.</em> Nevertheless, while script configuring the
    * AsyncAppender, it is safe to set a buffer size smaller than the
    * {@link #DEFAULT_BUFFER_SIZE default buffer size} because
    * configurators guarantee that an appender cannot be used before
    * being completely configured.
    * */
    void setBufferSize(int size);

    /**
    Returns the current value of the <b>BufferSize</b> option.
    */
    int getBufferSize() const;
  }; // class AsyncAppender

  class LOG4CXX_EXPORT Dispatcher : public  helpers::Thread
  {
    helpers::BoundedFIFOPtr bf;
    helpers::AppenderAttachableImplPtr aai;
    bool interrupted;
    AsyncAppender * container;

  public:
    DECLARE_ABSTRACT_LOG4CXX_OBJECT(Dispatcher)
    BEGIN_LOG4CXX_CAST_MAP()
      LOG4CXX_CAST_ENTRY(Dispatcher)
      LOG4CXX_CAST_ENTRY_CHAIN(Thread)
    END_LOG4CXX_CAST_MAP()

    Dispatcher(helpers::BoundedFIFOPtr bf, AsyncAppender * container);
    void close();

    /**
    The dispatching strategy is to wait until there are events in the
    buffer to process. After having processed an event, we release
    the monitor (variable bf) so that new events can be placed in the
    buffer, instead of keeping the monitor and processing the remaining
    events in the buffer.
    <p>Other approaches might yield better results.
    */
    void run();
  }; // class Dispatcher
}  //  namespace log4cxx

#endif//  _LOG4CXX_ASYNC_APPENDER_H

