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
 
#include <log4cxx/asyncappender.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/boundedfifo.h>
#include <log4cxx/spi/loggingevent.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;

IMPLEMENT_LOG4CXX_OBJECT(AsyncAppender)
IMPLEMENT_LOG4CXX_OBJECT(Dispatcher)

/** The default buffer size is set to 128 events. */
int AsyncAppender::DEFAULT_BUFFER_SIZE = 128;

AsyncAppender::AsyncAppender()
: locationInfo(false), interruptedWarningMessage(false)
{
  bf = new BoundedFIFO(DEFAULT_BUFFER_SIZE);
  
    aai = new AppenderAttachableImpl();

  dispatcher = new Dispatcher(bf, this);
  dispatcher->start();
}

AsyncAppender::~AsyncAppender()
{
  finalize();
}

void AsyncAppender::addAppender(const AppenderPtr& newAppender)
{
  synchronized sync(aai);
  aai->addAppender(newAppender);
}

void AsyncAppender::append(const spi::LoggingEventPtr& event)
{
  // Set the NDC and thread name for the calling thread as these
  // LoggingEvent fields were not set at event creation time.
  event->getNDC();
  // Get a copy of this thread's MDC.
  event->getMDCCopy();
  
/*  if(locationInfo)
  {
    event.getLocationInformation();
  }*/
  
  synchronized sync(bf);

  while(bf->isFull())
  {
    //LOGLOG_DEBUG(_T("Waiting for free space in buffer, ")
    //   << bf->length());
    bf->wait();
  }

  bf->put(event);
  if(bf->wasEmpty())
  {
    //LOGLOG_DEBUG(_T("Notifying dispatcher to process events."));
    bf->notify();
  }
}

void AsyncAppender::close()
{
  {
    synchronized sync(this);
    // avoid multiple close, otherwise one gets NullPointerException
    if(closed)
    {
      return;
    }
    
    closed = true;
  }
  
  // The following cannot be synchronized on "this" because the
  // dispatcher synchronizes with "this" in its while loop. If we
  // did synchronize we would systematically get deadlocks when
  // close was called.
  dispatcher->close();
  
  dispatcher->join();
  dispatcher = 0;
  bf = 0;
}

AppenderList AsyncAppender::getAllAppenders() const
{
  synchronized sync(aai);
  return aai->getAllAppenders();
}

AppenderPtr AsyncAppender::getAppender(const String& name) const
{
  synchronized sync(aai);
  return aai->getAppender(name);
}

bool AsyncAppender::isAttached(const AppenderPtr& appender) const
{
  synchronized sync(aai);
  return aai->isAttached(appender);
}

void AsyncAppender::setBufferSize(int size)
{
  bf->resize(size);
}

int AsyncAppender::getBufferSize() const
{
  return bf->getMaxSize();
}

void AsyncAppender::removeAllAppenders()
{
    synchronized sync(aai);
  aai->removeAllAppenders();
}

void AsyncAppender::removeAppender(const AppenderPtr& appender)
{
    synchronized sync(aai);
  aai->removeAppender(appender);
}

void AsyncAppender::removeAppender(const String& name)
{
    synchronized sync(aai);
  aai->removeAppender(name);
}

Dispatcher::Dispatcher(helpers::BoundedFIFOPtr bf, AsyncAppender * container)
 : bf(bf), container(container), aai(container->aai), interrupted(false)
{
  // set the dispatcher priority to lowest possible value
  setPriority(Thread::MIN_PRIORITY);
}

void Dispatcher::close()
{
  synchronized sync(bf);

  interrupted = true;
  // We have a waiting dispacther if and only if bf.length is
  // zero.  In that case, we need to give it a death kiss.
  if(bf->length() == 0)
  {
    bf->notify();
  }
}

void Dispatcher::run()
{
  LoggingEventPtr event;

  while(true)
  {
    {
      synchronized sync(bf);
      
      if(bf->length() == 0)
      {
        // Exit loop if interrupted but only if
        // the buffer is empty.
        if(interrupted)
        {
          //LOGLOG_DEBUG("Exiting.");
          break;
        }
        //LOGLOG_DEBUG("Waiting for new event to dispatch.");
        bf->wait();
      }
      
      event = bf->get();
      if(bf->wasFull())
      {
        //LOGLOG_DEBUG("Notifying AsyncAppender about freed space.");
        bf->notify();
      }
    } // synchronized

    if(aai != 0 && event != 0)
    {
      synchronized sync(aai);
      aai->appendLoopOnAppenders(event);
    }
  } // while

  // close and remove all appenders
  aai->removeAllAppenders();
}
