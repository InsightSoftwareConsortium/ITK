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

#ifdef HAVE_MS_THREAD
#include <windows.h>
#endif

#include <log4cxx/helpers/objectimpl.h>
#include <log4cxx/helpers/criticalsection.h>
#include <log4cxx/helpers/event.h>
#include <log4cxx/helpers/thread.h>

using namespace log4cxx::helpers;

class EventList
{
protected:
  EventList(Event * event)
  : event(event), next(0)
  {
  }
  
public:
  static void removeAll(EventList * list)
  {
    EventList * item = list;
    while (item != 0)
    {
      item = removeHead(item);
    }
  }
  
  static EventList * removeHead(EventList * list)
  {
    EventList * next = list->next;
    delete list;
    return next;
  }
  
  static EventList * append(EventList * list, Event * event)
  {
    if (list == 0)
    {
      return new EventList(event);
    }
    else
    {
      EventList * current = list;
      EventList * next = list->next;
      while (next != 0)
      {
        current = next;
        next = next->next;
      }
      current->next = new EventList(event);
      return list;
    }
  }
  
  Event * event;
  EventList * next;
};

ObjectImpl::ObjectImpl() : ref(0), eventList(0)
{
}

ObjectImpl::~ObjectImpl()
{
}

void ObjectImpl::addRef() const
{
  Thread::InterlockedIncrement(&ref);
}

void ObjectImpl::releaseRef() const
{
  if (Thread::InterlockedDecrement(&ref) == 0)
  {
    delete this;
  }
}

void ObjectImpl::lock() const
{
  cs.lock();
}

void ObjectImpl::unlock() const
{
  cs.unlock();
}

void ObjectImpl::wait() const
{
  if (cs.getOwningThread() != Thread::getCurrentThreadId())
  {
    if (cs.getOwningThread() == 0)
    {
      throw IllegalMonitorStateException(_T("Object not locked"));
    }
    else
    {
      throw IllegalMonitorStateException(_T("Object not locked by this thread"));
    }
  }
  
  Event event(false, false);
  eventList = EventList::append((EventList *)eventList, &event);
  cs.unlock();
  
  try
  {
    event.wait();
  }
  catch(Exception&)
  {
    cs.lock();
    eventList = EventList::removeHead((EventList *)eventList);
    return;
  }
  
  cs.lock();
}

void ObjectImpl::notify() const
{
  if (cs.getOwningThread() != Thread::getCurrentThreadId())
  {
    if (cs.getOwningThread() == 0)
    {
      throw IllegalMonitorStateException(_T("Object not locked"));
    }
    else
    {
      throw IllegalMonitorStateException(_T("Object not locked by this thread"));
    }
  }
  
  if (eventList != 0)
  {
    ((EventList *)eventList)->event->set();
    eventList = EventList::removeHead((EventList *)eventList);
  }
}

void ObjectImpl::notifyAll() const
{
  if (cs.getOwningThread() != Thread::getCurrentThreadId())
  {
    if (cs.getOwningThread() == 0)
    {
      throw IllegalMonitorStateException(_T("Object not locked"));
    }
    else
    {
      throw IllegalMonitorStateException(_T("Object not locked by this thread"));
    }
  }
  
  while (eventList != 0)
  {
    ((EventList *)eventList)->event->set();
    eventList = EventList::removeHead((EventList *)eventList);
  }
}

