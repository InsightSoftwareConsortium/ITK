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

#include <log4cxx/helpers/boundedfifo.h>
#include <log4cxx/spi/loggingevent.h>
#include <log4cxx/helpers/exception.h>
#include <algorithm>
#include <log4cxx/helpers/strictmath.h>

using namespace log4cxx::helpers;
using namespace log4cxx::spi;

IMPLEMENT_LOG4CXX_OBJECT(BoundedFIFO)

BoundedFIFO::BoundedFIFO(int maxSize)
 : numElements(0), first(0), next(0), maxSize(maxSize), buf(maxSize)
{
  if(maxSize < 1)
  {
    StringBuffer oss;
    oss << _T("The maxSize argument (") << maxSize
      << _T(") is not a positive integer.");
    throw new IllegalArgumentException(oss.str());
  }
}

LoggingEventPtr BoundedFIFO::get()
{
  if(numElements == 0)
  {
    return 0;
  }

  LoggingEventPtr r = buf[first];
  buf[first] = 0;

  if(++first == maxSize)
  {
    first = 0;
  }
  
  numElements--;
  return r;
}

void BoundedFIFO::put(const log4cxx::spi::LoggingEventPtr& o)
{
  if(numElements != maxSize)
  {
    buf[next] = o;
    if(++next == maxSize)
    {
      next = 0;
    }
    numElements++;
  }
}

void BoundedFIFO::resize(int newSize)
{
  synchronized sync(this);
  
  if(newSize == maxSize)
  {
    return;
  }
  
  std::vector<LoggingEventPtr> tmp(newSize);
  
  // we should not copy beyond the buf array
  int len1 = maxSize - first;

  // we should not copy beyond the tmp array
  len1 = StrictMath::minimum(len1, newSize);

  // er.. how much do we actually need to copy?
  // We should not copy more than the actual number of elements.
  len1 = StrictMath::minimum(len1, numElements);

  // Copy from buf starting a first, to tmp, starting at position 0, len1
  std::copy(buf.begin() + first, buf.begin() + (first + len1), tmp.begin());

  // Are there any uncopied elements and
  // is there still space in the new array?
  int len2 = 0;
  if((len1 < numElements) && (len1 < newSize))
  {
    len2 = numElements - len1;
    len2 = StrictMath::minimum(len2, newSize - len1);
    std::copy(buf.begin(), buf.begin() + (len2), tmp.begin() + len1);
  }

  this->buf = tmp;
  this->maxSize = newSize;
  this->first=0;
  this->numElements = len1+len2;
  this->next = this->numElements;

  // this should never happen, but again, it just might.
  if(this->next == this->maxSize)
  {
    this->next = 0;
  }
}

