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
 
#include <log4cxx/ndc.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

NDC::DiagnosticContext::DiagnosticContext(const String& message, 
  const DiagnosticContext * parent)
  : message(message)
{
  if (parent != 0)
  {
    fullMessage = parent->fullMessage + _T(' ') + message;
  } 
  else
  {
    fullMessage = message;
  }
}

// static member instanciation
ThreadSpecificData NDC::threadSpecificData;

NDC::NDC(const String& message)
{
  push(message);
}

NDC::~NDC()
{
  pop();
}

NDC::Stack * NDC::getCurrentThreadStack()
{
  return (Stack *)threadSpecificData.GetData();
}

void NDC::setCurrentThreadStack(NDC::Stack * stack)
{
  threadSpecificData.SetData((void *)stack); 
}

void NDC::clear()
{
  Stack * stack = getCurrentThreadStack();    
  if(stack != 0)
  {
    delete stack;
    setCurrentThreadStack(0);
  } 
}

NDC::Stack * NDC::cloneStack()
{
  Stack * stack = getCurrentThreadStack();
  if(stack != 0)
  {
    return new Stack(*stack);
  }
  else
  {
    return new Stack();
  }
}

void NDC::inherit(NDC::Stack * stack)
{
  if(stack != 0)
  {
    Stack * oldStack = getCurrentThreadStack();
    if(oldStack != 0)
    {
      delete oldStack;
    }
  
    setCurrentThreadStack(stack);
  }
}

String NDC::get()
{
  Stack * stack = getCurrentThreadStack();
  if(stack != 0 && !stack->empty())
  {
    return stack->top().fullMessage;
  }
  else
  {
    return String();
  }
}

int NDC::getDepth()
{
  Stack * stack = getCurrentThreadStack();
  if(stack == 0)
  {
    return 0;
  }
  else
  {
    return stack->size();
  }
}

String NDC::pop()
{
  Stack * stack = getCurrentThreadStack();
  if(stack != 0 && !stack->empty())
  {
    String message = stack->top().message;
    stack->pop();
    if (stack->empty())
    {
      delete stack;
      setCurrentThreadStack(0);
    }
    return message;
  }
  else
  {
    return String();
  }
}

String NDC::peek()
{
  Stack * stack = getCurrentThreadStack();
  if(stack != 0 && !stack->empty())
  {
    return stack->top().message;
  }
  else
  {
    return String();
  }
}

void NDC::push(const String& message)
{
  Stack * stack = getCurrentThreadStack();

  if (stack == 0)
  {
    stack = new Stack;
    setCurrentThreadStack(stack);
    stack->push(DiagnosticContext(message, 0));
  }
  else if (stack->empty())
  {
    stack->push(DiagnosticContext(message, 0));
  }
  else
  {
    DiagnosticContext& parent = stack->top();
    stack->push(DiagnosticContext(message, &parent));
  }
}

void NDC::remove()
{
  Stack * stack = getCurrentThreadStack();
  if(stack != 0)
  {
    delete stack;
    setCurrentThreadStack(0);
  }
}
