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

#include <log4cxx/spi/configurator.h>

using namespace log4cxx;
using namespace log4cxx::spi;

IMPLEMENT_LOG4CXX_OBJECT(Configurator);

/**
Special level value signifying inherited behaviour. The current
value of this string constant is <b>inherited</b>. #NuLL
is a synonym.  */
String Configurator::INHERITED = _T("inherited");
      
/**
Special level signifying inherited behaviour, same as
#INHERITED. The current value of this string constant is
<b>null</b>. */
String Configurator::NuLL = _T("null");
