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
 
#include <spi/loggerrepository.h> 
 
namespace log4cxx
{
  namespace helpers
  {
    class InitializationUtil
    {
    public:
      /**
      Configure <code>repository</code> using 
      <code>configuratonResourceStr</code> 
      and <code>configuratorClassNameStr</code>.  
       
      If <code>configuratonResourceStr</code>  is not a URL it will
      be searched
      as a resource from the classpath. 
      @param repository The repository to configre
      @param configuratonResourceStr URL to the configuration
      resource
      @param configuratorClassNameStr The name of the class to use as
      the configurator. This parameter can be null.
      */
      static void initialConfiguration(
        spi::LoggerRepositoryPtr repository, 
        const String& configuratonResourceStr,
        const String& configuratorClassNameStr);

    };
  }
}
