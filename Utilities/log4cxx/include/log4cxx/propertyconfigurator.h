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
 
#ifndef _LOG4CXX_PROPERTY_CONFIGURATOR_H
#define _LOG4CXX_PROPERTY_CONFIGURATOR_H

#include <log4cxx/helpers/objectptr.h>
#include <log4cxx/helpers/objectimpl.h>
#include <log4cxx/config.h>
#include <log4cxx/helpers/tchar.h>
#include <log4cxx/spi/configurator.h>
#include <map>

namespace log4cxx
{
  class Logger;
  typedef helpers::ObjectPtrT<Logger> LoggerPtr;

  class Appender;
  typedef helpers::ObjectPtrT<Appender> AppenderPtr;

  namespace spi
  {
    class LoggerFactory;
    typedef helpers::ObjectPtrT<LoggerFactory> LoggerFactoryPtr;

    class LoggerRepository;
    typedef helpers::ObjectPtrT<LoggerRepository> LoggerRepositoryPtr;
  } 

  namespace helpers
  {
    class Properties;
  } 

/**
Allows the configuration of log4cxx from an external file.  See
<b>{@link #doConfigure(const String&, spi::LoggerRepositoryPtr&)}</b>
for the expected format.

<p>It is sometimes useful to see how log4cxx is reading configuration
files. You can enable log4cxx internal logging by defining the
<b>log4j.debug</b> variable.

<P>At class initialization time class,
the file <b>log4j.properties</b> will be searched in the current directory.
If the file can be found, then it will
be fed to the 
{@link PropertyConfigurator#configure(const String& configFilename)}
method.

<p>The <code>PropertyConfigurator</code> does not handle the
advanced configuration features supported by the {@link
xml::DOMConfigurator DOMConfigurator} such as
support for {@link spi::Filter Filters}, custom
{@link spi::ErrorHandler ErrorHandlers}, nested
appenders such as the {@link AsyncAppender
AsyncAppender}, etc.

<p>All option <em>values</em> admit variable substitution. The
syntax of variable substitution is similar to that of Unix
shells. The string between an opening <b>&quot;${&quot;</b> and
closing <b>&quot;}&quot;</b> is interpreted as a key. The value of
the substituted variable can be defined as a system property or in
the configuration file itself. The value of the key is first
searched in the system properties, and if not found there, it is
then searched in the configuration file being parsed.  The
corresponding value replaces the ${variableName} sequence. For
example, if <code>java.home</code> system property is set to
<code>/home/xyz</code>, then every occurrence of the sequence
<code>${java.home}</code> will be interpreted as
<code>/home/xyz</code>.
*/
  class LOG4CXX_EXPORT PropertyConfigurator :
    virtual public spi::Configurator,
    virtual public helpers::ObjectImpl
  {
  protected:
    static String CATEGORY_PREFIX;
    static String LOGGER_PREFIX;
    static String FACTORY_PREFIX;
    static String ADDITIVITY_PREFIX;
    static String ROOT_CATEGORY_PREFIX;
    static String ROOT_LOGGER_PREFIX;
    static String APPENDER_PREFIX;
    static String RENDERER_PREFIX;
    static String THRESHOLD_PREFIX;

    /* Key for specifying the {@link spi::LoggerFactory
      LoggerFactory}.  Currently set to "<code>log4j.loggerFactory</code>".  */
    static String LOGGER_FACTORY_KEY;
    static String INTERNAL_ROOT_NAME;

    /**
    Used internally to keep track of configured appenders.
    */
    std::map<String, AppenderPtr> registry;
    spi::LoggerFactoryPtr loggerFactory;

  public:
    DECLARE_LOG4CXX_OBJECT(PropertyConfigurator)
    BEGIN_LOG4CXX_CAST_MAP()
      LOG4CXX_CAST_ENTRY(spi::Configurator)
    END_LOG4CXX_CAST_MAP()

    PropertyConfigurator();

/**
Read configuration from a file. <b>The existing configuration is
not cleared nor reset.</b> If you require a different behavior,
then call {@link LogManager#resetConfiguration
resetConfiguration} method before calling
<code>doConfigure</code>.

<p>The configuration file consists of statements in the format
<code>key=value</code>. The syntax of different configuration
elements are discussed below.

<h3>Repository-wide threshold</h3>

<p>The repository-wide threshold filters logging requests by level
regardless of logger. The syntax is:

<pre>
log4j.threshold=[level]
</pre>

<p>The level value can consist of the string values OFF, FATAL,
ERROR, WARN, INFO, DEBUG, ALL or a <em>custom level</em> value. A
custom level value can be specified in the form
level#classname. By default the repository-wide threshold is set
to the lowest possible value, namely the level <code>ALL</code>.
</p>


<h3>Appender configuration</h3>

<p>Appender configuration syntax is:
<pre>
# For appender named <i>appenderName</i>, set its class.
# Note: The appender name can contain dots.
log4j.appender.appenderName=fully.qualified.name.of.appender.class

# Set appender specific options.
log4j.appender.appenderName.option1=value1
...
log4j.appender.appenderName.optionN=valueN
</pre>

For each named appender you can configure its {@link Layout Layout}. The
syntax for configuring an appender's layout is:
<pre>
log4j.appender.appenderName.layout=fully.qualified.name.of.layout.class
log4j.appender.appenderName.layout.option1=value1
....
log4j.appender.appenderName.layout.optionN=valueN
</pre>

<h3>Configuring loggers</h3>

<p>The syntax for configuring the root logger is:
<pre>
log4j.rootLogger=[level], appenderName, appenderName, ...
</pre>

<p>This syntax means that an optional <em>level</em> can be
supplied followed by appender names separated by commas.

<p>The level value can consist of the string values OFF, FATAL,
ERROR, WARN, INFO, DEBUG, ALL or a <em>custom level</em> value. A
custom level value can be specified in the form
<code>level#classname</code>.

<p>If a level value is specified, then the root level is set
to the corresponding level.  If no level value is specified,
then the root level remains untouched.

<p>The root logger can be assigned multiple appenders.

<p>Each <i>appenderName</i> (separated by commas) will be added to
the root logger. The named appender is defined using the
appender syntax defined above.

<p>For non-root categories the syntax is almost the same:
<pre>
log4j.logger.logger_name=[level|INHERITED|NULL], appenderName, appenderName, 
...
</pre>

<p>The meaning of the optional level value is discussed above
in relation to the root logger. In addition however, the value
INHERITED can be specified meaning that the named logger should
inherit its level from the logger hierarchy.

<p>If no level value is supplied, then the level of the
named logger remains untouched.

<p>By default categories inherit their level from the
hierarchy. However, if you set the level of a logger and later
decide that that logger should inherit its level, then you should
specify INHERITED as the value for the level value. NULL is a
synonym for INHERITED.

<p>Similar to the root logger syntax, each <i>appenderName</i>
(separated by commas) will be attached to the named logger.

<p>See the <a href="../../../../manual.html#additivity">appender
additivity rule</a> in the user manual for the meaning of the
<code>additivity</code> flag.

<h3>Logger Factories</h3>

The usage of custom logger factories is discouraged and no longer
documented.

<h3>Example</h3>

<p>An example configuration is given below. Other configuration
file examples are given in the <code>examples</code> folder.

<pre>

# Set options for appender named "A1".
# Appender "A1" will be a SyslogAppender
log4j.appender.A1=SyslogAppender

# The syslog daemon resides on www.abc.net
log4j.appender.A1.SyslogHost=www.abc.net

# A1's layout is a PatternLayout, using the conversion pattern
# <b>%r %-5p %c{2} %M.%L %x - %m\n</b>. Thus, the log output will
# include # the relative time since the start of the application in
# milliseconds, followed by the level of the log request,
# followed by the two rightmost components of the logger name,
# followed by the callers method name, followed by the line number,
# the nested disgnostic context and finally the message itself.
# Refer to the documentation of PatternLayout for further information
# on the syntax of the ConversionPattern key.
log4j.appender.A1.layout=PatternLayout
log4j.appender.A1.layout.ConversionPattern=%-4r %-5p %c{2} %M.%L %x - %m\n

# Set options for appender named "A2"
# A2 should be a RollingFileAppender, with maximum file size of 10 MB
# using at most one backup file. A2's layout is TTCC, using the
# ISO8061 date format with context printing enabled.
log4j.appender.A2=RollingFileAppender
log4j.appender.A2.MaxFileSize=10MB
log4j.appender.A2.MaxBackupIndex=1
log4j.appender.A2.layout=TTCCLayout
log4j.appender.A2.layout.ContextPrinting=enabled
log4j.appender.A2.layout.DateFormat=ISO8601

# Root logger set to DEBUG using the A2 appender defined above.
log4j.rootLogger=DEBUG, A2

# Logger definitions:
# The SECURITY logger inherits is level from root. However, it's output
# will go to A1 appender defined above. It's additivity is non-cumulative.
log4j.logger.SECURITY=INHERIT, A1
log4j.additivity.SECURITY=false

# Only warnings or above will be logged for the logger "SECURITY.access".
# Output will go to A1.
log4j.logger.SECURITY.access=WARN


# The logger "class.of.the.day" inherits its level from the
# logger hierarchy.  Output will go to the appender's of the root
# logger, A2 in this case.
log4j.logger.class.of.the.day=INHERIT
</pre>

<p>Refer to the <b>setOption</b> method in each Appender and
Layout for class specific options.

<p>Use the <code>#</code> or <code>!</code> characters at the
beginning of a line for comments.

<p>
@param configFileName The name of the configuration file where the
configuration information is stored.
@param hierarchy The hierarchy to operation upon.
*/
    void doConfigure(const String& configFileName, 
      spi::LoggerRepositoryPtr& hierarchy);

    /**
    Read configuration options from file <code>configFilename</code>.
    */
    static void configure(const String& configFilename);

    /**
    Like {@link #configureAndWatch(const String& configFilename, long delay)}
    except that the
    default delay as defined by helpers::FileWatchdog#DEFAULT_DELAY
    is used.
    @param configFilename A file in key=value format.
    */
    static void configureAndWatch(const String& configFilename);

    /**
    Read the configuration file <code>configFilename</code> if it
    exists. Moreover, a thread will be created that will periodically
    check if <code>configFilename</code> has been created or
    modified. The period is determined by the <code>delay</code>
    argument. If a change or file creation is detected, then
    <code>configFilename</code> is read to configure log4j.

    @param configFilename A file in key=value format.
    @param delay The delay in milliseconds to wait between each check.
    */
    static void configureAndWatch(const String& configFilename,
      long delay);

    /**
    Read configuration options from <code>properties</code>.
    See #doConfigure(const string&, spi::LoggerRepositoryPtr&)
    for the expected format.
    */
    static void configure(helpers::Properties& properties);

    /**
    Read configuration options from <code>properties</code>.
    See #doConfigure(const String&, spi::LoggerRepositoryPtr&)
    for the expected format.
    */
    void doConfigure(helpers::Properties& properties, 
      spi::LoggerRepositoryPtr& hierarchy);

// --------------------------------------------------------------------------
// Internal stuff
// --------------------------------------------------------------------------
protected:
    /**
    Check the provided <code>Properties</code> object for a
    #LoggerFactory
    entry specified by #LOGGER_FACTORY_KEY.  If such an entry
    exists, an attempt is made to create an instance using the default
    constructor.  This instance is used for subsequent Logger creations
    within this configurator.
    @see #parseCatsAndRenderers
    */
    void configureLoggerFactory(helpers::Properties& props);

    void configureRootCategory(helpers::Properties& props,
      spi::LoggerRepositoryPtr& hierarchy);

    /**
    Parse non-root elements, such non-root categories and renderers.
    */
    void parseCatsAndRenderers(helpers::Properties& props,
      spi::LoggerRepositoryPtr& hierarchy);

    /**
    Parse the additivity option for a non-root logger.
    */
    void parseAdditivityForLogger(helpers::Properties& props,
      LoggerPtr& cat, const String& loggerName);

    /**
    This method must work for the root logger as well.
    */
    void parseCategory(
      helpers::Properties& props, LoggerPtr& logger,
      const String& optionKey, const String& loggerName,
      const String& value);

    AppenderPtr parseAppender(
      helpers::Properties& props, const String& appenderName);

    void registryPut(const AppenderPtr& appender);
    AppenderPtr registryGet(const String& name);
  }; // class PropertyConfigurator
}  // namespace log4cxx

#endif //_LOG4CXX_PROPERTY_CONFIGURATOR_H
