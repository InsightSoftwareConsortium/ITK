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

#ifdef HAVE_XML

#ifdef HAVE_LIBXML2
#include <log4cxx/helpers/gnomexml.h>
#elif defined(HAVE_MS_XML)
#include <windows.h>
#include <log4cxx/helpers/msxml.h>
#endif


#include <log4cxx/xml/domconfigurator.h>
#include <log4cxx/appender.h>
#include <log4cxx/layout.h>
#include <log4cxx/logger.h>
#include <log4cxx/logmanager.h>
#include <log4cxx/level.h>
#include <log4cxx/spi/filter.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/helpers/loader.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/config/propertysetter.h>
#include <log4cxx/spi/errorhandler.h>
#include <log4cxx/spi/loggerfactory.h>
#include <log4cxx/defaultcategoryfactory.h>
#include <log4cxx/helpers/filewatchdog.h>
#include <log4cxx/spi/loggerrepository.h>
#include <log4cxx/spi/loggingevent.h>

using namespace log4cxx;
using namespace log4cxx::xml;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;
using namespace log4cxx::config;

class XMLWatchdog  : public FileWatchdog
{
public:
  XMLWatchdog(const String& filename) : FileWatchdog(filename)
  {
  }
  
  /**
  Call DOMConfigurator#doConfigure with the
  <code>filename</code> to reconfigure log4cxx.
  */
  void doOnChange()
  {
    DOMConfigurator().doConfigure(filename, 
      LogManager::getLoggerRepository());
  }
};

AppenderPtr AppenderMap::get(const String& appenderName)
{
  AppenderPtr appender;
  std::map<String, AppenderPtr>::iterator it;
  it = map.find(appenderName);
  
  if (it != map.end())
  {
    appender = it->second;
  }

  return appender;
}

void AppenderMap::put(const String& appenderName, AppenderPtr appender)
{
  map.insert(std::map<String, AppenderPtr>::value_type(appenderName, appender));
}

IMPLEMENT_LOG4CXX_OBJECT(DOMConfigurator)

#define CONFIGURATION_TAG _T("log4j:configuration")
#define OLD_CONFIGURATION_TAG _T("configuration")
#define APPENDER_TAG _T("appender")
#define APPENDER_REF_TAG _T("appender-ref")
#define PARAM_TAG _T("param")
#define LAYOUT_TAG _T("layout")
#define CATEGORY _T("category")
#define LOGGER _T("logger")
#define LOGGER_REF _T("logger-ref")
#define CATEGORY_FACTORY_TAG _T("categoryFactory")
#define NAME_ATTR _T("name")
#define CLASS_ATTR _T("class")
#define VALUE_ATTR _T("value")
#define ROOT_TAG _T("root")
#define ROOT_REF _T("root-ref")
#define LEVEL_TAG _T("level")
#define PRIORITY_TAG _T("priority")
#define FILTER_TAG _T("filter")
#define ERROR_HANDLER_TAG _T("errorHandler")
#define REF_ATTR _T("ref")
#define ADDITIVITY_ATTR _T("additivity")
#define THRESHOLD_ATTR _T("threshold")
#define CONFIG_DEBUG_ATTR _T("configDebug")
#define INTERNAL_DEBUG_ATTR _T("debug")

/**
Used internally to parse appenders by IDREF name.
*/
AppenderPtr DOMConfigurator::findAppenderByName(XMLDOMDocumentPtr doc, const String& appenderName)
{      
    AppenderPtr appender = ((AppenderMap *)appenderBag)->get(appenderName);
  
    if (appender != 0)
  {
    return appender;
    } 
  else
  {
    XMLDOMElementPtr element = doc->getElementById(APPENDER_TAG, appenderName);
    
    if(element == 0)
    {
      LogLog::error(_T("No appender named [")+
        appenderName+_T("] could be found.")); 
      return 0;
    }
    else
    {
      appender = parseAppender(element);
      ((AppenderMap *)appenderBag)->put(appenderName, appender);
      return appender;
    }
    } 
}

/**
 Used internally to parse appenders by IDREF element.
*/
AppenderPtr DOMConfigurator::findAppenderByReference(XMLDOMElementPtr appenderRef)
{    
  String appenderName = subst(appenderRef->getAttribute(REF_ATTR));
  XMLDOMDocumentPtr doc = appenderRef->getOwnerDocument();
  return findAppenderByName(doc, appenderName);
}

/**
Used internally to parse an appender element.
*/
AppenderPtr DOMConfigurator::parseAppender(XMLDOMElementPtr appenderElement)
{
    String className = subst(appenderElement->getAttribute(CLASS_ATTR));
  LogLog::debug(_T("Class name: [") + className+_T("]"));
    try
  {
    ObjectPtr instance = Loader::loadClass(className).newInstance();
    AppenderPtr appender = instance;
    PropertySetter propSetter(appender);
    
    appender->setName(subst(appenderElement->getAttribute(NAME_ATTR)));

    XMLDOMNodeListPtr children = appenderElement->getChildNodes();
    int length = children->getLength();
    
    for (int loop = 0; loop < length; loop++)
    {
      XMLDOMNodePtr currentNode = children->item(loop);
      
      /* We're only interested in Elements */
      if (currentNode->getNodeType() == XMLDOMNode::ELEMENT_NODE)
      {
        XMLDOMElementPtr currentElement = currentNode;
        String tagName = currentElement->getTagName();

        // Parse appender parameters 
        if (tagName == PARAM_TAG)
        {
          setParameter(currentElement, propSetter);
        }
        // Set appender layout
        else if (tagName == LAYOUT_TAG)
        {
          appender->setLayout(parseLayout(currentElement));
        }
        // Add filters
        else if (tagName == FILTER_TAG)
        {
          parseFilters(currentElement, appender);
        }
        else if (tagName == ERROR_HANDLER_TAG)
        {
          parseErrorHandler(currentElement, appender);
        }
        else if (tagName == APPENDER_REF_TAG)
        {
          String refName = subst(currentElement->getAttribute(REF_ATTR));
          if(appender->instanceof(AppenderAttachable::getStaticClass()))
          {
            AppenderAttachablePtr aa = appender;
            LogLog::debug(_T("Attaching appender named [")+
              refName+_T("] to appender named [")+
              appender->getName()+_T("]."));
            aa->addAppender(findAppenderByReference(currentElement));
          } 
          else
          {
            LogLog::error(_T("Requesting attachment of appender named [")+
              refName+ _T("] to appender named [")+ appender->getName()+
              _T("] which does not implement AppenderAttachable."));
          }
        }
      }
    }
    propSetter.activate();
    return appender;
    }
    /* Yes, it's ugly.  But all of these exceptions point to the same
  problem: we can't create an Appender */
    catch (Exception& oops)
  {
    LogLog::error(_T("Could not create an Appender. Reported error follows."),
      oops);
    return 0;
    }
}

/**
Used internally to parse an {@link ErrorHandler} element.
*/
void DOMConfigurator::parseErrorHandler(XMLDOMElementPtr element, AppenderPtr appender)
{
    ErrorHandlerPtr eh = OptionConverter::instantiateByClassName(
    subst(element->getAttribute(CLASS_ATTR)),
    ErrorHandler::getStaticClass(), 
    0);

    if(eh != 0)
  {
    eh->setAppender(appender);
    
    PropertySetter propSetter(eh);
    XMLDOMNodeListPtr children = element->getChildNodes();
    int length = children->getLength();
    
    for (int loop = 0; loop < length; loop++) 
    {
      XMLDOMNodePtr currentNode = children->item(loop);
      if (currentNode->getNodeType() == XMLDOMNode::ELEMENT_NODE)
      {
        XMLDOMElementPtr currentElement = currentNode;
        String tagName = currentElement->getTagName();
        if(tagName == PARAM_TAG)
        {
          setParameter(currentElement, propSetter);
        }
        else if(tagName == APPENDER_REF_TAG)
        {
          eh->setBackupAppender(findAppenderByReference(currentElement));
        }
        else if(tagName == LOGGER_REF)
        {
          String loggerName = currentElement->getAttribute(REF_ATTR);      
          LoggerPtr logger = repository->getLogger(loggerName, loggerFactory);
          eh->setLogger(logger);
        }
        else if(tagName == ROOT_REF)
        {
          LoggerPtr root = repository->getRootLogger();
          eh->setLogger(root);
        }
      }
    }
    propSetter.activate();
    appender->setErrorHandler(eh);
    }
}

/**
 Used internally to parse a filter element.
*/
void DOMConfigurator::parseFilters(XMLDOMElementPtr element, AppenderPtr appender)
{
  String clazz = subst(element->getAttribute(CLASS_ATTR));
  FilterPtr filter = OptionConverter::instantiateByClassName(clazz,
    Filter::getStaticClass(), 0);
  
  if(filter != 0)
  {
    PropertySetter propSetter(filter);
    XMLDOMNodeListPtr children = element->getChildNodes();
    int length = children->getLength();
    
    for (int loop = 0; loop < length; loop++)
    {
      XMLDOMNodePtr currentNode = children->item(loop);
      if (currentNode->getNodeType() == XMLDOMNode::ELEMENT_NODE)
      {
        XMLDOMElementPtr currentElement = currentNode;
        String tagName = currentElement->getTagName();
        if(tagName == PARAM_TAG)
        {
          setParameter(currentElement, propSetter);
        } 
      }
    }
    propSetter.activate();
    LogLog::debug(_T("Adding filter of type [")+filter->getClass().toString()
      +_T("] to appender named [")+appender->getName()+_T("]."));
    appender->addFilter(filter);
  }    
}

/**
Used internally to parse an category element.
*/
void DOMConfigurator::parseLogger(XMLDOMElementPtr loggerElement)
{
  // Create a new org.apache.log4j.Category object from the <category> element.
  String loggerName = subst(loggerElement->getAttribute(NAME_ATTR));
  
  LogLog::debug(_T("Retreiving an instance of Logger."));
  LoggerPtr logger = repository->getLogger(loggerName, loggerFactory);

  // Setting up a category needs to be an atomic operation, in order
  // to protect potential log operations while category
  // configuration is in progress.
  synchronized sync(logger);
  bool additivity = OptionConverter::toBoolean(
    subst(loggerElement->getAttribute(ADDITIVITY_ATTR)),
    true);
  
  LogLog::debug(_T("Setting [")+logger->getName()+_T("] additivity to [")+
    (additivity ? String(_T("true")) : String(_T("false")))+_T("]."));
  logger->setAdditivity(additivity);
  parseChildrenOfLoggerElement(loggerElement, logger, false);
}

/**
 Used internally to parse the logger factory element.
*/
void DOMConfigurator::parseLoggerFactory(XMLDOMElementPtr factoryElement)
{
  String className = subst(factoryElement->getAttribute(CLASS_ATTR));
  
  if(className.empty())
  {
    LogLog::error(_T("Logger Factory tag ") + String(CLASS_ATTR) +
      _T(" attribute not found."));
    LogLog::debug(_T("No Category Logger configured."));
  }
  else
  {
    LogLog::debug(_T("Desired logger factory: [")+className+_T("]"));
    loggerFactory = OptionConverter::instantiateByClassName(
      className, 
      LoggerFactory::getStaticClass(), 
      0);
    PropertySetter propSetter(loggerFactory);
    
    XMLDOMElementPtr currentElement = 0;
    XMLDOMNodePtr currentNode = 0;
    XMLDOMNodeListPtr children = factoryElement->getChildNodes();
    int length = children->getLength();
    
    for (int loop=0; loop < length; loop++)
    {
      currentNode = children->item(loop);
      if (currentNode->getNodeType() == XMLDOMNode::ELEMENT_NODE)
      {
        currentElement = currentNode;
        if (currentElement->getTagName() == PARAM_TAG) 
        {
          setParameter(currentElement, propSetter);
        }
      }
    }
  }
}

/**
 Used internally to parse the roor category element.
*/
void DOMConfigurator::parseRoot(XMLDOMElementPtr rootElement)
{
  LoggerPtr root = repository->getRootLogger();
  // category configuration needs to be atomic
  synchronized sync(root);
  parseChildrenOfLoggerElement(rootElement, root, true);
}

/**
 Used internally to parse the children of a logger element.
*/
void DOMConfigurator::parseChildrenOfLoggerElement(
  XMLDOMElementPtr loggerElement, LoggerPtr logger, bool isRoot)
{
    
    PropertySetter propSetter(logger);
    
    // Remove all existing appenders from logger. They will be
    // reconstructed if need be.
    logger->removeAllAppenders();
  
  
    XMLDOMNodeListPtr children = loggerElement->getChildNodes();
    int length = children->getLength();
    
    for (int loop = 0; loop < length; loop++)
  {
    XMLDOMNodePtr currentNode = children->item(loop);
    
    if (currentNode->getNodeType() == XMLDOMNode::ELEMENT_NODE)
    {
      XMLDOMElementPtr currentElement = currentNode;
      String tagName = currentElement->getTagName();
      
      if (tagName == APPENDER_REF_TAG)
      {
        AppenderPtr appender = findAppenderByReference(currentElement);
        String refName =  subst(currentElement->getAttribute(REF_ATTR));
        if(appender != 0)
        {
          LogLog::debug(_T("Adding appender named [")+ refName+ 
          _T("] to logger [")+logger->getName()+_T("]."));
        }
        else 
        {
          LogLog::debug(_T("Appender named [")+ refName +
            _T("] not found."));
        }
        
        logger->addAppender(appender);
        
      } 
      else if(tagName == LEVEL_TAG)
      {
        parseLevel(currentElement, logger, isRoot);
      } 
      else if(tagName == PRIORITY_TAG)
      {
        parseLevel(currentElement, logger, isRoot);
      } 
      else if(tagName == PARAM_TAG)
      {
        setParameter(currentElement, propSetter);
      }
    }
    }
    propSetter.activate();
}

/**
 Used internally to parse a layout element.
*/  
LayoutPtr DOMConfigurator::parseLayout (XMLDOMElementPtr layout_element)
{
  String className = subst(layout_element->getAttribute(CLASS_ATTR));
  LogLog::debug(_T("Parsing layout of class: \"")+className+_T("\""));     
  try 
  {
    ObjectPtr instance = Loader::loadClass(className).newInstance();
    LayoutPtr layout = instance;
    PropertySetter propSetter(layout);
    
    XMLDOMNodeListPtr params   = layout_element->getChildNodes();
    int length   = params->getLength();
    
    for (int loop = 0; loop < length; loop++)
    {
      XMLDOMNodePtr currentNode = params->item(loop);
      if (currentNode->getNodeType() == XMLDOMNode::ELEMENT_NODE)
      {
        XMLDOMElementPtr currentElement = currentNode;
        String tagName = currentElement->getTagName();
        if(tagName == PARAM_TAG)
        {
          setParameter(currentElement, propSetter);
        }
      }
    }
    
    propSetter.activate();
    return layout;
  }
  catch (Exception& oops)
  {
    LogLog::error(_T("Could not create the Layout. Reported error follows."),
      oops);
    return 0;
  }
}

/**
 Used internally to parse a level  element.
*/
void DOMConfigurator::parseLevel(XMLDOMElementPtr element, LoggerPtr logger, bool isRoot)
{
    String loggerName = logger->getName();
    if(isRoot) 
  {
    loggerName = _T("root");
    }
  
    String levelStr = subst(element->getAttribute(VALUE_ATTR));
  LogLog::debug(_T("Level value for ")+loggerName+_T(" is [")+levelStr+_T("]."));
    
    if(levelStr == INHERITED || levelStr == NuLL)
  {
    if(isRoot)
    {
      LogLog::error(_T("Root level cannot be inherited. Ignoring directive."));
    } 
    else
    {
      logger->setLevel(0);
    }
    } 
  else
  {
    String className = subst(element->getAttribute(CLASS_ATTR));

    if (className.empty())
    {
      logger->setLevel(OptionConverter::toLevel(levelStr, Level::DEBUG));
    }
    else
    {
      LogLog::debug(_T("Desired Level sub-class: [") + className + _T("]"));

      try
      {
        Level::LevelClass& levelClass =
          (Level::LevelClass&)Loader::loadClass(className);
        LevelPtr level = levelClass.toLevel(levelStr);
        logger->setLevel(level);
      }
      catch (Exception& oops)
      {
        LogLog::error(
          _T("Could not create level [") + levelStr +
          _T("]. Reported error follows."),
          oops);

        return;
      }
      catch (...)
      {
        LogLog::error(
          _T("Could not create level [") + levelStr);

        return;
      }
    }
    }

  LogLog::debug(loggerName + _T(" level set to ") + logger->getLevel()->toString());    
}

void DOMConfigurator::setParameter(XMLDOMElementPtr elem, PropertySetter& propSetter)
{
  String name = subst(elem->getAttribute(NAME_ATTR));
  String value = elem->getAttribute(VALUE_ATTR);
  value = subst(OptionConverter::convertSpecialChars(value));
  propSetter.setProperty(name, value);
}

void DOMConfigurator::doConfigure(const String& filename, spi::LoggerRepositoryPtr& repository)
{
  this->repository = repository;
  LogLog::debug(_T("DOMConfigurator configuring file ") + filename + _T("..."));

  appenderBag = new AppenderMap();
  loggerFactory = new DefaultCategoryFactory();

  try
  {
#ifdef HAVE_LIBXML2
    XMLDOMDocumentPtr doc = new GnomeXMLDOMDocument();
#elif defined(HAVE_MS_XML)
    XMLDOMDocumentPtr doc = new MsXMLDOMDocument();
#endif
    doc->load(filename); 
    parse(doc->getDocumentElement());
    }
  catch (Exception& e)
  {
    // I know this is miserable...
    LogLog::error(_T("Could not parse input source [")+filename+_T("]."), e);
    }

  delete (AppenderMap *)appenderBag;
}

void DOMConfigurator::configure(const String& filename)
{
  DOMConfigurator().doConfigure(filename, LogManager::getLoggerRepository());
}

void DOMConfigurator::configureAndWatch(const String& configFilename)
{
  configureAndWatch(configFilename, FileWatchdog::DEFAULT_DELAY);
}

void DOMConfigurator::configureAndWatch(const String& configFilename, long delay)
{
  XMLWatchdog * xdog = new XMLWatchdog(configFilename);
  xdog->setDelay(delay);
  xdog->start();
}

/**
 Used internally to configure the log4j framework by parsing a DOM
 tree of XML elements based on <a
 href="doc-files/log4j.dtd">log4j.dtd</a>.
 
*/
void DOMConfigurator::parse(XMLDOMElementPtr element)
{
    String rootElementName = element->getTagName();
  
    if (rootElementName != CONFIGURATION_TAG)
  {
    if(rootElementName == OLD_CONFIGURATION_TAG)
    {
      //LogLog::warn(_T("The <")+String(OLD_CONFIGURATION_TAG)+
      //  _T("> element has been deprecated."));
      //LogLog::warn(_T("Use the <")+String(CONFIGURATION_TAG)+
      //  _T("> element instead."));
    } 
    else
    {
      LogLog::error(_T("DOM element is - not a <")+
        String(CONFIGURATION_TAG)+_T("> element."));
      return;
    }
    }
  
    String debugAttrib = subst(element->getAttribute(INTERNAL_DEBUG_ATTR));

    LogLog::debug(_T("debug attribute= \"") + debugAttrib +_T("\"."));
    // if the log4j.dtd is not specified in the XML file, then the
    // "debug" attribute is returned as the empty string.
    if(!debugAttrib.empty() && debugAttrib != NuLL) 
  {      
    LogLog::setInternalDebugging(OptionConverter::toBoolean(debugAttrib, true));
    } 
  else 
  {
    LogLog::debug(_T("Ignoring ") + String(INTERNAL_DEBUG_ATTR)
      + _T(" attribute."));
    }
  
  
    String confDebug = subst(element->getAttribute(CONFIG_DEBUG_ATTR));
    if(!confDebug.empty() && confDebug != NuLL) 
  {      
    LogLog::warn(_T("The \"")+String(CONFIG_DEBUG_ATTR)+
      _T("\" attribute is deprecated."));
    LogLog::warn(_T("Use the \"")+String(INTERNAL_DEBUG_ATTR)+
      _T("\" attribute instead."));
    LogLog::setInternalDebugging(OptionConverter::toBoolean(confDebug, true));
    }
  
    String thresholdStr = subst(element->getAttribute(THRESHOLD_ATTR));
    LogLog::debug(_T("Threshold =\"") + thresholdStr +_T("\"."));
    if(!thresholdStr.empty() && thresholdStr != NuLL)
  {
    repository->setThreshold(thresholdStr);
    }

    String tagName;
    XMLDOMElementPtr currentElement;
    XMLDOMNodePtr currentNode;
    XMLDOMNodeListPtr children = element->getChildNodes();
    int length = children->getLength();
  int loop;

    for (loop = 0; loop < length; loop++)
  {
    currentNode = children->item(loop);
    if (currentNode->getNodeType() == XMLDOMNode::ELEMENT_NODE)
    {
      currentElement = currentNode;
      tagName = currentElement->getTagName();
      
      if (tagName == CATEGORY_FACTORY_TAG)
      {
        parseLoggerFactory(currentElement);
      }
    }
    }
    
    for (loop = 0; loop < length; loop++)
  {
    currentNode = children->item(loop);
    if (currentNode->getNodeType() == XMLDOMNode::ELEMENT_NODE)
    {
      currentElement =  currentNode;
      tagName = currentElement->getTagName();
      
      if (tagName == CATEGORY || tagName == LOGGER)
      {
        parseLogger(currentElement);
      }
      else if (tagName == ROOT_TAG)
      {
        parseRoot(currentElement);
      }
    }
    }
}

String DOMConfigurator::subst(const String& value)
{
    try
  {
    return OptionConverter::substVars(value, props);
    } 
  catch(IllegalArgumentException& e)
  {
    LogLog::warn(_T("Could not perform variable substitution."), e);
    return value;
    }
}

#endif // HAVE_XML
