#include "configurationParser.h"

namespace configuration
{

/**
 * Create a new Parser and return a pointer to it.
 */
Parser::Pointer
Parser::New()
{
  return new Self;
}


/**
 * Constructor sets up an XML_Parser to make call-backs into this
 * Parser.
 */
Parser
::Parser():
  m_XML_Parser(XML_ParserCreate(NULL)),
  m_Package(NULL),
  m_CdataSectionFlag(false)
{
  Parser::InitializeHandlers();
  
  XML_SetElementHandler(m_XML_Parser,
                        BeginElement_proxy,
                        EndElement_proxy);
  XML_SetUserData(m_XML_Parser,
                  this);
  XML_SetCdataSectionHandler(m_XML_Parser,
                             BeginCdataSectionHandler_proxy,
                             EndCdataSectionHandler_proxy);
  XML_SetCharacterDataHandler(m_XML_Parser,
                              CharacterDataHandler_proxy);
}


/**
 * Cleanup the XML_Parser
 */
Parser
::~Parser()
{
  XML_ParserFree(m_XML_Parser);
}


/**
 * Parse the XML from the given input stream until end-of-input is reached.
 */
void
Parser
::Parse(std::istream& inStream)
{
  char buf[257];
  bool done = false;

  /**
   * Parse until end of input stream is reached.
   */
  while(!done)
    {
    inStream.read(buf, sizeof(buf));
    size_t len = inStream.gcount();
    done = (len < sizeof(buf));
    if(!XML_Parse(m_XML_Parser, buf, len, done))
      {
      std::cerr << "Parser::Parse(): \""
                << XML_ErrorString(XML_GetErrorCode(m_XML_Parser))
                << "\" at source line "
                << XML_GetCurrentLineNumber(m_XML_Parser)
                << std::endl;
      }
    }
}


/**
 * Called when a new element is opened in the XML source.
 * Checks the tag name, and calls the appropriate handler with an
 * Attributes container.
 */
void
Parser
::BeginElement(const char *name, const char **atts)
{
  try {
  // Try to look up the handler for this element.
  BeginHandlers::iterator handler = beginHandlers.find(name);
  if(handler != beginHandlers.end())
    {
    // Found one, call it.
    Attributes attributes;
    for(int i=0; atts[i] && atts[i+1] ; i += 2)
      {
      attributes.Set(atts[i], atts[i+1]);
      }
    (this->*(handler->second))(attributes);
    }
  else
    {
    throw UnknownElementTagException(name);
    }
  }
  catch (const ParseException& e)
    {
    e.PrintLocation(std::cerr, m_XML_Parser, "configuration");
    e.Print(std::cerr);
    }
  catch (const String& e)
    {
    std::cerr << "Caught exception in Parser::BeginElement():"
              << std::endl << e.c_str() << std::endl;
    }
  catch (...)
    {
    std::cerr << "Caught unknown exception in Parser::BeginElement()."
              << std::endl;
    }
}


/**
 * Called at the end of an element in the XML source opened when
 * BeginElement was called.
 */
void
Parser
::EndElement(const char *name)
{
  try {
  // Try to look up the handler for this element.
  EndHandlers::iterator handler = endHandlers.find(name);
  if(handler != endHandlers.end())
    {
    // Found one, call it.
    (this->*(handler->second))();
    }
  else
    {
    throw UnknownElementTagException(name);
    }
  }
  catch (const ParseException& e)
    {
    e.PrintLocation(std::cerr, m_XML_Parser, "configuration");
    e.Print(std::cerr);
    }
  catch (const String& e)
    {
    std::cerr << "Caught exception in Parser::EndElement():"
              << std::endl << e.c_str() << std::endl;
    }
  catch (...)
    {
    std::cerr << "Caught unknown exception in Parser::EndElement()."
              << std::endl;
    }
}


/**
 * Set the flag to indicate whether we are currently parsing a CDATA
 * section from the XML source.
 */
void
Parser
::BeginCdataSectionHandler()
{
  m_CdataSectionFlag = true;
}


/**
 * Get the flag to indicate whether we are currently parsing a CDATA
 * section from the XML source.
 */
void
Parser
::EndCdataSectionHandler()
{
  m_CdataSectionFlag = false;
}


/**
 * When an XML character data segment is encountered, this is called
 * to handle it.
 *
 * Such a segment is of the form:
 * <![CDATA[....]]>
 * or just
 * ....
 * "data" will be ...., one line at a time.
 * "data" will NOT be '\0' terminated, but "length" specifies how much.
 */
void
Parser
::CharacterDataHandler(const XML_Char *data, int length)
{
  this->CurrentElement()->AddCharacterData(data, length, m_CdataSectionFlag);
}


/**
 * Get the top of the element stack.
 */
ConfigureObject::Pointer
Parser
::CurrentElement()
{
  return m_ElementStack.top();
}


/**
 * An unexpected element type is on top of the stack.
 */
class ElementStackTypeException: public ParseException
{
public:
  ElementStackTypeException(const char* e, const char* t):
    ParseException(), m_Expected(e), m_Got(t) {}
  void Print(std::ostream& os) const
    {
      os << "Expected \"" << m_Expected.c_str()
         << "\" as immediate enclosing element, but got \"" << m_Got << "\""
         << std::endl;
    }
private:
  String m_Expected;
  String m_Got;
};


/**
 * Get the current Package off the top of the element stack.
 */
Package::Pointer
Parser
::CurrentPackage()
{
  if(!m_ElementStack.top()->IsPackage())
    throw ElementStackTypeException("Package",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Package*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current Dependencies off the top of the element stack.
 */
Dependencies::Pointer
Parser
::CurrentDependencies()
{
  if(!m_ElementStack.top()->IsDependencies())
    throw ElementStackTypeException("Dependencies",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Dependencies*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current CodeBlock off the top of the element stack.
 */
CodeBlock::Pointer
Parser
::CurrentCodeBlock()
{
  if(!m_ElementStack.top()->IsCodeBlock())
    throw ElementStackTypeException("CodeBlock",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<CodeBlock*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current ArgumentSet off the top of the element stack.
 */
ArgumentSet::Pointer
Parser
::CurrentArgumentSet()
{
  if(!m_ElementStack.top()->IsArgumentSet())
    throw ElementStackTypeException("ArgumentSet",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<ArgumentSet*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current Argument off the top of the element stack.
 */
Argument::Pointer
Parser
::CurrentArgument()
{
  if(!m_ElementStack.top()->IsArgument())
    throw ElementStackTypeException("Argument",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Argument*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current Headers off the top of the element stack.
 */
Headers::Pointer
Parser
::CurrentHeaders()
{
  if(!m_ElementStack.top()->IsHeaders())
    throw ElementStackTypeException("Headers",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Headers*>(m_ElementStack.top().RealPointer());
}


/**
 * Push a new element onto the element stack.
 */
void
Parser
::PushElement(ConfigureObject* element)
{
  m_ElementStack.push(element);
}


/**
 * Pop the top off the element stack.
 */
void
Parser
::PopElement()
{
  m_ElementStack.pop();

  // Sanity check.
  if(m_ElementStack.empty())
    {
    throw String("Main package popped from element stack!");
    }
}


/**
 * Begin handler for Package element.
 */
void
Parser
::begin_Package(const Attributes& atts)
{
  String name = atts.Get("name");
  if(!m_Package)
    {
    // This is the outermost package.
    m_Package = Package::New(name);
    this->PushElement(m_Package);
    }
  else
    {
    // This is a package dependency element.
    // Add the dependency.
    this->CurrentDependencies()->Add(name);
    }
}

/**
 * End handler for Package element.
 */
void
Parser
::end_Package()
{
  // Don't pop off the main package element!
}


/**
 * Begin handler for Dependencies element.
 */
void
Parser
::begin_Dependencies(const Attributes&)
{
  Package* package = this->CurrentPackage();
  
  // See if the package already has a Dependencies instance.
  Dependencies::Pointer dependencies = package->GetDependencies();
  if(!dependencies)
    {
    // Need to create a new Dependencies and give it to the package.
    dependencies = Dependencies::New();
    package->SetDependencies(dependencies);
    }
  
  // Put the Dependencies on the stack so packages can be added.
  this->PushElement(dependencies);
}


/**
 * End handler for Dependencies element.
 */
void
Parser
::end_Dependencies()
{
  this->PopElement();
}


/**
 * Begin handler for CreateMethod element.
 */
void
Parser
::begin_CreateMethod(const Attributes& atts)
{
  String name = atts.Get("name");
  
  // Create a new CodeBlock to hold the lines of code.
  CodeBlock::Pointer newCodeBlock = CodeBlock::New();
  
  // Save the CodeBlock by this name.
  m_CreateMethods[name] = newCodeBlock;
  
  // Put new CodeBlock on the stack so it can be filled with lines.
  this->PushElement(newCodeBlock);
}


/**
 * End handler for CreateMethod element.
 */
void
Parser
::end_CreateMethod()
{
  // Take the CodeBlock off the stack.
  this->PopElement();
}


/**
 * Begin handler for DeleteMethod element.
 */
void
Parser
::begin_DeleteMethod(const Attributes& atts)
{
  String name = atts.Get("name");
  
  // Create a new CodeBlock to hold the lines of code.
  CodeBlock::Pointer newCodeBlock = CodeBlock::New();
  
  // Save the CodeBlock by this name.
  m_DeleteMethods[name] = newCodeBlock;
  
  // Put new CodeBlock on the stack so it can be filled with lines.
  this->PushElement(newCodeBlock);
}


/**
 * End handler for DeleteMethod element.
 */
void
Parser
::end_DeleteMethod()
{
  // Take the CodeBlock off the stack.
  this->PopElement();
}


/**
 * Begin handler for ArgumentSet element.
 */
void
Parser
::begin_ArgumentSet(const Attributes& atts)
{
  String name = atts.Get("name");
  
  if(this->CurrentElement()->IsArgumentSet())
    {
    // The ArgumentSet is being referenced inside another set.
    if(m_ArgumentSets.count(name) > 0)
      {
      // Copy all the arguments over to it.
      this->CurrentArgumentSet()->Add(m_ArgumentSets[name]);
      }
    else
      {
      // The referenced argument set does not exist.  Complain.
      std::cerr << "ArgumentSet \"" << name.c_str() << "\" does not exist!"
                << std::endl;
      }
    
    // Put a dummy element on the stack to be popped off by end_ArgumentSet().
    this->PushElement(0);
    }
  else
    {
    // Create a new ArgumentSet.
    ArgumentSet::Pointer newArgumentSet = ArgumentSet::New();
    
    // Save the ArgumentSet by this name.
    m_ArgumentSets[name] = newArgumentSet;
    
    // Put new ArgumentSet on the stack so it can be filled.
    this->PushElement(newArgumentSet);
    }
}


/**
 * End handler for ArgumentSet element.
 */
void
Parser
::end_ArgumentSet()
{
  if(this->CurrentElement())
    {
    // Print out the arguments defined.
    this->CurrentArgumentSet()->Print(std::cout);
    std::cout << "----end_ArgumentSet----" << std::endl;
    }
  
  // Take the ArgumentSet off the stack.
  this->PopElement();
}


/**
 * Begin handler for Argument element.
 */
void
Parser
::begin_Argument(const Attributes& atts)
{
  String tag = atts.Get("tag");
  
  // Create a new Argument.
  Argument::Pointer newArgument = Argument::New(tag);
  
  // Put new Argument on the stack so it can be filled with code.
  this->PushElement(newArgument);
}


/**
 * End handler for Argument element.
 */
void
Parser
::end_Argument()
{
  // Hold onto the finished Argument as it is popped off the stack.
  Argument::Pointer finishedArgument = this->CurrentArgument();
  
  // Take the Argument off the stack.
  this->PopElement();

  // Add the argument to the current ArgumentSet.
  this->GenerateArgumentCombinations(finishedArgument);
}


/**
 * Begin handler for Headers element.
 */
void
Parser
::begin_Headers(const Attributes& atts)
{
  Package* package = this->CurrentPackage();

  // See if the package already has a Headers instance.
  Headers::Pointer headers = package->GetHeaders();
  if(!headers)
    {
    // Need to create a new Headers and give it to the package.
    headers = Headers::New();
    package->SetHeaders(headers);
    }
  
  // Put the Headers on the stack so files and directories can be added.
  this->PushElement(headers);
}


/**
 * End handler for Headers element.
 */
void
Parser
::end_Headers()
{
  // Take the Headers off the stack.
  this->PopElement();
}


/**
 * Begin handler for File element.
 */
void
Parser
::begin_File(const Attributes& atts)
{
  String name = atts.Get("name");

  // Add the file to the current set of headers.
  this->CurrentHeaders()->AddFile(name);
}


/**
 * End handler for File element.
 */
void
Parser
::end_File()
{
}


/**
 * Begin handler for Directory element.
 */
void
Parser
::begin_Directory(const Attributes& atts)
{
  String name = atts.Get("name");
  
  // Add the directory to the current set of headers.
  this->CurrentHeaders()->AddDirectory(name);
}


/**
 * End handler for Directory element.
 */
void
Parser
::end_Directory()
{
}


/**
 * Map of Parser element begin handlers.
 */
Parser::BeginHandlers Parser::beginHandlers;

/**
 * Map of Parser element end handlers.
 */
Parser::EndHandlers Parser::endHandlers;

/**
 * This static method initializes the beginHandlers and endHandlers
 * maps.  It is called by the Parser constructor every time,
 * but only initializes the maps once.
 */
void
Parser
::InitializeHandlers()
{
  // Make sure we only initialize the maps once.
  static bool initialized = false;  
  if(initialized) return;
  
  beginHandlers["Package"]      = &Parser::begin_Package;
  beginHandlers["Dependencies"] = &Parser::begin_Dependencies;
  beginHandlers["CreateMethod"] = &Parser::begin_CreateMethod;
  beginHandlers["DeleteMethod"] = &Parser::begin_DeleteMethod;
  beginHandlers["ArgumentSet"]  = &Parser::begin_ArgumentSet;
  beginHandlers["Argument"]     = &Parser::begin_Argument;
  beginHandlers["Headers"]      = &Parser::begin_Headers;
  beginHandlers["File"]         = &Parser::begin_File;
  beginHandlers["Directory"]    = &Parser::begin_Directory;

  endHandlers["Package"]      = &Parser::end_Package;
  endHandlers["Dependencies"] = &Parser::end_Dependencies;
  endHandlers["CreateMethod"] = &Parser::end_CreateMethod;
  endHandlers["DeleteMethod"] = &Parser::end_DeleteMethod;
  endHandlers["ArgumentSet"]  = &Parser::end_ArgumentSet;
  endHandlers["Argument"]     = &Parser::end_Argument;
  endHandlers["Headers"]      = &Parser::end_Headers;
  endHandlers["File"]         = &Parser::end_File;
  endHandlers["Directory"]    = &Parser::end_Directory;
  
  initialized = true;
}


/**
 * Passes call through to real parser object according to first argument.
 */
void
Parser
::BeginElement_proxy(void* parser, const char *name, const char **atts)
{
  static_cast<Parser*>(parser)->BeginElement(name, atts);
}


/**
 * Passes call through to real parser object according to first argument.
 */
void
Parser
::EndElement_proxy(void* parser, const char *name)
{
  static_cast<Parser*>(parser)->EndElement(name);
}


/**
 * Passes call through to real parser object according to first argument.
 */
void
Parser
::CharacterDataHandler_proxy(void* parser,const XML_Char *data, int length)
{
  static_cast<Parser*>(parser)->CharacterDataHandler(data, length);
}


/**
 * Passes call through to real parser object according to first argument.
 */
void
Parser
::BeginCdataSectionHandler_proxy(void* parser)
{
  static_cast<Parser*>(parser)->BeginCdataSectionHandler();
}


/**
 * Passes call through to real parser object according to first argument.
 */
void
Parser
::EndCdataSectionHandler_proxy(void* parser)
{
  static_cast<Parser*>(parser)->EndCdataSectionHandler();
}


class NamePortion
{
public:
  virtual const String& GetPortion() const =0;
  virtual ~NamePortion() {}
};


class StringNamePortion: public NamePortion
{
public:
  StringNamePortion(const String& in_string): m_String(in_string) {}
  virtual const String& GetPortion() const
    {
      return m_String;
    }
  virtual ~StringNamePortion() {}
private:
  String m_String;
};

class ReplaceNamePortion: public NamePortion
{
public:
  ReplaceNamePortion(const String& in_string): m_String(in_string) {}
  virtual const String& GetPortion() const
    {
      return m_String;
    }
  virtual ~ReplaceNamePortion() {}
private:
  const String& m_String;
};


/**
 * Given an Argument, generate all the combinations of ArgumentSet
 * substitutions possbile based on $ tokens in the Argument's code.
 */
void
Parser
::GenerateArgumentCombinations(const Argument* in_argument)
{
  typedef std::list<NamePortion*>  Portions;
  typedef std::map<String, String*>  Substitutions;
  Portions      portions;
  Substitutions substitutions;

  String in_string = in_argument->GetCode();
  
  String s;
  for(String::const_iterator c=in_string.begin(); c != in_string.end(); ++c)
    {
    if(*c != '$')
      {
      s.insert(s.end(), *c);
      }
    else
      {
      if(s.length() > 0)
        {
        portions.push_back(new StringNamePortion(s));
        s = "";
        }
      // Get argument set name token.
      ++c;
      while(c != in_string.end())
        {
        char ch = *c;
        if(((ch >= 'a') && (ch <= 'z'))
           || ((ch >= 'A') && (ch <= 'Z'))
           || ((ch >= '0') && (ch <= '9'))
           || (ch == '_'))
          {
          s.insert(s.end(), ch);
          ++c;
          }
        else
          {
          break;
          }
        }
      if(s.length() > 0)
        {
        String* sub;
        if(substitutions.count(s) == 0)
          {
          sub = new String();
          substitutions[s] = sub;
          }
        else
          {
          sub = substitutions[s];
          }
        portions.push_back(new ReplaceNamePortion(*sub));
        s = "";
        }
      else
        {
        // Error: No token after $
        }
      if(c != in_string.end())
        s.insert(s.end(), *c);
      }
    }
  
  if(s.length() > 0)
    {
    portions.push_back(new StringNamePortion(s));
    }

  // If there are no substitutions to be made, just generate this
  // single combination.
  if(substitutions.empty())
    {
    this->CurrentArgumentSet()->Add(in_argument->GetTag(),
                                    in_argument->GetCode());
    return;
    }
  
  // We must generate all combinations of argument substitutions.
  
  typedef std::pair<ArgumentSet::ConstIterator,
                    ArgumentSet::ConstIterator>  ArgumentSetIteratorPair;
  std::list<ArgumentSetIteratorPair> argumentSets;
  Substitutions::const_iterator substitution = substitutions.begin();
  
  bool done = false;
  while(!done)
    {
    if(substitution == substitutions.end())
      {
      // We are at the end of the list of substitutions.
      if(argumentSets.back().first != argumentSets.back().second)
        {
        --substitution;
        *(substitution->second) = argumentSets.back().first->second;
        ++substitution;
        // Generate this combination.
        String s;
        for(Portions::const_iterator i = portions.begin();
            i != portions.end(); ++i)
          s.append((*i)->GetPortion());
        std::cout << s.c_str() << std::endl;
        
        // Increment to the next argument in this set.
        ++(argumentSets.back().first);
        }
      else
        {
        // We have finished this set of combinations.
        // Walk back through the list until we find an unfinished set
        // of arguments.
        while(argumentSets.back().first == argumentSets.back().second)
          {
          argumentSets.pop_back();
          --substitution;
          if(argumentSets.empty())
            {
            // We have finished all combinations.
            done = true;
            break;
            }
          ++(argumentSets.back().first);
          }
        if(!done)
          {
          --substitution;
          *(substitution->second) = argumentSets.back().first->second;
          ++substitution;
          }
        }
      }
    else
      {
      // We are not at the end of the list of substitutions.
      // Move on to the next one.
      if(m_ArgumentSets.count(substitution->first) == 0)
        {
        std::cerr << "Cannot find ArgumentSet \""
                  << substitution->first.c_str() << "\"" << std::endl;
        return;
        }
      argumentSets.push_back(
        ArgumentSetIteratorPair(m_ArgumentSets[substitution->first]->Begin(),
                                m_ArgumentSets[substitution->first]->End()));
      *(substitution->second) = argumentSets.back().first->second;
      ++substitution;
      }
    }
  
  // Free the string portions that were allocated.
  for(Portions::iterator i=portions.begin(); i != portions.end(); ++i)
    delete *i;
  
  // Free the substitutions that were allocated.
  for(Substitutions::iterator i = substitutions.begin();
      i != substitutions.end(); ++i)
    delete i->second;
}

} // namespace configuration

