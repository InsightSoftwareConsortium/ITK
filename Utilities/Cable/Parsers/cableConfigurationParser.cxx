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
  this->TopParseElement()->AddCharacterData(data, length, m_CdataSectionFlag);
}


/**
 * Get the top of the element stack.
 */
ConfigureObject::Pointer
Parser
::TopParseElement()
{
  return m_ElementStack.top();
}


/**
 * An unknown Set has been referenced with a $ token.
 */
class UnknownSetException: public ParseException
{
public:
  UnknownSetException(const String& unknown):
    ParseException(), m_Unknown(unknown) {}
  void Print(std::ostream& os) const
    {
      os << "Unknown Set \"" << m_Unknown.c_str() << "\""
         << std::endl;
    }
private:
  String m_Unknown;
};


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
 * Get the current Set off the top of the element stack.
 */
Set::Pointer
Parser
::CurrentSet()
{
  if(!m_ElementStack.top()->IsSet()
     && !m_ElementStack.top()->IsWrapperSet())
    throw ElementStackTypeException("Set",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Set*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current Element off the top of the element stack.
 */
Element::Pointer
Parser
::CurrentElement()
{
  if(!m_ElementStack.top()->IsElement())
    throw ElementStackTypeException("Element",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Element*>(m_ElementStack.top().RealPointer());
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
 * Begin handler for Set element.
 */
void
Parser
::begin_Set(const Attributes& atts)
{
  String name = atts.Get("name");
  
  if(this->TopParseElement()->IsSet()
     || this->TopParseElement()->IsWrapperSet())
    {
    // The Set is being referenced inside another set.
    if(m_Sets.count(name) > 0)
      {
      // Copy all the elements over to it.
      this->CurrentSet()->Add(m_Sets[name]);
      }
    else
      {
      // The referenced element set does not exist.  Complain.
      throw UnknownSetException(name);
      }
    
    // Put a dummy element on the stack to be popped off by end_Set().
    this->PushElement(0);
    }
  else
    {
    // Create a new Set.
    Set::Pointer newSet = Set::New();
    
    // Save the Set by this name.
    m_Sets[name] = newSet;
    
    // Put new Set on the stack so it can be filled.
    this->PushElement(newSet);
    }
}


/**
 * End handler for Set element.
 */
void
Parser
::end_Set()
{
  if(this->TopParseElement())
    {
    std::cout << "----begin Set----" << std::endl;
    // Print out the elements defined.
    this->CurrentSet()->Print(std::cout);
    std::cout << "----end Set----" << std::endl;
    }
  
  // Take the Set off the stack.
  this->PopElement();
}


/**
 * Begin handler for Element element.
 */
void
Parser
::begin_Element(const Attributes& atts)
{
  String tag = atts.Get("tag");
  
  // Create a new Element.
  Element::Pointer newElement = Element::New(tag);
  
  // Put new Element on the stack so it can be filled with code.
  this->PushElement(newElement);
}


/**
 * End handler for Element element.
 */
void
Parser
::end_Element()
{
  // Hold onto the finished Element as it is popped off the stack.
  Element::Pointer finishedElement = this->CurrentElement();
  
  // Take the Element off the stack.
  this->PopElement();

  // Add the element to the current Set.
  this->GenerateElementCombinations(finishedElement,
                                    this->CurrentSet().RealPointer());
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
 * Begin handler for WrapperSet element.
 */
void
Parser
::begin_WrapperSet(const Attributes&)
{
  // Create a new WrapperSet.
  WrapperSet::Pointer newWrapperSet = WrapperSet::New();
    
  // Save the WrapperSet.
  //m_WrapperSets.insert(newWrapperSet);
    
  // Put new WrapperSet on the stack so it can be filled.
  this->PushElement(newWrapperSet);
}


/**
 * End handler for WrapperSet element.
 */
void
Parser
::end_WrapperSet()
{
  if(this->TopParseElement())
    {
    std::cout << "----begin WrapperSet----" << std::endl;
    // Print out the elements defined.
    this->CurrentSet()->Print(std::cout);
    std::cout << "----end WrapperSet----" << std::endl;
    }
  
  // Take the WrapperSet off the stack.
  this->PopElement();
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
  beginHandlers["Set"]          = &Parser::begin_Set;
  beginHandlers["Element"]      = &Parser::begin_Element;
  beginHandlers["Headers"]      = &Parser::begin_Headers;
  beginHandlers["File"]         = &Parser::begin_File;
  beginHandlers["Directory"]    = &Parser::begin_Directory;
  beginHandlers["WrapperSet"]   = &Parser::begin_WrapperSet;

  endHandlers["Package"]      = &Parser::end_Package;
  endHandlers["Dependencies"] = &Parser::end_Dependencies;
  endHandlers["CreateMethod"] = &Parser::end_CreateMethod;
  endHandlers["DeleteMethod"] = &Parser::end_DeleteMethod;
  endHandlers["Set"]          = &Parser::end_Set;
  endHandlers["Element"]      = &Parser::end_Element;
  endHandlers["Headers"]      = &Parser::end_Headers;
  endHandlers["File"]         = &Parser::end_File;
  endHandlers["Directory"]    = &Parser::end_Directory;
  endHandlers["WrapperSet"]   = &Parser::end_WrapperSet;
  
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


/**
 * Represent a substitution for Parser::GenerateElementCombinations().
 * A ReplacePortion refers to an instance of this class.  As
 * GenerateElementCombinations() iterates through a Set of elements for
 * substitution, it sets this instance to represent a each, one at a time.
 */
class Substitution
{
public:
  Substitution() {}
  Substitution(const String& in_tag, const String& in_code):
    m_Tag(in_tag), m_Code(in_code) {}
  void Set(const String& in_tag, const String& in_code)
    {
      m_Tag = in_tag;
      m_Code = in_code;
    }
  const String& GetTag() const
    { return m_Tag; }
  const String& GetCode() const
    { return m_Code; }
  
private:
  String m_Tag;
  String m_Code;
};


/**
 * Interface to the parts of an input string of code, possibly with
 * $SomeSetName tokens in it.  An indivitual Portion will be either a
 * StringPortion, which has no substitutions, or a ReplacePortion, which has
 * only a substitution, and no hard-coded text.
 *
 * This is used by Parser::GenerateElementCombinations() to hold the pieces
 * of a string after the set substitution tokens have been extracted.
 */
class Portion
{
public:
  /**
   * Get the C++ code corresponding to this Portion of a string.
   */
  virtual String GetCode() const =0;
  /**
   * Get the tag corresponding to this Portion of a string.  This is empty
   * for StringPortion, and holds a real tag for ReplacePortion.
   */
  virtual String GetTag() const =0;
  virtual ~Portion() {}
};


/**
 * Represent a hard-coded part of an input string, that has no substitutions
 * in it.  The tag for this part of a string is always empty.
 */
class StringPortion: public Portion
{
public:
  StringPortion(const String& in_code): m_Code(in_code) {}
  virtual String GetCode() const
    { return m_Code; }
  virtual String GetTag() const
    { return ""; }
  virtual ~StringPortion() {}
private:
  /**
   * Hold this Portion's contribution to the output string.
   */
  String m_Code;
};


/**
 * Represent the "$SomeSetName" portion of an input string.  This has a
 * reference to the Substitution holding the real output to generate.
 */
class ReplacePortion: public Portion
{
public:
  ReplacePortion(const Substitution& in_substitution):
    m_Substitution(in_substitution) {}
  virtual String GetCode() const
    { return m_Substitution.GetCode(); }
  virtual String GetTag() const
    { return m_Substitution.GetTag(); }
  virtual ~ReplacePortion() {}
private:
  /**
   * Refer to the real Substitution for this Portion's contribution.
   */
  const Substitution& m_Substitution;
};


/**
 * Given an Element, generate all the combinations of Set
 * substitutions possbile based on $ tokens in the Element's code.
 */
void
Parser
::GenerateElementCombinations(const Element* in_element,
                              Set* out_set)
{
  typedef std::list<Portion*>  Portions;
  typedef std::map<String, Substitution* >  Substitutions;
  Portions      portions;
  Substitutions substitutions;
  
  // The input code supplied in the source file.
  String in_string = in_element->GetCode();
  
  // Break the input code into blocks alternating between literal code and
  // set-substitution tokens (like $SomeSetName).
  String currentPortion;
  for(String::const_iterator c=in_string.begin(); c != in_string.end(); ++c)
    {
    // Look for the '$' to mark the beginning of a token.
    if(*c != '$')
      {
      currentPortion.insert(currentPortion.end(), *c);
      }
    else
      {
      // If there is a portion of the string, record it.
      if(currentPortion.length() > 0)
        {
        portions.push_back(new StringPortion(currentPortion));
        currentPortion = "";
        }
      // Get element set name token.
      String setName = "";
      ++c;
      // Look for all characters that can be part of a C++ identifier.
      while(c != in_string.end())
        {
        char ch = *c;
        if(((ch >= 'a') && (ch <= 'z'))
           || ((ch >= 'A') && (ch <= 'Z'))
           || ((ch >= '0') && (ch <= '9'))
           || (ch == '_'))
          {
          setName.insert(setName.end(), ch);
          ++c;
          }
        else
          {
          break;
          }
        }
      // We have a complete set name.  Make sure it is valid.
      if((setName.length() > 0)
         && (m_Sets.count(setName) > 0)
         && (out_set != m_Sets[setName]))
        {
        // We have a valid set name.  Prepare the substitution entry
        // for it.
        Substitution* sub;
        if(substitutions.count(setName) == 0)
          {
          sub = new Substitution();
          substitutions[setName] = sub;
          }
        else
          {
          sub = substitutions[setName];
          }
        portions.push_back(new ReplacePortion(*sub));
        setName = "";
        }
      else   
        {
        // Invalid set name.  Complain.
        throw UnknownSetException(setName);
        }
      // Begin the next portion of the string with this character,
      // the first after the end of the setName.
      if(c != in_string.end())
        currentPortion.insert(currentPortion.end(), *c);
      }
    }
  
  // If there is a final portion of the string, record it.
  if(currentPortion.length() > 0)
    {
    portions.push_back(new StringPortion(currentPortion));
    }

  // If there are no substitutions to be made, just generate this
  // single combination.
  if(substitutions.empty())
    {
    out_set->Add(in_element->GetTag(), in_element->GetCode());
    return;
    }
  
  // We must generate all combinations of element substitutions.
  
  // A pair of iterators into a Set.  The first is the current spot, and
  // the second is the ending iterator.
  typedef std::pair<Set::ConstIterator,
                    Set::ConstIterator>  SetIteratorPair;
  
  // Prepare a "stack" of element set iterator pairs that will keep track
  // of all combinations of substitutions to be done.
  std::list<SetIteratorPair> elementSets;
  
  // Keep track of the current substitution level.  Combinations are only
  // generated at the inner-most substitution level since bindings have
  // been assigned for all substitutions.
  Substitutions::const_iterator substitution = substitutions.begin();
  
  bool done = false;
  while(!done)
    {
    if(substitution == substitutions.end())
      {
      // We are at the end of the list of substitutions.
      if(elementSets.back().first != elementSets.back().second)
        {
        // Prepare the substitution for the current spot in this element set.
        --substitution;
        substitution->second->Set(elementSets.back().first->first,
                                  elementSets.back().first->second);
        ++substitution;

        // Generate this combination's output.
        String tag = in_element->GetTag();
        String code;
        // Put together all the pieces, with substitutions.
        for(Portions::const_iterator i = portions.begin();
            i != portions.end(); ++i)
          {
          tag.append((*i)->GetTag());
          code.append((*i)->GetCode());
          }
        // Add this combination to the output set.
        out_set->Add(tag, code);
        
        // Increment to the next spot in this unfinished element set.
        ++(elementSets.back().first);
        }
      else
        {
        // We have finished this set of combinations.  That is, we have
        // finished iterating through the elements in the inner-most
        // set of substitution elements.

        // Walk back through the list of substitutions until we find an
        // unfinished set.
        while(elementSets.back().first == elementSets.back().second)
          {
          // Pop off finished element set.
          elementSets.pop_back();
          // Corresponding back-step in substitution list.
          --substitution;
          
          // If the outermost element set has been finished,
          // then we are done.
          if(elementSets.empty())
            {
            // We have finished all combinations.
            done = true;
            break;
            }

          // Not done, so increment to the next spot in this unfinished element set.
          ++(elementSets.back().first);
          }
        if(!done)
          {
          --substitution;
          // Prepare the substitution for the current spot in this element set.
          substitution->second->Set(elementSets.back().first->first,
                                    elementSets.back().first->second);
          ++substitution;
          }
        }
      }
    else
      {
      // We are not at the end of the list of substitutions.
      // Move on to the next one.
      // Prepare to iterate over all elements in the next set of substitutions.
      elementSets.push_back(
        SetIteratorPair(m_Sets[substitution->first]->Begin(),
                                m_Sets[substitution->first]->End()));
      // Prepare the substitution for the current spot in this element set.
      substitution->second->Set(elementSets.back().first->first,
                                elementSets.back().first->second);
      ++substitution;
      }
    }
  
  // Free the string portions that were allocated.
  for(Portions::iterator i=portions.begin(); i != portions.end(); ++i)
    {
    delete *i;
    }
  
  // Free the substitutions that were allocated.
  for(Substitutions::iterator i = substitutions.begin();
      i != substitutions.end(); ++i)
    {
    delete i->second;
    }
}

} // namespace configuration

