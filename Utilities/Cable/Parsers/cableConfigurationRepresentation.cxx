#include "configRep.h"

      
/**
 * Print the CodeBlock's lines to the output file.
 */
void
CodeBlock
::PrintLines(FILE* outFile) const
{
  for(LinesConstIterator l = m_Lines.begin();
      l != m_Lines.end(); ++l)
    {
    fprintf(outFile, "%s", l->c_str());
    }
}


/**
 * Construct a new Create and return a smart pointer to it.
 */
Create::Pointer
Create
::New(void)
{
  return new Create;
}


/**
 * Print this create function to the file.
 */
void
Create
::PrintFunction(FILE* outFile, const String& typeName) const
{
  fprintf(outFile,
          "#define _wrap_TypeName %s\n"
          "template <>\n"
          "struct NewObjectOf<%s>\n"
          "{\n"
          "  static %s* Create(void)\n"
          "  {\n",
          typeName.c_str(),
          typeName.c_str(),
          typeName.c_str());
  this->PrintLines(outFile);
  fprintf(outFile, "\n"
          "  }\n"
          "};\n"
          "#undef _wrap_TypeName\n");
}


/**
 * Construct a new Delete and return a smart pointer to it.
 */
Delete::Pointer
Delete
::New(void)
{
  return new Delete;
}


/**
 * Print this delete function to the file.
 */
void
Delete
::PrintFunction(FILE* outFile, const String& typeName) const
{
  fprintf(outFile,
          "#define _wrap_TypeName %s\n"
          "template <>\n"
          "struct OldObjectOf<%s>\n"
          "{\n"
          "  static void Delete(void* object)\n"
          "  {\n",
          typeName.c_str(),
          typeName.c_str());
  this->PrintLines(outFile);
  fprintf(outFile, "\n"
          "  }\n"
          "};\n"
          "#undef _wrap_TypeName\n");
}

/**
 * Construct a new WrapType and return a smart pointer to it.
 */
WrapType::Pointer
WrapType
::New(const String& name)
{
  return new WrapType(name);
}


/**
 * Print the configuration for this WrapType.
 */
void
WrapType
::Print(FILE* file) const
{
  fprintf(file, "  <WrapType name=\"%s\">\n",
          m_Name.c_str());
  if(m_Create)
    {
    fprintf(file, "    <Create>\n");
    m_Create->PrintFunction(file, m_Name);
    fprintf(file, "    </Create>\n");
    }
  if(m_Delete)
    {
    fprintf(file, "    <Delete>\n");
    m_Delete->PrintFunction(file, m_Name);
    fprintf(file, "    </Delete>\n");
    }
  fprintf(file, "  </WrapType>\n");
}


/**
 * Construct a new WrapperConfiguration and return a smart pointer to it.
 */
WrapperConfiguration::Pointer
WrapperConfiguration
::New(const String& source, const String& dest)
{
  return new WrapperConfiguration(source, dest);
}


/**
 * Return a FILE pointer to the input XML file specified in the configuration.
 */
FILE*
WrapperConfiguration
::GetSourceXML(void) const
{
  FILE* f = fopen(m_Source.c_str(), "rt");
  if(!f)
    {
    throw String("Error opening XML source file: ")+m_Source+"\n";
    }
  
  return f;
}


/**
 * Return a FILE pointer to the output file specified in the configuration.
 */
FILE*
WrapperConfiguration
::GetOutputFile(void) const
{
  if(m_Dest.length() == 0)
    {
    return stdout;
    }
  else
    {
    FILE* f = fopen(m_Dest.c_str(), "rt");
    if(!f)
      {
      throw String("Error opening output file: ")+m_Dest+"\n";
      }
    
    return f;
    }
}


/**
 * Print the wrapper configuration.  This includes what types will be used.
 */
void
WrapperConfiguration
::Print(FILE* file) const
{
  fprintf(file, "<WrapperConfiguration source=\"%s\" dest=\"%s\">\n",
          m_Source.c_str(), m_Dest.c_str());
  for(WrapTypesConstIterator w = m_WrapTypes.begin();
      w != m_WrapTypes.end(); ++w)
    {
    w->second->Print(file);
    }
  fprintf(file, "</WrapperConfiguration>\n");
}


/**
 * Print out the list of names of WrapType s that do not know about
 * their Class definitions.
 */
void
WrapperConfiguration
::PrintMissingTypes(FILE* file) const
{
  for(WrapTypesConstIterator w = m_WrapTypes.begin();
      w != m_WrapTypes.end(); ++w)
    {
    if(!w->second->HaveClass())
      {
      fprintf(file, "%s\n", w->first.c_str());
      }
    }
}


/**
 * Given a namespace, try to find the Class representations of all the
 * WrapTypes.  Return whether they were all found.
 *
 * This is done by walking all the namespaces and classes, and comparing
 * the fully qualified names with those needed.  When a match is found,
 * the entry is set.
 */
bool
WrapperConfiguration
::FindTypes(Namespace* globalNamespace)
{
  this->FindTypesInNamespace(globalNamespace);

  // Check if all types have been found.
  for(WrapTypesConstIterator w = m_WrapTypes.begin();
      w != m_WrapTypes.end(); ++w)
    {
    if(!w->second->HaveClass())
      {
      return false;
      }
    }
  return true;
}


/**
 * Helper to FindTypes to walk a Namespace.
 */
void
WrapperConfiguration
::FindTypesInNamespace(Namespace* ns)
{
  for(ClassesIterator c = ns->GetClasses().begin();
      c != ns->GetClasses().end(); ++c)
    {
    if((*c)->IsPublic())
      {
      this->FindTypesInClass(*c);
      }
    }
  for(NamespacesIterator n = ns->GetNamespaces().begin();
      n != ns->GetNamespaces().end(); ++n)
    {
    this->FindTypesInNamespace(*n);
    }
}


/**
 * Helper to FindTypes to walk a Class.
 */
void
WrapperConfiguration
::FindTypesInClass(Class* cl)
{
  String name = cl->GetQualifiedName();
  
  // See if this class a desired type.
  if((m_WrapTypes.count(name) > 0)
     && !m_WrapTypes[name]->HaveClass())
    {
    m_WrapTypes[name]->SetClass(cl);
    }
  
  for(ClassesIterator c = cl->GetClasses().begin();
      c != cl->GetClasses().end(); ++c)
    {
    if((*c)->IsPublic())
      {
      this->FindTypesInClass(*c);
      }
    }
}
