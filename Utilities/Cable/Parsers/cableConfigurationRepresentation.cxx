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
::New(const String& fileName)
{
  return new WrapperConfiguration(fileName);
}


/**
 * Return a FILE pointer to the input XML file specified in the configuration.
 */
FILE*
WrapperConfiguration
::GetSourceXML(void) const
{
  FILE* f = fopen(m_FileName.c_str(), "rt");
  if(!f)
    {
    throw String("Error opening XML source file: ")+m_FileName+"\n";
    }
  
  return f;
}


/**
 * Print the wrapper configuration.  This includes what types will be used.
 */
void
WrapperConfiguration
::Print(FILE* file) const
{
  fprintf(file, "<WrapperConfiguration source=\"%s\">\n",
          m_FileName.c_str());
  for(WrapTypesConstIterator w = m_WrapTypes.begin();
      w != m_WrapTypes.end(); ++w)
    {
    (*w)->Print(file);
    }
  fprintf(file, "</WrapperConfiguration>\n");
}

