/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    genTclGenerator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "genTclGenerator.h"

#include <iostream>
#include <fstream>

namespace gen
{

typedef configuration::CableConfiguration CableConfiguration;

/**
 * Construct an instance of this generator and return it.
 */
GeneratorBase* TclGenerator::GetInstance(const CableConfiguration* in_config,
                                         const source::Namespace* in_globalNamespace)
{
  return new TclGenerator(in_config, in_globalNamespace);
}


/**
 * Generate Tcl wrappers for all packages specified in the configuration.
 */
void
TclGenerator
::Generate()
{
  // Just loop over all pacakges in the configuration.
  for(CableConfiguration::PackageIterator package =
        m_CableConfiguration->BeginPackages();
      package != m_CableConfiguration->EndPackages(); ++package)
    {
    this->GeneratePackage(*package);
    }
}


void
TclGenerator
::GeneratePackage(const configuration::Package* package)
{
  // Make sure the output directory exists.
  if(!GeneratorBase::MakeDirectory("Tcl"))
    {
    std::cerr << "Error making Tcl directory." << std::endl;
    return;
    }
  
  // Setup the output file names.
  String wrapperFile = "Tcl/"+package->GetName()+"_tcl.cxx";
  
  // Open the output files.
  std::ofstream wrapperStream(wrapperFile.c_str());
  if(!wrapperStream) { return; }
  
  wrapperStream <<
    "#include \"wrapCalls.h\"\n"
    "\n"
    "/**\n"
    " * Include headers needed for wrapped types.\n"
    " */\n";
  
  // Be sure to include needed headers.
  this->GenerateIncludes(wrapperStream, package->GetHeaders());

  wrapperStream <<
    "\n";
  
  // Begin the recursive generation at the package's starting namespace.
  this->GenerateNamespace(wrapperStream, package->GetStartingNamespace());

  wrapperStream.close();
}

void
TclGenerator
::GenerateIncludes(std::ostream& wrapperStream,
                   const configuration::Headers* headers)
{
  // Make sure we have headers to write out.
  if(!headers)
    {
    return;
    }
  
  // Include headers with the "all" purpose.
  for(configuration::Headers::FilesIterator header = headers->BeginFiles();
      header != headers->EndFiles(); ++header)
    {
    if(header->purpose == "" || header->purpose == "all")
      {
      wrapperStream << "#include \"" << header->name.c_str() << "\""
                    << std::endl;
      }
    }
}


void
TclGenerator
::GenerateNamespace(std::ostream& wrapperStream,
                    const configuration::PackageNamespace* ns)
{
  for(configuration::PackageNamespace::WrapperIterator wIter =
        ns->BeginWrappers();
      wIter != ns->EndWrappers(); ++wIter)
    {
    const configuration::Named* wrapper = *wIter;
    if(wrapper->IsPackageNamespace())
      {
      this->GenerateNamespace(wrapperStream,
                              dynamic_cast<const configuration::PackageNamespace*>(wrapper));
      }
    else if(wrapper->IsWrapperSet())
      {
      this->GenerateWrapperSet(wrapperStream,
                               dynamic_cast<const configuration::WrapperSet*>(wrapper),
                               ns);
      }
    }

}

void
TclGenerator
::GenerateWrapperSet(std::ostream& wrapperStream,
                     const configuration::WrapperSet* wrapperSet,
                     const configuration::PackageNamespace* ns)
{
  String qualifedName = ns->GetQualifiedName()+"::"+wrapperSet->GetName();
  source::Class* wStruct = m_GlobalNamespace->LookupClass(qualifedName);
  for(configuration::WrapperSet::ConstIterator wrapper = wrapperSet->Begin();
      wrapper != wrapperSet->End(); ++wrapper)
    {
    source::Class* c = wStruct->LookupClass(wrapper->first);
    if(c)
      {
      this->GenerateClassWrapper(wrapperStream, c);
      }
    else
      {
      wrapperStream << "// Couldn't find type " << wrapper->first.c_str() << std::endl;
      }
    }
}

void
TclGenerator
::GenerateClassWrapper(std::ostream& wrapperStream,
                       const source::Class* c)
{
  const source::Namespace* gns = c->GetGlobalNamespace();
  typedef std::vector<source::Method*> Methods;
  Methods methods;
  for(source::MethodsIterator methodItr = c->GetMethods().begin();
      methodItr != c->GetMethods().end(); ++methodItr)
    {
    source::Method* method = *methodItr;
    if(method->GetAccess() == source::Public)
      {
      if(method->IsMethod()
         || method->IsConstructor())
        {
        methods.push_back(method);
        }
      }
    }
  
  wrapperStream <<
    "#define _wrap_WRAPPED_TYPE " << c->GetName().c_str() << "\n"
    "#define _wrap_WRAPPED_TYPE_NAME \"" << c->GetName().c_str() << "\"\n"
    "#define _wrap_METHOD_WRAPPER_PROTOTYPES \\\n";
  
  if(!methods.empty())
    {
    for(unsigned int m = 0 ; m < methods.size() ;)
      {
      if(methods[m]->IsMethod())
        {
        wrapperStream <<
          "  void Method_" << m << "_" << methods[m]->GetName().c_str() << "(const Argument&, const Arguments&) const";
        }
      else if(methods[m]->IsConstructor())
        {
        wrapperStream <<
          "  void* Constructor_" << m << "(const Arguments&) const";
        }
      if(++m != methods.size())
        {
        wrapperStream << "; \\";
        }
      wrapperStream << "\n";
      }
    }
  else
    {
    wrapperStream <<
      "  typedef int PlaceholderForNoMethodWrappers\n";
    }
  
  wrapperStream <<
    "\n"
    "#include \"wrapWrapperInclude.h\"\n"
    "\n"
    "#undef _wrap_METHOD_WRAPPER_PROTOTYPES\n"
    "#undef _wrap_WRAPPED_TYPE_NAME\n"
    "#undef _wrap_WRAPPED_TYPE\n"
    "\n"
    "namespace _wrap_\n"
    "{\n"
    "\n";

  
  for(unsigned int m = 0 ; m < methods.size() ; ++m)
    {
    if(methods[m]->IsMethod())
      {
      wrapperStream <<
        "void\n"
        "Wrapper< " << c->GetName().c_str() << " >\n"
        "::Method_" << m << "_" << methods[m]->GetName().c_str() << "(const Argument& implicit, const Arguments& arguments) const\n"
        "{\n";
      
      String implicit = c->GetName();
      if(methods[m]->IsConst())
        {
        implicit = "const "+implicit;
        }
      wrapperStream <<
        "  " << implicit.c_str() << "& instance = ArgumentAsReferenceTo< " << implicit.c_str() << " >::Get(implicit, this);\n";
      
      if(methods[m]->GetReturns() && methods[m]->GetReturns()->GetType()
         && methods[m]->GetReturns()->GetType()->GetCxxType(gns).GetName() != "void")
        {
        const source::Type* t = methods[m]->GetReturns()->GetType();
        if(t->IsPointerType())
          {
          const source::PointerType* pt = dynamic_cast<const source::PointerType*>(t);
          t = pt->GetPointedToType();
          wrapperStream <<
            "  ReturnPointerTo< " << t->GetCxxType(gns).GetName() << " >::From(\n";
          }
        else if(t->IsReferenceType())
          {
          const source::ReferenceType* rt = dynamic_cast<const source::ReferenceType*>(t);
          t = rt->GetReferencedType();
          wrapperStream <<
            "  ReturnReferenceTo< " << t->GetCxxType(gns).GetName() << " >::From(\n";
          }
        else
          {
          wrapperStream <<
            "  Return< " << t->GetCxxType(gns).GetName() << " >::From(\n";
          }
        }

      wrapperStream <<
        "  instance." << methods[m]->GetName() << "(";
      
      unsigned int argCount = 0;
      for(source::ArgumentsIterator a = methods[m]->GetArguments().begin();
          a != methods[m]->GetArguments().end(); ++a)
        {
        if(a != methods[m]->GetArguments().begin())
          wrapperStream << ",";
        wrapperStream << "\n";
        const source::Type* t = (*a)->GetType();
        if(t->IsReferenceType())
          {
          const source::ReferenceType* rt = dynamic_cast<const source::ReferenceType*>(t);
          t = rt->GetReferencedType();
          wrapperStream <<
            "    ArgumentAsReferenceTo< " << t->GetCxxType(gns).GetName() << " >::Get(arguments[" << argCount++ << "], this)";
          }
        else
          {
          wrapperStream <<
            "    ArgumentAs< " << t->GetCxxType(gns).GetName() << " >::Get(arguments[" << argCount++ << "], this)";
          }
        }
      
      if(methods[m]->GetReturns() && methods[m]->GetReturns()->GetType()
         && methods[m]->GetReturns()->GetType()->GetCxxType(gns).GetName() != "void")
        {
        wrapperStream << "), this);\n";
        }
      else
        {
        wrapperStream << ");\n"
          "  Return<void>::From(this);\n";
        }
      
      wrapperStream <<
        "}\n"
        "\n";
      }
    else if(methods[m]->IsConstructor())
      {
        wrapperStream <<
          "void*\n"
          "Wrapper< " << c->GetName().c_str() << " >\n"
          "::Constructor_" << m << "(const Arguments& arguments) const\n"
          "{\n"
          "  return new " << c->GetName().c_str() << "(";
        
        unsigned int argCount = 0;
        for(source::ArgumentsIterator a = methods[m]->GetArguments().begin();
            a != methods[m]->GetArguments().end(); ++a)
          {
          if(a != methods[m]->GetArguments().begin())
            wrapperStream << ",";
          wrapperStream << "\n";
          const source::Type* t = (*a)->GetType();
          if(t->IsReferenceType())
            {
            const source::ReferenceType* rt = dynamic_cast<const source::ReferenceType*>(t);
            t = rt->GetReferencedType();
            wrapperStream <<
              "    ArgumentAsReferenceTo< " << t->GetCxxType(gns).GetName() << " >::Get(arguments[" << argCount++ << "], this)";
            }
          else
            {
            wrapperStream <<
              "    ArgumentAs< " << t->GetCxxType(gns).GetName() << " >::Get(arguments[" << argCount++ << "], this)";
            }
          }
        
        wrapperStream << ");\n"
          "}\n"
          "\n";
      }
    }
  
  wrapperStream <<
    "\n"
    "void\n"
    "Wrapper< " << c->GetName().c_str() << " >\n"
    "::RegisterMethodWrappers()\n"
    "{\n";

  for(unsigned int m = 0 ; m < methods.size() ; ++m)
    {
    if(!methods[m]->GetArguments().empty())
      {
      wrapperStream <<
        "  {\n"
        "  Method::ParameterTypes parameterTypes;\n";
      for(source::ArgumentsIterator a = methods[m]->GetArguments().begin();
          a != methods[m]->GetArguments().end(); ++a)
        {
        const source::Type* t = (*a)->GetType();
        wrapperStream <<
          "  parameterTypes.push_back(CvType< " << t->GetCxxType(gns).GetName() << " >::type.GetType());\n";
        }
      }

    wrapperStream <<
      "  this->AddFunction(\n";
    
    if(methods[m]->IsMethod())
      {
      String returnTypeName = "void";
      if(methods[m]->GetReturns() && methods[m]->GetReturns()->GetType())
        {
        returnTypeName = methods[m]->GetReturns()->GetType()->GetCxxType(gns).GetName();
        }
      wrapperStream <<
        "    new Method(this, &Wrapper::Method_" << m << "_" << methods[m]->GetName() << ",\n"
        "               \"" << methods[m]->GetName() << "\", " << (methods[m]->IsConst() ? "true":"false") << ",\n"
        "               CvType< " << returnTypeName.c_str() << " >::type";
      }
    else if(methods[m]->IsConstructor())
      {
      wrapperStream <<
        "    new Constructor(this, &Wrapper::Constructor_" << m << ",\n"
        "                    \"" << methods[m]->GetName() << "\"";
      
      }
    
    if(methods[m]->GetArguments().empty())
      {
      wrapperStream << "));\n";
      }
    else
      {
      wrapperStream << ",\n"
        "      parameterTypes));\n"
        "  }\n";
      }
    }
  
  wrapperStream <<
    "}\n"
    "\n"
    "} // namespace _wrap_\n"
    "\n";
}

} // namespace gen
