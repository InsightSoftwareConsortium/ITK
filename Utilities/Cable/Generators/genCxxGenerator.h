#ifndef _genCxxGenerator_h
#define _genCxxGenerator_h

#include "configRep.h"

namespace gen
{

typedef std::string String;

/**
 * Class to simplify indentation printing.
 */
class Indent
{
public:
  Indent(unsigned int indent): m_Indent(indent) {}
  void Print(std::ostream& os) const;
  Indent Next() const { return Indent(m_Indent+2); }
private:
  unsigned int m_Indent;
};

std::ostream& operator<<(std::ostream&, const Indent&);

/**
 * Generation class for C++ wrappers.
 */
class CxxGenerator
{
public:
  CxxGenerator(const configuration::CableConfiguration* in_config):
    m_CableConfiguration(in_config) {}
  ~CxxGenerator() {}
  
  void Generate();
private:
  void GeneratePackage(const configuration::Package*);  
  void GenerateHeaderIncludes(std::ostream&, const configuration::Headers*);  
  void GenerateNamespace(std::ostream&, const Indent&,
                         const configuration::PackageNamespace*);
  void GenerateWrapperSet(std::ostream&, const Indent&,
                          const configuration::WrapperSet*);
  void GenerateInstantiationSet(std::ostream&, const Indent&,
                                const configuration::InstantiationSet*);

  /**
   * The configuration that controls generation.
   */
  configuration::CableConfiguration::ConstPointer m_CableConfiguration;
};

} // namespace gen
  
#endif
