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
  CxxGenerator(const configuration::Package* in_package):
    m_Package(in_package) {}
  ~CxxGenerator() {}
  
  void Generate(std::ostream&);
private:
  
  void GenerateNamespace(std::ostream&, const Indent&,
                         const configuration::Namespace*);
  void GenerateWrapperSet(std::ostream&, const Indent&,
                          const configuration::WrapperSet*);
  void GenerateInstantiationSet(std::ostream&, const Indent&,
                                const configuration::InstantiationSet*);

  /**
   * The package to generate.
   */
  configuration::Package::ConstPointer m_Package;
};

} // namespace gen
  
#endif
