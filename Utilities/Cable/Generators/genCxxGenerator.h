#ifndef _genCxxGenerator_h
#define _genCxxGenerator_h

#include "configRep.h"

namespace gen
{


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

using namespace configuration;

/**
 * Generation class for C++ wrappers.
 */
class CxxGenerator
{
public:
  CxxGenerator(const Package* in_package):
    m_Package(in_package) {}
  ~CxxGenerator() {}
  
  void Generate(std::ostream&);
private:
  
  void GenerateNamespace(std::ostream&, const Indent&, const Namespace*);
  void GenerateWrapperSet(std::ostream&, const Indent&, const WrapperSet*);

  /**
   * The package to generate.
   */
  Package::ConstPointer m_Package;
};

} // namespace gen
  
#endif
