#include "itkBrains2IPLHeaderInfo.h"
//#include "itkMacro.h"

namespace itk {
Brains2IPLHeaderInfo::Brains2IPLHeaderInfo()
{
  //Nothing to be done here.
}
Brains2IPLHeaderInfo::~Brains2IPLHeaderInfo()
{
  //Nothing to be done here.
}
std::string Brains2IPLHeaderInfo::GetHeaderBeginTag(void) const { return std::string("IPL_HEADER_BEGIN"); }
std::string Brains2IPLHeaderInfo::GetHeaderEndTag(void)   const { return std::string("IPL_HEADER_END"); }
} //End namespace itk
