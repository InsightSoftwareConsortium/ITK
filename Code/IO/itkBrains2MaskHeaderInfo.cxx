#include "itkBrains2MaskHeaderInfo.h"
//#include "itkMacro.h"

namespace itk {
Brains2MaskHeaderInfo::Brains2MaskHeaderInfo()
{
  //Nothing to be done here.
}
Brains2MaskHeaderInfo::~Brains2MaskHeaderInfo()
{
  //Nothing to be done here.
}
std::string Brains2MaskHeaderInfo::GetHeaderBeginTag(void) const { return std::string("MASK_HEADER_BEGIN"); }
std::string Brains2MaskHeaderInfo::GetHeaderEndTag(void)   const { return std::string("MASK_HEADER_END"); }
} //End namespace itk
