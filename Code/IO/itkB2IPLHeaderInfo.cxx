#include "itkB2IPLHeaderInfo.h"
//#include "itkMacro.h"

namespace itk {
    B2IPLHeaderInfo::B2IPLHeaderInfo()
    {
        //Nothing to be done here.
    }
    B2IPLHeaderInfo::~B2IPLHeaderInfo()
    {
        //Nothing to be done here.
    }
     std::string B2IPLHeaderInfo::GetHeaderBeginTag(void) const { return std::string("IPL_HEADER_BEGIN"); }
     std::string B2IPLHeaderInfo::GetHeaderEndTag(void)   const { return std::string("IPL_HEADER_END"); }
} //End namespace itk
