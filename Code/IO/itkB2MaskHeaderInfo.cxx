#include "itkB2MaskHeaderInfo.h"
//#include "itkMacro.h"

namespace itk {
    B2MaskHeaderInfo::B2MaskHeaderInfo()
    {
        //Nothing to be done here.
    }
    B2MaskHeaderInfo::~B2MaskHeaderInfo()
    {
        //Nothing to be done here.
    }
     std::string B2MaskHeaderInfo::GetHeaderBeginTag(void) const { return std::string("MASK_HEADER_BEGIN"); }
     std::string B2MaskHeaderInfo::GetHeaderEndTag(void)   const { return std::string("MASK_HEADER_END"); }
} //End namespace itk
