#ifndef __itkB2MaskHeaderInfo_H__
#define __itkB2MaskHeaderInfo_H__

#include "itkB2HeaderBase.h"

namespace itk {
class B2MaskHeaderInfo: public B2HeaderBase
{
public:
  B2MaskHeaderInfo();
  ~B2MaskHeaderInfo();
  virtual std::string GetHeaderBeginTag(void) const;
  virtual std::string GetHeaderEndTag(void) const;
protected:
private:
};

}//End namespace itk
#endif // __itkB2MaskHeaderInfo_H__
