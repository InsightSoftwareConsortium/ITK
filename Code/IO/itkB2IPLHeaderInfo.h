#ifndef __itkB2IPLHeaderInfo_H__
#define __itkB2IPLHeaderInfo_H__

#include "itkB2HeaderBase.h"

namespace itk {
class B2IPLHeaderInfo: public B2HeaderBase
{
public:
  B2IPLHeaderInfo();
  ~B2IPLHeaderInfo();
  virtual std::string GetHeaderBeginTag(void) const;
  virtual std::string GetHeaderEndTag(void) const;
protected:
private:
};

}//End namespace itk
#endif // __itkB2IPLHeaderInfo_H__
