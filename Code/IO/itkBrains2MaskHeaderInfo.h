#ifndef __itkBrains2MaskHeaderInfo_H__
#define __itkBrains2MaskHeaderInfo_H__

#include "itkBrains2HeaderBase.h"

namespace itk {
class Brains2MaskHeaderInfo: public Brains2HeaderBase
{
public:
  Brains2MaskHeaderInfo();
  ~Brains2MaskHeaderInfo();
  virtual std::string GetHeaderBeginTag(void) const;
  virtual std::string GetHeaderEndTag(void) const;
protected:
private:
};

}//End namespace itk
#endif // __itkBrains2MaskHeaderInfo_H__
