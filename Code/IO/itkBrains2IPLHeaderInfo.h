#ifndef __itkBrains2IPLHeaderInfo_H__
#define __itkBrains2IPLHeaderInfo_H__

#include "itkBrains2HeaderBase.h"

namespace itk {
class Brains2IPLHeaderInfo: public Brains2HeaderBase
{
public:
  Brains2IPLHeaderInfo();
  ~Brains2IPLHeaderInfo();
  virtual std::string GetHeaderBeginTag(void) const;
  virtual std::string GetHeaderEndTag(void) const;
protected:
private:
};

}//End namespace itk
#endif // __itkBrains2IPLHeaderInfo_H__
