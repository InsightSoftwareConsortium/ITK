#ifndef DoubleToString_h
#define DoubleToString_h

#include "double-conversion.h"
#include "itkMacro.h"

class DoubleToString
{
public:
  DoubleToString() :
    m_DoubleToStringConverter(double_conversion::DoubleToStringConverter::EcmaScriptConverter())
    {
    }
  std::string operator()(double val)
    {
      char buf[256];
      double_conversion::StringBuilder builder(buf,sizeof(buf));
      builder.Reset();
      if(!m_DoubleToStringConverter.ToShortest(val,&builder))
        {
        itkGenericExceptionMacro(<< "Conversion failed for " << val);
        }
      return std::string(builder.Finalize());
    }
private:
  DoubleToString & operator=(const DoubleToString &); // not defined
  const double_conversion::DoubleToStringConverter &m_DoubleToStringConverter;
};

#endif // DoubleToString_h
