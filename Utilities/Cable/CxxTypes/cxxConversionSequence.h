#ifndef _cxxConversionSequence_h
#define _cxxConversionSequence_h

namespace _cxx_
{


/**
 * Abstract interface to any conversion sequence.
 */
class ConversionSequence
{
};


#if 0
template <typename Out>
struct ConvertTo
{
  static Out With(ConversionSequence* cs, void* object)
    {
      return ((Out (*)(void*))cs->GetFunction())(object)
    }
};
#endif




} // namespace _cxx_

#endif
