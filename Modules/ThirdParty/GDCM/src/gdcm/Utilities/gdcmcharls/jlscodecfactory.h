//
// (C) CharLS Team 2014, all rights reserved. See the accompanying "License.txt" for licensed use. 
//

#ifndef CHARLS_JLSCODECFACTORY
#define CHARLS_JLSCODECFACTORY

#include <memory>

struct JlsCustomParameters;
struct JlsParameters;

template<typename STRATEGY>
class JlsCodecFactory
{
public:
    std::unique_ptr<STRATEGY> GetCodec(const JlsParameters& params, const JlsCustomParameters&);
private:
    std::unique_ptr<STRATEGY> GetCodecImpl(const JlsParameters& params);
};

#endif
