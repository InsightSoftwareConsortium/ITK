%include <std_unordered_map.i>

%{
#include "itkLabelOverlapMeasuresImageFilter.h"
%}

%template(hashmapUCLOLSM)    std::unordered_map< unsigned char, itk::LabelOverlapLabelSetMeasures >;
%template(hashmapSCLOLSM)    std::unordered_map< signed char, itk::LabelOverlapLabelSetMeasures >;
%template(hashmapUSLOLSM)    std::unordered_map< unsigned short, itk::LabelOverlapLabelSetMeasures >;
%template(hashmapSSLOLSM)    std::unordered_map< signed short, itk::LabelOverlapLabelSetMeasures >;
%template(hashmapULLOLSM)    std::unordered_map< unsigned long, itk::LabelOverlapLabelSetMeasures >;
%template(hashmapSLLOLSM)    std::unordered_map< signed long, itk::LabelOverlapLabelSetMeasures >;
%template(hashmapULLLOLSM)   std::unordered_map< unsigned long long, itk::LabelOverlapLabelSetMeasures >;
%template(hashmapSLLLOLSM)   std::unordered_map< signed long long, itk::LabelOverlapLabelSetMeasures >;
