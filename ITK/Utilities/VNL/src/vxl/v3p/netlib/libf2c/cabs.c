#include "v3p_f2c.h"
#undef abs
#undef min
#undef max

#ifdef KR_headers
extern double sqrt();
double f__cabs(real_value, imag_value) double real_value, imag_value;
#else
#undef abs
#include "math.h"
#ifdef __cplusplus
extern "C" {
#endif
double f__cabs(double real_value, double imag_value)
#endif
{
double temp;

if(real_value < 0)
        real_value = -real_value;
if(imag_value < 0)
        imag_value = -imag_value;
if(imag_value > real_value){
        temp = real_value;
        real_value = imag_value;
        imag_value = temp;
}
if((real_value+imag_value) == real_value)
        return(real_value);

temp = imag_value/real_value;
temp = real_value*sqrt(1.0 + temp*temp);  /*overflow!!*/
return(temp);
}
#ifdef __cplusplus
}
#endif
