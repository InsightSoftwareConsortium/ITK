#include <vnl/vnl_complex.h>
#include <vcl_complex.txx> // needed for __doadv() with EGCS -- PVr
//fsm: Why is it needed (i.e. what is the symptom of not doing so)? Does
//every Templates file which uses complex need it? Could this not be fixed
//in vcl/egcs/vcl_complex.txx? We have not had any egcs problems in Oxford.
#include <vnl/vnl_c_vector.txx>

VNL_C_VECTOR_INSTANTIATE_unordered(vnl_double_complex);
