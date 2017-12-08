// This is core/vnl/algo/vnl_fft.h
#ifndef vnl_fft_h_
#define vnl_fft_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \author fsm

#include <vcl_compiler.h>
#include <vnl/algo/vnl_algo_export.h>

//: use C++ overloading to find the correct FORTRAN routine from templated FFT code.
void VNL_ALGO_EXPORT vnl_fft_setgpfa(float  *triggs, long size, long pqr[3], long *info);
//: use C++ overloading to find the correct FORTRAN routine from templated FFT code.
void VNL_ALGO_EXPORT vnl_fft_setgpfa(double *triggs, long size, long pqr[3], long *info);


//        CALL GPFA(A,B,TRIGS,INC,JUMP,N,LOT,ISIGN,NIPQ,INFO)
//
//        A IS FIRST REAL INPUT/OUTPUT VECTOR
//        B IS FIRST IMAGINARY INPUT/OUTPUT VECTOR
//        TRIGS IS A TABLE OF TWIDDLE FACTORS, PRECALCULATED
//              BY CALLING SUBROUTINE 'SETGPFA'
//        INC IS THE INCREMENT WITHIN EACH DATA VECTOR
//        JUMP IS THE INCREMENT BETWEEN DATA VECTORS
//        N IS THE LENGTH OF THE TRANSFORMS:
//          -----------------------------------
//            N = (2**IP) * (3**IQ) * (5**IR)
//          -----------------------------------
//        LOT IS THE NUMBER OF TRANSFORMS
//        ISIGN = +1 FOR FORWARD TRANSFORM
//              = -1 FOR INVERSE TRANSFORM
//     NIPQ is an array containing the number of factors (for
//     power of 2,3 and 5
//     INFO is set to -1 if there is a problem, 0 otherwise

// These functions perform a number (LOT) of 1D FFTs, each of the same signal size (N).
// The signal is stored in two real arrays (A, B), with consecutive elements separated
// by a stride (INC). The separation between the LOT signals to be transformed is JUMP.
void VNL_ALGO_EXPORT vnl_fft_gpfa(float  *a, float  *b, float const  *triggs,
                  long inc, long jump, long n,
                  long lot, long isign, long const pqr[3], long *info);
void VNL_ALGO_EXPORT vnl_fft_gpfa(double *a, double *b, double const *triggs,
                  long inc, long jump, long n,
                  long lot, long isign, long const pqr[3], long *info);

#endif // vnl_fft_h_
