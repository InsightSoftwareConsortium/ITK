/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
// This is vxl/vnl/algo/vnl_fft_3d.h
#ifndef __vnl_fft_3d_h
#define __vnl_fft_3d_h
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief In-place 3D fast fourier transform
// \author fsm

#include "vnl/vnl_matrix.h"
#include "vnl/algo/vnl_fft_base.h"

//: In-place 3D fast fourier transform

template< class T >
struct vnl_fft_3d:public vnl_fft_base< 3, T > {
  typedef vnl_fft_base< 3, T > base;

  //: constructor takes size of signal.
  vnl_fft_3d(int M, int N, int Q)
  {
    base::factors_[0].resize(M);
    base::factors_[1].resize(N);
    base::factors_[2].resize(Q);
  }

  //: dir = +1/-1 according to direction of transform.
  void transform(vnl_matrix< vcl_complex< T > > & signal, int dir)
  { base::transform(signal.data_block(), dir); }

  //: forward FFT
  void fwd_transform(vnl_matrix< vcl_complex< T > > & signal)
  { transform(signal, +1); }

  //: backward (inverse) FFT
  void bwd_transform(vnl_matrix< vcl_complex< T > > & signal)
  { transform(signal, -1); }

  //: return size of signal.
  unsigned rows() const { return base::factors_[0].number(); }
  unsigned cols() const { return base::factors_[1].number(); }
};

#endif // vnl_fft_3d_h_
