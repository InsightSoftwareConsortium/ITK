/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#define BLOCK_DIM 16 // set by ITK for 2D

// Basic filtering brick, used both for
// 1D filtering of 2D images
// and for 2D filtering of 3D volumes

__kernel void
partialFilter(const __global INPIXELTYPE * data,
              __global OUTPIXELTYPE *      outs,
              __constant REALTYPE *        Bvalues,
              __constant REALTYPE *        MMatrix,
              unsigned int                 start,
              unsigned int                 step,
              unsigned int                 length)
{
  REALTYPE       scratch[MAX_LINE_LENGTH];
  const REALTYPE outV1 = data[start] / (1.0 - Bvalues[1] - Bvalues[2] - Bvalues[3]);
  REALTYPE       sV0 = outV1, sV1 = outV1, sV2 = outV1;

  // Unrolling does not work well here (truly recursive).
  for (unsigned int i = 0; i < length; i++)
  {
    scratch[i] = (REALTYPE)(data[start + i * step] + sV0 * Bvalues[1] + sV1 * Bvalues[2] + sV2 * Bvalues[3]);

    sV2 = sV1;
    sV1 = sV0;
    sV0 = scratch[i];
  }

  const REALTYPE remB = (1.0 - Bvalues[1] - Bvalues[2] - Bvalues[3]);
  const REALTYPE u_p = data[start + (length - 1) * step] / remB;
  const REALTYPE v_p = u_p / remB;

  // Unrolling the loops explicitly helps on certain compilers (e.g. ATI/AMD architecture) and does not
  // worsen performance on NVidia GPUs, for which the compiler has been reported to unroll the loops by default).
  REALTYPE Vn0 = (REALTYPE)(v_p //;
                            + (scratch[length - 1] - u_p) * MMatrix[0] + (scratch[length - 2] - u_p) * MMatrix[1] +
                            (scratch[length - 3] - u_p) * MMatrix[2]);
  REALTYPE Vn1 = (REALTYPE)(v_p //;
                            + (scratch[length - 1] - u_p) * MMatrix[3] + (scratch[length - 2] - u_p) * MMatrix[4] +
                            (scratch[length - 3] - u_p) * MMatrix[5]);

  REALTYPE Vn2 = (REALTYPE)(v_p //;
                            + (scratch[length - 1] - u_p) * MMatrix[6] + (scratch[length - 2] - u_p) * MMatrix[7] +
                            (scratch[length - 3] - u_p) * MMatrix[8]);

  /*REALTYPE Vn0 = v_p;
  REALTYPE Vn1 = v_p;
  REALTYPE Vn2 = v_p;

  //#pragma unroll 3
  for (unsigned int i = 0;i < 3;++i)
  {
      Vn0 += (scratch[length - 1 - i] - u_p) * MMatrix[i];
      Vn1 += (scratch[length - 1 - i] - u_p) * MMatrix[3+i];
      Vn2 += (scratch[length - 1 - i] - u_p) * MMatrix[6+i];
  }*/

  // This was not in the 2006 Triggs paper but sounds quite logical since m_B is not one
  Vn0 *= Bvalues[0];
  Vn1 *= Bvalues[0];
  Vn2 *= Bvalues[0];

  scratch[length - 1] = Vn0;

  // Very bad idea to unroll here as well (see causal pass).
  for (int i = length - 2; i >= 0; i--)
  {
    scratch[i] = (REALTYPE)(scratch[i] * Bvalues[0] + Vn0 * Bvalues[1] + Vn1 * Bvalues[2] + Vn2 * Bvalues[3]);
    Vn2 = Vn1;
    Vn1 = Vn0;
    Vn0 = scratch[i];
  }

// Roll the anticausal part into the output
#pragma unroll 16
  for (unsigned int i = 0; i < length; ++i)
  {
    outs[start + i * step] = (OUTPIXELTYPE)(scratch[i]);
  }
}

/*
#ifdef DIM_1
//This means (recursively) filtering values in one single vector.
//Code present only for completeness (1D->3D).
__kernel void YvvFilter(__global const INPIXELTYPE* data,
                        __global OUTPIXELTYPE* outs,
                        __constant REALTYPE* m_Bvalues,
                        __constant REALTYPE* m_MMatrix,
                        int sizeX )
{
    if (sizeX>MAX_LINE_LENGTH) {return;}

    int gix = get_global_id(0);

    if(gix == 0)
    {
        partialFilter(data, outs, m_Bvalues, m_MMatrix, 0, 1, sizeX);
    }

    return;
}

#endif
*/

#ifdef DIM_2

__kernel void
YvvFilter(const __global INPIXELTYPE * data,
          __global OUTPIXELTYPE *      outs,
          __constant REALTYPE *        m_Bvalues,
          __constant REALTYPE *        m_MMatrix,
          int                          sizeX,
          int                          sizeY,
          unsigned int                 dim)
{
  if (sizeX > MAX_LINE_LENGTH || sizeY > MAX_LINE_LENGTH)
  {
    return;
  }

  int WIDTH = sizeX;
  int HEIGHT = sizeY;

  int gix = get_global_id(0);

  switch (dim)
  {
    case 0: // X
      if (gix < HEIGHT)
      {
        // start= (id(0)*height + id(1))*width + 0;
        partialFilter(data, outs, m_Bvalues, m_MMatrix, gix * WIDTH, 1, WIDTH);
      }
      break;
    case 1: // Y
      if (gix < WIDTH)
      {
        // start= (id(1)*height + 0)*width + id(0);
        partialFilter(data, outs, m_Bvalues, m_MMatrix, gix, WIDTH, HEIGHT);
      }
      break;
  }
  return;
}

#endif


#ifdef DIM_3

__kernel void
YvvFilter(const __global INPIXELTYPE * data,
          __global OUTPIXELTYPE *      outs,
          __constant REALTYPE *        m_Bvalues,
          __constant REALTYPE *        m_MMatrix,
          int                          sizeX,
          int                          sizeY,
          int                          sizeZ,
          unsigned int                 dim // to know which dimension to do
)
{
  if (sizeX > MAX_LINE_LENGTH || sizeY > MAX_LINE_LENGTH)
  {
    return;
  }

  int WIDTH = sizeX;
  int HEIGHT = sizeY;
  int DEPTH = sizeZ;

  int gix = get_global_id(0);
  int giy = get_global_id(1);
  /*
      __local REALTYPE Bvalues[4];
      __local REALTYPE MMatrix[9];

      if(gix<9)
      {
          if(gix<4)
          {
              Bvalues[gix] = m_Bvalues[gix];
          }
          MMatrix[gix] = m_MMatrix[gix];
      }
      barrier(CLK_LOCAL_MEM_FENCE);
  */

  switch (dim)
  {         // generic start= (giz*height + giy)*width + gix;
    case 0: // X
      if (gix < DEPTH && giy < HEIGHT)
      {
        // start= (id(0)*height + id(1))*width + 0;
        partialFilter(data, outs, m_Bvalues, m_MMatrix, (gix * HEIGHT + giy) * WIDTH, 1, WIDTH);
      }
      break;
    case 1: // Y
      if (giy < DEPTH && gix < WIDTH)
      {
        // start= (id(1)*height + 0)*width + id(0);
        partialFilter(data, outs, m_Bvalues, m_MMatrix, giy * WIDTH * HEIGHT + gix, WIDTH, HEIGHT);
      }
      break;
    case 2: // Z
      if (gix < WIDTH && giy < HEIGHT)
      {
        // start= (0*height + id(1))*width + id(0);
        partialFilter(data, outs, m_Bvalues, m_MMatrix, giy * WIDTH + gix, HEIGHT * WIDTH, DEPTH);
      }
      break;
  }
  return;
}
#endif
