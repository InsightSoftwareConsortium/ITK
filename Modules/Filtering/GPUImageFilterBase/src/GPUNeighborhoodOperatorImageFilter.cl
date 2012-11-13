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

//
// Brute-force neighborhood operator filter
//

#ifdef DIM_1
__kernel void NeighborOperatorFilter(const __global INTYPE* in,
                                     __constant int *inIndex,
                                     __constant int *inSize,
                                     __global OUTTYPE* out,
                                     __constant int *outIndex,
                                     __constant int *outSize,
                                     __constant OPTYPE* op,
                                     int radiusx
                                     )
{
  int gix = get_global_id(0);
  int inGix = gix + outIndex[0] - inIndex[0]; //index conversion
  OPTYPE sum = 0;
  unsigned int opIdx = 0;

  if(gix < outSize[0])
  {
    /*
    // Clamping boundary condition
    for(int x = max((int)0, (int)(gix-radiusx)); x <= min((int)(width-1), (int)(gix+radiusx)); x++)
    {
      sum += (OPTYPE)in[x] * (OPTYPE)op[opIdx];
      opIdx++;
    }
    */

    // Zero-flux boundary condition
    for(int x = inGix-radiusx; x <= inGix+radiusx; x++)
    {
      unsigned int cidx = (unsigned int)(min(max(0, x),inSize[0]-1));
      sum += (OPTYPE)in[cidx] * (OPTYPE)op[opIdx];
      opIdx++;
    }

    out[gix] = (OUTTYPE)(sum);
  }
}
#endif

#ifdef DIM_2
__kernel void NeighborOperatorFilter(const __global INTYPE* in,
                                     __constant int *inIndex,
                                     __constant int *inSize,
                                     __global OUTTYPE* out,
                                     __constant int *outIndex,
                                     __constant int *outSize,
                                     __constant OPTYPE* op,
                                     int radiusx, int radiusy
                                     )
{
  int gix = get_global_id(0);
  int giy = get_global_id(1);
  int inGix = gix + outIndex[0] - inIndex[0]; //index conversion
  int inGiy = giy + outIndex[1] - inIndex[1]; //index conversion
  unsigned int gidx = outSize[0]*giy + gix;
  OPTYPE sum = 0;
  unsigned int opIdx = 0;

  if(gix < outSize[0] && giy < outSize[1])
  {
    /*
    // Clamping boundary condition
    for(int y = max((int)0, (int)(giy-radiusy)); y <= min((int)(height-1), (int)(giy+radiusy)); y++)
    {
      for(int x = max((int)0, (int)(gix-radiusx)); x <= min((int)(width-1), (int)(gix+radiusx)); x++)
      {
        unsigned int cidx = width*y + x;

        sum += (OPTYPE)in[cidx] * (OPTYPE)op[opIdx];
        opIdx++;
      }
    }
    */

    // Zero-flux boundary condition
    for(int y = inGiy-radiusy; y <= inGiy+radiusy; y++)
    {
      unsigned int yid = (unsigned int)(min(max(0, y),inSize[1]-1));
      for(int x = inGix-radiusx; x <= inGix+radiusx; x++)
      {
        unsigned int cidx = inSize[0]*yid + (unsigned int)(min(max(0, x),inSize[0]-1));

        sum += (OPTYPE)in[cidx] * (OPTYPE)op[opIdx];

        opIdx++;
      }
    }

    out[gidx] = (OUTTYPE)(sum);
  }
}
#endif

#ifdef DIM_3
__kernel void NeighborOperatorFilter(const __global INTYPE* in,
                                     __constant int *inIndex,
                                     __constant int *inSize,
                                     __global OUTTYPE* out,
                                     __constant int *outIndex,
                                     __constant int *outSize,
                                     __constant OPTYPE* op,
                                     int radiusx, int radiusy, int radiusz
                                     )
{
  int gix = get_global_id(0);
  int giy = get_global_id(1);
  int giz = get_global_id(2);
  int inGix = gix + outIndex[0] - inIndex[0]; //index conversion
  int inGiy = giy + outIndex[1] - inIndex[1]; //index conversion
  int inGiz = giz + outIndex[2] - inIndex[2]; //index conversion
  unsigned int gidx = outSize[0]*(giz*outSize[1] + giy) + gix;
  OPTYPE sum = 0;
  unsigned int opIdx = 0;

  /* NOTE: More than three-level nested conditional statements (e.g.,
     if A && B && C..) invalidates command queue during kernel
     execution on Apple OpenCL 1.0 (such Macbook Pro with NVIDIA 9600M
     GT). Therefore, we flattened conditional statements. */
  bool isValid = true;
  if(gix < 0 || gix >= outSize[0]) isValid = false;
  if(giy < 0 || giy >= outSize[1]) isValid = false;
  if(giz < 0 || giz >= outSize[2]) isValid = false;

  if( isValid )
  {
    /*
    // Clamping boundary condition
    for(int z = max(0, (int)(giz-radiusz)); z <= min((int)(depth-1), (int)(giz+radiusz)); z++)
    {
      for(int y = max((int)0, (int)(giy-radiusy)); y <= min((int)(height-1), (int)(giy+radiusy)); y++)
      {
        for(int x = max((int)0, (int)(gix-radiusx)); x <= min((int)(width-1), (int)(gix+radiusx)); x++)
        {
          unsigned int cidx = width*(z*height + y) + x;

          sum += (OPTYPE)in[cidx] * (OPTYPE)op[opIdx];
          opIdx++;
        }
      }
    }
    */

    // Zero-flux boundary condition
    for(int z = inGiz-radiusz; z <= inGiz+radiusz; z++)
    {
      unsigned int zid = (unsigned int)(min(max(0, z),inSize[2]-1));
      for(int y = inGiy-radiusy; y <= inGiy+radiusy; y++)
      {
        unsigned int yid = (unsigned int)(min(max(0, y),inSize[1]-1));
        for(int x = inGix-radiusx; x <= inGix+radiusx; x++)
        {
          unsigned int cidx = inSize[0]*(zid*inSize[1] + yid) + (unsigned int)(min(max(0, x),inSize[0]-1));
          sum += (OPTYPE)(in[cidx]) * op[opIdx];
          opIdx++;
        }
      }
    }

    out[gidx] = (OUTTYPE)(sum);
  }
}
#endif
