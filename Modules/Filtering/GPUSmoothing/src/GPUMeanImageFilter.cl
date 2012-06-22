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
// Brute-force mean filter
//
// The current implementation does not use shared memory, so this could be
// very slow on older GPUs (pre-Fermi) that do not have hardware cache.
//

#ifdef DIM_1
__kernel void MeanFilter(__global const PIXELTYPE* in,__global PIXELTYPE* out, int radiusx, int width)
{
  int gix = get_global_id(0);
  float sum = 0;
  unsigned int num = 0;
  if(gix < width)
  {
    /*
    // Clamping boundary condition
    for(int x = max((int)0, (int)(gix-radiusx)); x <= min((int)(width-1), (int)(gix+radiusx)); x++)
    {
      sum += (float)in[x];
      num++;
    }
    */

    // Zero-flux boundary condition
    num = 2*radiusx + 1;
    for(int x = gix-radiusx; x <= gix+radiusx; x++)
    {
      unsigned int cidx = (unsigned int)(min(max(0, x),width-1));
      sum += (float)in[cidx];
    }

    out[gix] = (PIXELTYPE)(sum/(float)num);
  }
}
#endif

#ifdef DIM_2
__kernel void MeanFilter(__global const PIXELTYPE* in,
                         __global PIXELTYPE* out,
                         int radiusx, int radiusy, int width, int height)
{
  int gix = get_global_id(0);
  int giy = get_global_id(1);
  unsigned int gidx = width*giy + gix;
  float sum = 0;
  unsigned int   num = 0;

  if(gix < width && giy < height)
  {
    /*
    // Clamping boundary condition
    for(int y = max((int)0, (int)(giy-radiusy)); y <= min((int)(height-1), (int)(giy+radiusy)); y++)
    {
      for(int x = max((int)0, (int)(gix-radiusx)); x <= min((int)(width-1), (int)(gix+radiusx)); x++)
      {
        unsigned int cidx = width*y + x;

        sum += (float)in[cidx];
        num++;
      }
    }
    */

    // Zero-flux boundary condition
    num = (2*radiusx + 1)*(2*radiusy + 1);
    for(int y = giy-radiusy; y <= giy+radiusy; y++)
    {
      unsigned int yid = (unsigned int)(min(max(0, y),height-1));
      for(int x = gix-radiusx; x <= gix+radiusx; x++)
      {
        unsigned int cidx = width*yid + (unsigned int)(min(max(0, x),width-1));

        sum += (float)in[cidx];
      }
    }

    out[gidx] = (PIXELTYPE)(sum/(float)num);
  }
}
#endif

#ifdef DIM_3
__kernel void MeanFilter(const __global PIXELTYPE* in,
                         __global PIXELTYPE* out,
                         int radiusx, int radiusy, int radiusz,
                         int width, int height, int depth)
{

  int gix = (int)get_global_id(0);
  int giy = (int)get_global_id(1);
  int giz = (int)get_global_id(2);

  unsigned int gidx = width*(giz*height + giy) + gix;

  float sum = 0;
  unsigned int num = 0;

  /* NOTE: More than three-level nested conditional statements (e.g.,
     if A && B && C..) invalidates command queue during kernel
     execution on Apple OpenCL 1.0 (such Macbook Pro with NVIDIA 9600M
     GT). Therefore, we flattened conditional statements. */
  bool isValid = true;
  if(gix >= width) isValid = false;
  if(giy >= height) isValid = false;
  if(giz >= depth) isValid = false;

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

          sum += (float)in[cidx];
          num++;
        }
      }
    }
*/

    // Zero-flux boundary condition
    num = (2*radiusx + 1)*(2*radiusy + 1)*(2*radiusz + 1);
    for(int z = giz-radiusz; z <= giz+radiusz; z++)
    {
      unsigned int zid = (unsigned int)(min(max(0, z),depth-1));
      for(int y = giy-radiusy; y <= giy+radiusy; y++)
      {
        unsigned int yid = (unsigned int)(min(max(0, y),height-1));
        for(int x = gix-radiusx; x <= gix+radiusx; x++)
        {
          unsigned int cidx = width*(zid*height + yid) + (unsigned int)(min(max(0, x),width-1));

          sum += (float)in[cidx];
        }
      }
    }

    out[gidx] = (PIXELTYPE)(sum/(float)num);
  }

}
#endif
