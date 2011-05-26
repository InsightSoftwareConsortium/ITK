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
#ifdef DIM_1
__kernel void MeanFilter(__global const PIXELTYPE* in,__global PIXELTYPE* out, int radiusx, int width)
{
  int gix = get_global_id(0);
  float sum = 0;
  int   num = 0;
  if(gix < width)
  {
    for(int x = max((int)0, (int)(gix-radiusx)); x <= min((int)(width-1), (int)(gix+radiusx)); x++)
    {
      sum += (float)in[x];
      num++;
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
  int   num = 0;
  if(gix < width && giy < height)
  {
    for(int y = max((int)0, (int)(giy-radiusy)); y <= min((int)(height-1), (int)(giy+radiusy)); y++)
    {
      for(int x = max((int)0, (int)(gix-radiusx)); x <= min((int)(width-1), (int)(gix+radiusx)); x++)
      {
        unsigned int cidx = width*y + x;

        sum += (float)in[cidx];
        num++;
      }
    }
    out[gidx] = (PIXELTYPE)(sum/(float)num);
  }
}
#endif

#ifdef DIM_3
__kernel void MeanFilter(__global const PIXELTYPE* in,
                         __global PIXELTYPE* out,
                         int radiusx, int radiusy, int radiusz,
                         int width, int height, int depth)
{
  int gix = get_global_id(0);
  int giy = get_global_id(1);
  int giz = get_global_id(2);
  unsigned int gidx = width*(giz*heigh + giy) + gix;
  float sum = 0;
  int   num = 0;
  if(gix < width && giy < height && giz < depth)
  {
    for(int z = max(0, (int)(giz-radiusz)); z <= min((int)(depth-1), (int)giz+radiusz)); z++)
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
    out[gidx] = (PIXELTYPE)(sum/(float)num);
  }
}
#endif
