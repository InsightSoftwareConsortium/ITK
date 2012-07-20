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

// assume input and output pixel type is same
#define INPIXELTYPE PIXELTYPE
#define BUFPIXELTYPE float
//
// Dim 1
//
#ifdef DIM_1
#define ImageDimension 1
__kernel void AverageGradientMagnitudeSquared(__global const INPIXELTYPE *in, __global BUFPIXELTYPE *buf, float scalex, int width)
{
  int gix = get_global_id(0);

  // NOTE! 1D version is not implemented
  if(gix < width)
  {
    buf[gix] = 0;
  }
}
#endif

//
// Dim 2
//
// sm needs to be of size = sizeof(float) * get_local_size(0) * get_local_size(1)
// x_l and x_r need to be of size = get_local_size(0)
// y_l and y_r need to be of size = get_local_size(1)
//
#ifdef DIM_2
#define ImageDimension 2
__kernel void AverageGradientMagnitudeSquared(__global const INPIXELTYPE *in, __global BUFPIXELTYPE *buf, local float* sm,
                                              local float* x_l, local float* x_r,
                                              local float* y_l, local float* y_r,
                                              float scalex, float scaley, int width, int height)
{
  // global index
  int gix = get_global_id(0);
  int giy = get_global_id(1);
  unsigned int gidx = width*giy + gix;
  unsigned int bidx =  get_num_groups(0)*get_group_id(1) + get_group_id(0);

  // local index for shared memory.. note 1-pixel boundary for shared memory
  int lix = get_local_id(0);
  int liy = get_local_id(1);
  int lwidth = get_local_size(0);
  int lheight = get_local_size(1);
  int lidx = lwidth*liy + lix;

  // centralized derivatives, half directonal derivatives
  float dx[2];

  bool isValid = true;
  if (gix >= width) isValid = false;
  if (giy >= height) isValid = false;

  if (!isValid)
  {
    sm[lidx] = 0;
  }
  else
  {
    // inner
    sm[lidx] = in[gidx];

    if(lix == 0)
    {
      // Left
      if(gix > 0) y_l[liy] = in[gidx-1];
      else y_l[liy] = in[gidx];
    }
    if(lix == lwidth-1 || gix == width-1)
    {
      // Right
      if(gix < (width-1)) y_r[liy] = in[gidx+1];
      else y_r[liy] = in[gidx];
    }

    if(liy == 0)
    {
      // Bottom
      if(giy > 0) x_l[lix] = in[gidx - width];
      else x_l[lix] = in[gidx];
    }
    if(liy == lheight-1 || giy == height-1)
    {
      // Up
      if(giy < (height-1)) x_r[lix] = in[gidx + width];
      else x_r[lix] = in[gidx];
    }
  }

  // Synchronize shared memory
  barrier(CLK_LOCAL_MEM_FENCE);

  float val = 0;

  // Compute Update
  if(isValid)
  {
    float df, db;

    // centralized derivatives
    df = (lix == lwidth-1 || gix == width-1) ? y_r[liy] : sm[lidx+1];
    db = (lix == 0) ? y_l[liy] : sm[lidx-1];
    dx[0] = (df - db)*-0.5f*scalex;  // grad x

    df = (liy == lheight-1 || giy == height-1) ? x_r[lix] : sm[lidx+lwidth];
    db = (liy == 0) ? x_l[lix] : sm[lidx-lwidth];
    dx[1] = (df - db)*-0.5f*scaley;

    val = dx[0]*dx[0] + dx[1]*dx[1];
  }

  // Synchronize shared memory
  barrier(CLK_LOCAL_MEM_FENCE);

  sm[lidx] = val;

  barrier(CLK_LOCAL_MEM_FENCE);

  // Reduction on shared memory
  // Reducton along y
  int interval = lheight/2;

  while(interval > 0)
  {
    if(liy < interval)
    {
      sm[lidx] += sm[lidx + interval*lwidth];
    }

    interval = interval >> 1; // divide by 2

    barrier(CLK_LOCAL_MEM_FENCE);
  }

  // Reduction along x
  interval = lwidth/2;
  while (interval > 0)
  {
    if (liy == 0 && lix < interval)
    {
      sm[lidx] += sm[lidx+interval];
    }
    interval = interval >> 1;
    // every work item needs to visit this barrier, otherwise
    // an INVALID_COMMAND_QUEUE was occurring on Windows and Linux
    // NVIDIA OpenCL implementations
    barrier(CLK_LOCAL_MEM_FENCE);
  }

  // write the final value to global memory
  if(lix == 0 && liy == 0)
  {
    // Write the final value to global memory
    buf[bidx] = sm[lidx];
  }
}
#endif

//
// Dim 3
//
// sm needs to be of size = sizeof(float) * get_local_size(0) * get_local_size(1) * get_local_size(2)
// xy_l and xy_r need to be of size = get_local_size(0) * get_local_size(1)
// xz_l and xz_r need to be of size = get_local_size(0) * get_local_size(2)
// yz_l and yz_r need to be of size = get_local_size(1) * get_local_size(2)
//
#ifdef DIM_3
#define ImageDimension 3
__kernel void AverageGradientMagnitudeSquared(__global const INPIXELTYPE *in, __global BUFPIXELTYPE *buf, local float* sm,
                                              local float* xy_l, local float* xy_r,
                                              local float* xz_l, local float* xz_r,
                                              local float* yz_l, local float* yz_r,
                                              float scalex, float scaley, float scalez,
                                              int width, int height, int depth)
{
  int gix = get_global_id(0);
  int giy = get_global_id(1);
  int giz = get_global_id(2);
  unsigned int gidx = (unsigned int)width*((unsigned int)(giz*height + giy)) + (unsigned int)gix;
  // index into the buffer that's been reduced by sum
  unsigned int bidx = get_num_groups(0)*(get_num_groups(1)*get_group_id(2) + get_group_id(1)) + get_group_id(0);

  // local index for shared memory.. note 1-pixel boundary for shared memory
  int lix = get_local_id(0);
  int liy = get_local_id(1);
  int liz = get_local_id(2);
  int lwidth = get_local_size(0);
  int lheight = get_local_size(1);
  int ldepth = get_local_size(2);
  int lidx = lwidth*(liz*lheight + liy) + lix;
  int xyidx = lwidth*liy + lix;
  int xzidx = lwidth*liz + lix;
  int yzidx = lheight*liz + liy;

  // centralized derivatives, half directonal derivatives
  float dx[3];

  // shared memory - split boundary and center to avoid bank conflict

  // Center
  bool isValid = true;
  if (gix >= width) isValid = false;
  if (giy >= height) isValid = false;
  if (giz >= depth) isValid = false;

  if (!isValid)
  {
    sm[lidx] = 0;
  }
  else
  {
    sm[lidx] = in[gidx];

    // 6 top/bottom/left/right/up/down neighbor planes
    if(lix == 0)
    {
      // y/z top plane
      if(gix > 0) yz_l[yzidx] = in[gidx-1];
      else yz_l[yzidx] = in[gidx];
    }
    if(lix == lwidth-1 || gix == width-1)
    {
      // y/z bottom plane
      if(gix < (width-1)) yz_r[yzidx] = in[gidx+1];
      else yz_r[yzidx] = in[gidx];
    }

    if(liy == 0)
    {
      // x/z top plane
      if(giy > 0) xz_l[xzidx] = in[gidx - width];
      else xz_l[xzidx] = in[gidx];
    }
    if(liy == lheight-1 || giy == height-1)
    {
      // x/z bottom plane
      if(giy < (height-1)) xz_r[xzidx] = in[gidx + width];
      else xz_r[xzidx] = in[gidx];
    }

    if(liz == 0)
    {
      // x/y top plane
      if(giz > 0) xy_l[xyidx] = in[gidx - width*height];
      else xy_l[xyidx] = in[gidx];
    }
    if(liz == ldepth-1 || giz == depth-1)
    {
      // x/z bottom plane
      if(giz < (depth-1)) xy_r[xyidx] = in[gidx + width*height];
      else xy_r[xyidx] = in[gidx];
    }
  }

  // synchronize shared memory
  barrier(CLK_LOCAL_MEM_FENCE);

  float val = 0;

  if (isValid)
  {
    // Compute Update
    // centralized derivatives
    float df, db;

    df = (lix == lwidth-1 || gix == width-1) ? yz_r[yzidx] : sm[lidx+1];
    db = (lix == 0) ? yz_l[yzidx] : sm[lidx-1];
    dx[0] = (df - db)*-0.5f*scalex;

    df = (liy == lheight-1 || giy == height-1) ? xz_r[xzidx] : sm[lidx+lwidth];
    db = (liy == 0) ? xz_l[xzidx] : sm[lidx-lwidth];
    dx[1] = (df - db)*-0.5f*scaley;

    df = (liz == ldepth-1 || giz == depth-1) ? xy_r[xyidx] : sm[lidx+lwidth*lheight];
    db = (liz == 0) ? xy_l[xyidx] : sm[lidx-lwidth*lheight];
    dx[2] = (df - db)*-0.5f*scalez;

    val = dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];
  }

  barrier(CLK_LOCAL_MEM_FENCE);

  sm[lidx] = val;

  barrier(CLK_LOCAL_MEM_FENCE);

  //
  // Reduction by Sum
  //
  int interval = ldepth/2;

  while(interval > 0) // reduction along z axis
  {
    if(liz < interval)
    {
      sm[lidx] += sm[lidx + interval*lwidth*lheight];
    }

    interval = interval >> 1; // divide by 2

    barrier(CLK_LOCAL_MEM_FENCE);
  }

  interval = lheight/2;

  while(interval > 0)
  {
    if(liz == 0 && liy < interval)
    {
      sm[lidx] += sm[lidx+interval*lwidth];
    }

    interval = interval >> 1; // divide by 2

    // every work item needs to visit this barrier, otherwise
    // an INVALID_COMMAND_QUEUE was occurring on Windows and Linux
    // NVIDIA OpenCL implementations
    barrier(CLK_LOCAL_MEM_FENCE);
  }

  // Reduction along x
  interval = lwidth/2;

  while(interval > 0)
  {
    if (liz == 0 && liy == 0)
    {
      if(lix < interval)
      {
        sm[lidx] += sm[lidx + interval];
      }
    }
    //if(interval > 16) barrier(CLK_LOCAL_MEM_FENCE);  // don't need to synchronize if within a warp (only for NVIDIA)
    interval = interval >> 1; // divide by 2

    // every work item needs to visit this barrier, otherwise
    // an INVALID_COMMAND_QUEUE was occurring on Windows and Linux
    // NVIDIA OpenCL implementations
    barrier(CLK_LOCAL_MEM_FENCE);
  }

  // write the final value to global memory
  if (liz == 0 && liy == 0)
  {
    if(lix == 0)
    {
      // Write the final value to global memory
      buf[bidx] = sm[lidx];
    }
  }
}
#endif
