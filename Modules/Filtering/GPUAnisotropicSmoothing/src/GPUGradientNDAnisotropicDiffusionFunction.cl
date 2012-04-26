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
#define BUFPIXELTYPE PIXELTYPE

//
// Dim 1
//
#ifdef DIM_1
#define ImageDimension 1
__kernel void ComputeUpdate(__global const INPIXELTYPE *in, __global BUFPIXELTYPE *buf, ARGTYPE K, float scalex, int width)
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
#ifdef DIM_2
#define ImageDimension 2
__kernel void ComputeUpdate(__global const INPIXELTYPE *in, __global BUFPIXELTYPE *buf, ARGTYPE K, float scalex, float scaley, int width, int height)
{
  // global index
  int gix = get_global_id(0);
  int giy = get_global_id(1);

  unsigned int gidx = width*giy + gix;

  // local index for shared memory.. note 1-pixel boundary for shared memory
  int lix = get_local_id(0) + 1;
  int liy = get_local_id(1) + 1;

  // centralized derivatives, half directonal derivatives
  float dx[2], dx_f, dx_b, delta, Cx, Cxd;

  __local float sm[BLOCK_SIZE+2][BLOCK_SIZE+2];

  bool isValid = true;
  if (gix >= width) isValid = false;
  if (giy >= height) isValid = false;

  if (isValid)
  {

    // Read (BLOCK_SIZE+2)x(BLOCK_SIZE+2) data - 1 pixel boundary around block
    sm[lix][liy] = in[gidx];

    if(lix == 1)
    {
      // left
      if(gix > 0) sm[lix-1][liy] = in[gidx-1];
      else sm[lix-1][liy] = in[gidx];

      // corner
      if(liy == 1)
      {
        sm[lix-1][liy-1] = in[(width)*max(0,(int)(giy-1)) + max(0,(int)(gix-1))];
      }
    }
    if(lix == BLOCK_SIZE || gix == width-1)
    {
      // right
      if(gix < (width-1)) sm[lix+1][liy] = in[gidx+1];
      else sm[lix+1][liy] = in[gidx];

      // corner
      if(liy == BLOCK_SIZE || giy == height-1)
      {
        sm[lix+1][liy+1] = in[(width)*min(height-1,giy+1) + min(width-1,gix+1)];
      }
    }

    if(liy == 1)
    {
      // bottom
      if(giy > 0) sm[lix][liy-1] = in[gidx - width];
      else sm[lix][liy-1] = in[gidx];

      if(lix == BLOCK_SIZE || gix == width-1)
      {
        sm[lix+1][liy-1] = in[(width)*max(0,giy-1) + min(width-1,gix+1)];
      }
    }
    if(liy == BLOCK_SIZE || giy == height-1)
    {
      // up
      if(giy < (height-1)) sm[lix][liy+1] = in[gidx + width];
      else sm[lix][liy+1] = in[gidx];

      if(lix == 1)
      {
        sm[lix-1][liy+1] = in[width*min((int)(height-1), (int)(giy+1)) + max(0,(int)(gix-1))];
      }
    }
  }

  // synchronize shared memory
  barrier(CLK_LOCAL_MEM_FENCE);

  // Compute Update
  if(isValid)
  {
    // delta
    delta = 0;

    if(K > 0 || K < 0) // if K != 0
    {
      // centralized derivatives
      dx[0] = (sm[lix+1][liy] - sm[lix-1][liy])*0.5f*scalex;
      dx[1] = (sm[lix][liy+1] - sm[lix][liy-1])*0.5f*scaley;

      // along x
      dx_f = (sm[lix+1][liy] - sm[lix][liy])*scalex;
      dx_b = (sm[lix][liy] - sm[lix-1][liy])*scalex;
      Cx  = exp( (dx_f*dx_f + 0.25f*(pown(dx[1] + (sm[lix+1][liy+1] - sm[lix+1][liy-1])*0.5f*scaley, 2))) / K );
      Cxd = exp( (dx_b*dx_b + 0.25f*(pown(dx[1] + (sm[lix-1][liy+1] - sm[lix-1][liy-1])*0.5f*scaley, 2))) / K );
      delta += (Cx*dx_f - Cxd*dx_b);

      // along y
      dx_f = (sm[lix][liy+1] - sm[lix][liy])*scaley;
      dx_b = (sm[lix][liy] - sm[lix][liy-1])*scaley;
      Cx  = exp( (dx_f*dx_f + 0.25f*(pown(dx[0] + (sm[lix+1][liy+1] - sm[lix-1][liy+1])*0.5f*scalex, 2))) / K );
      Cxd = exp( (dx_b*dx_b + 0.25f*(pown(dx[0] + (sm[lix+1][liy-1] - sm[lix-1][liy-1])*0.5f*scalex, 2))) / K );
      delta += (Cx*dx_f - Cxd*dx_b);
    }

    buf[gidx] = delta;
  }
}
#endif

//
// Dim 3
//
#ifdef DIM_3
#define ImageDimension 3
__kernel void ComputeUpdate(__global const INPIXELTYPE *in, __global BUFPIXELTYPE *buf, ARGTYPE K, float scalex, float scaley, float scalez, int width, int height, int depth)
{
  int gix = get_global_id(0);
  int giy = get_global_id(1);
  int giz = get_global_id(2);
  unsigned int gidx = (unsigned int)width*((unsigned int)(giz*height + giy)) + (unsigned int)gix;

  // local index for shared memory.. note 1-pixel boundary for shared memory
  int lix = get_local_id(0) + 1;
  int liy = get_local_id(1) + 1;
  int liz = get_local_id(2) + 1;

  // centralized derivatives, half directonal derivatives
  float dx[3], dx_f, dx_b, delta, Cx, Cxd;
  unsigned int tidx, tx, ty, tz;;

  // shared memory
  __local float sm[BLOCK_SIZE+2][BLOCK_SIZE+2][BLOCK_SIZE+2];

  //
  // Read (BLOCK_SIZE+2)x(BLOCK_SIZE+2) data - 1 pixel boundary around block
  //
  bool isValid = true;
  if (gix >= width) isValid = false;
  if (giy >= height) isValid = false;
  if (giz >= depth) isValid = false;

  if (isValid)
  {

    // Center
    sm[lix][liy][liz] = in[gidx];

    // 6 top/bottom/left/right/up/down planes
    if(lix == 1)
    {
      // y/z top plane
      if(gix > 0) sm[lix-1][liy][liz] = in[gidx-1];
      else sm[lix-1][liy][liz] = in[gidx];
    }
    if(lix == BLOCK_SIZE || gix == width-1)
    {
      // y/z bottom plane
      if(gix < (width-1)) sm[lix+1][liy][liz] = in[gidx+1];
      else sm[lix+1][liy][liz] = in[gidx];
    }

    if(liy == 1)
    {
      // x/z top plane
      if(giy > 0) sm[lix][liy-1][liz] = in[gidx - width];
      else sm[lix][liy-1][liz] = in[gidx];
    }
    if(liy == BLOCK_SIZE || giy == height-1)
    {
      // x/z bottom plane
      if(giy < (height-1)) sm[lix][liy+1][liz] = in[gidx + width];
      else sm[lix][liy+1][liz] = in[gidx];
    }

    if(liz == 1)
    {
      // x/y top plane
      if(giz > 0) sm[lix][liy][liz-1] = in[gidx - width*height];
      else sm[lix][liy][liz-1] = in[gidx];
    }
    if(liz == BLOCK_SIZE || giz == depth-1)
    {
      // x/z bottom plane
      if(giz < (depth-1)) sm[lix][liy][liz+1] = in[gidx + width*height];
      else sm[lix][liy][liz+1] = in[gidx];
    }

    // 8 corners and 4 edges along z
    if(lix == 1 && liy == 1)
    {
      tx = (unsigned int)max(0, gix-1);
      ty = (unsigned int)max(0, giy-1);
      tz = (unsigned int)giz;
      tidx = width*(tz*height + ty) + tx;
      sm[lix-1][liy-1][liz] = in[ tidx ];

      if(liz == 1)
      {
        tz = (unsigned int)max(0, giz-1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix-1][liy-1][liz-1] = in[ tidx ];
      }
      if(liz == BLOCK_SIZE || giz == depth-1)
      {
        tz = (unsigned int)min(depth-1, giz+1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix-1][liy-1][liz+1] = in[ tidx ];
      }
    }
    if(lix == 1 && (liy == BLOCK_SIZE || giy == height-1))
    {
      tx = (unsigned int)max(0, gix-1);
      ty = (unsigned int)min(height-1, giy+1);
      tz = (unsigned int)giz;
      tidx = width*(tz*height + ty) + tx;
      sm[lix-1][liy+1][liz] = in[ tidx ];

      if(liz == 1)
      {
        tz = (unsigned int)max(0, giz-1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix-1][liy+1][liz-1] = in[ tidx ];
      }
      if(liz == BLOCK_SIZE || giz == depth-1)
      {
        tz = (unsigned int)min(depth-1, giz+1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix-1][liy+1][liz+1] = in[ tidx ];
      }
    }
    if((lix == BLOCK_SIZE || gix == width-1) && liy == 1)
    {
      tx = (unsigned int)min(width-1, gix+1);
      ty = (unsigned int)max(0, giy-1);
      tz = (unsigned int)giz;
      tidx = width*(tz*height + ty) + tx;
      sm[lix+1][liy-1][liz] = in[ tidx ];

      if(liz == 1)
      {
        tz = (unsigned int)max(0, giz-1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix+1][liy-1][liz-1] = in[ tidx ];
      }
      if(liz == BLOCK_SIZE || giz == depth-1)
      {
        tz = (unsigned int)min(depth-1, giz+1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix+1][liy-1][liz+1] = in[ tidx ];
      }
    }
    if((lix == BLOCK_SIZE || gix == width-1) && (liy == BLOCK_SIZE || giy == height-1))
    {
      tx = (unsigned int)min(width-1, gix+1);
      ty = (unsigned int)min(height-1, giy+1);
      tz = (unsigned int)giz;
      tidx = width*(tz*height + ty) + tx;
      sm[lix+1][liy+1][liz] = in[ tidx ];

      if(liz == 1)
      {
        tz = (unsigned int)max(0, giz-1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix+1][liy+1][liz-1] = in[ tidx ];
      }
      if(liz == BLOCK_SIZE || giz == depth-1)
      {
        tz = (unsigned int)min(depth-1, giz+1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix+1][liy+1][liz+1] = in[ tidx ];
      }
    }

    // 4 edges along y
    if(lix == 1 && liz == 1)
    {
      tx = (unsigned int)max(0, gix-1);
      ty = (unsigned int)giy;
      tz = (unsigned int)max(0, giz-1);
      tidx = width*(tz*height + ty) + tx;
      sm[lix-1][liy][liz-1] = in[ tidx ];

      if(liy == 1)
      {
        ty = (unsigned int)max(0, giy-1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix-1][liy-1][liz-1] = in[ tidx ];
      }
      if(liy == BLOCK_SIZE || giy == height-1)
      {
        ty = (unsigned int)min(height-1, giy+1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix-1][liy+1][liz-1] = in[ tidx ];
      }
    }
    if(lix == 1 && (liz == BLOCK_SIZE || giz == depth-1))
    {
      tx = (unsigned int)max(0, gix-1);
      ty = (unsigned int)giy;
      tz = (unsigned int)min(depth-1, giz+1);
      tidx = width*(tz*height + ty) + tx;
      sm[lix-1][liy][liz+1] = in[ tidx ];

      if(liy == 1)
      {
        ty = (unsigned int)max(0, giy-1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix-1][liy-1][liz+1] = in[ tidx ];
      }
      if(liy == BLOCK_SIZE || giy == height-1)
      {
        ty = (unsigned int)min(height-1, giy+1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix-1][liy+1][liz+1] = in[ tidx ];
      }
    }
    if((lix == BLOCK_SIZE || gix == width-1) && liz == 1)
    {
      tx = (unsigned int)min(width-1, gix+1);
      ty = (unsigned int)giy;
      tz = (unsigned int)max(0, giz-1);
      tidx = width*(tz*height + ty) + tx;
      sm[lix+1][liy][liz-1] = in[ tidx ];

      if(liy == 1)
      {
        ty = (unsigned int)max(0, giy-1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix+1][liy-1][liz-1] = in[ tidx ];
      }
      if(liy == BLOCK_SIZE || giy == height-1)
      {
        ty = (unsigned int)min(height-1, giy+1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix+1][liy+1][liz-1] = in[ tidx ];
      }
    }
    if((lix == BLOCK_SIZE || gix == width-1) && (liz == BLOCK_SIZE || giz == depth-1))
    {
      tx = (unsigned int)min(width-1, gix+1);
      ty = (unsigned int)giy;
      tz = (unsigned int)min(depth-1, giz+1);
      tidx = width*(tz*height + ty) + tx;
      sm[lix+1][liy][liz+1] = in[ tidx ];

      if(liy == 1)
      {
        ty = (unsigned int)max(0, giy-1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix+1][liy-1][liz+1] = in[ tidx ];
      }
      if(liy == BLOCK_SIZE || giy == height-1)
      {
        ty = (unsigned int)min(height-1, giy+1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix+1][liy+1][liz+1] = in[ tidx ];
      }
    }


    // 4 edges along x
    if(liy == 1 && liz == 1)
    {
      tx = (unsigned int)gix;
      ty = (unsigned int)max(0, giy-1);
      tz = (unsigned int)max(0, giz-1);
      tidx = width*(tz*height + ty) + tx;
      sm[lix][liy-1][liz-1] = in[ tidx ];

      if(lix == 1)
      {
        tx = (unsigned int)max(0, gix-1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix-1][liy-1][liz-1] = in[ tidx ];
      }
      if(lix == BLOCK_SIZE || gix == width-1)
      {
        tx = (unsigned int)min(width-1, gix+1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix+1][liy-1][liz-1] = in[ tidx ];
      }
    }
    if(liy == 1 && (liz == BLOCK_SIZE || giz == depth-1))
    {
      tx = (unsigned int)gix;
      ty = (unsigned int)max(0, giy-1);
      tz = (unsigned int)min(depth-1, giz+1);
      tidx = width*(tz*height + ty) + tx;
      sm[lix][liy-1][liz+1] = in[ tidx ];

      if(lix == 1)
      {
        tx = (unsigned int)max(0, gix-1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix-1][liy-1][liz+1] = in[ tidx ];
      }
      if(lix == BLOCK_SIZE || gix == width-1)
      {
        tx = (unsigned int)min(width-1, gix+1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix+1][liy-1][liz+1] = in[ tidx ];
      }
    }
    if((liy == BLOCK_SIZE || giy == height-1) && liz == 1)
    {
      tx = (unsigned int)gix;
      ty = (unsigned int)min(height-1, giy+1);
      tz = (unsigned int)max(0, giz-1);
      tidx = width*(tz*height + ty) + tx;
      sm[lix][liy+1][liz-1] = in[ tidx ];

      if(lix == 1)
      {
        tx = (unsigned int)max(0, gix-1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix-1][liy+1][liz-1] = in[ tidx ];
      }
      if(lix == BLOCK_SIZE || gix == width-1)
      {
        tx = (unsigned int)min(width-1, gix+1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix+1][liy+1][liz-1] = in[ tidx ];
      }
    }
    if((liy == BLOCK_SIZE || giy == height-1) && (liz == BLOCK_SIZE || giz == depth-1))
    {
      tx = (unsigned int)gix;
      ty = (unsigned int)min(height-1, giy+1);
      tz = (unsigned int)min(depth-1, giz+1);
      tidx = width*(tz*height + ty) + tx;
      sm[lix][liy+1][liz+1] = in[ tidx ];

      if(lix == 1)
      {
        tx = (unsigned int)max(0, gix-1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix-1][liy+1][liz+1] = in[ tidx ];
      }
      if(lix == BLOCK_SIZE || gix == width-1)
      {
        tx = (unsigned int)min(width-1, gix+1);
        tidx = width*(tz*height + ty) + tx;
        sm[lix+1][liy+1][liz+1] = in[ tidx ];
      }
    }
  }

  // synchronize shared memory
  barrier(CLK_LOCAL_MEM_FENCE);

  if (isValid)
  {
    // Compute Update
    // delta
    delta = 0;

    if(K > 0 || K < 0) // if K != 0
    {
      // centralized derivatives
      dx[0] = (sm[lix+1][liy][liz] - sm[lix-1][liy][liz])*0.5f*scalex;
      dx[1] = (sm[lix][liy+1][liz] - sm[lix][liy-1][liz])*0.5f*scaley;
      dx[2] = (sm[lix][liy][liz+1] - sm[lix][liy][liz-1])*0.5f*scalez;

      // along x
      dx_f = (sm[lix+1][liy][liz] - sm[lix][liy][liz])*scalex;
      dx_b = (sm[lix][liy][liz] - sm[lix-1][liy][liz])*scalex;

      Cx  = exp( (dx_f*dx_f + 0.25f*(pown(dx[1] + (sm[lix+1][liy+1][liz] - sm[lix+1][liy-1][liz])*0.5f*scaley, 2))
                            + 0.25f*(pown(dx[2] + (sm[lix+1][liy][liz+1] - sm[lix+1][liy][liz-1])*0.5f*scalez, 2))) / K );
      Cxd = exp( (dx_b*dx_b + 0.25f*(pown(dx[1] + (sm[lix-1][liy+1][liz] - sm[lix-1][liy-1][liz])*0.5f*scaley, 2))
                            + 0.25f*(pown(dx[2] + (sm[lix-1][liy][liz+1] - sm[lix-1][liy][liz-1])*0.5f*scalez, 2))) / K );
      delta += (Cx*dx_f - Cxd*dx_b);

      // along y
      dx_f = (sm[lix][liy+1][liz] - sm[lix][liy][liz])*scaley;
      dx_b = (sm[lix][liy][liz] - sm[lix][liy-1][liz])*scaley;

      Cx  = exp( (dx_f*dx_f + 0.25f*(pown(dx[0] + (sm[lix+1][liy+1][liz] - sm[lix-1][liy+1][liz])*0.5f*scalex, 2))
                            + 0.25f*(pown(dx[2] + (sm[lix][liy+1][liz+1] - sm[lix][liy+1][liz-1])*0.5f*scalez, 2))) / K );
      Cxd = exp( (dx_b*dx_b + 0.25f*(pown(dx[0] + (sm[lix+1][liy-1][liz] - sm[lix-1][liy-1][liz])*0.5f*scalex, 2))
                            + 0.25f*(pown(dx[2] + (sm[lix][liy-1][liz+1] - sm[lix][liy-1][liz-1])*0.5f*scalez, 2))) / K );
      delta += (Cx*dx_f - Cxd*dx_b);

      // along z
      dx_f = (sm[lix][liy][liz+1] - sm[lix][liy][liz])*scalez;
      dx_b = (sm[lix][liy][liz] - sm[lix][liy][liz-1])*scalez;

      Cx  = exp( (dx_f*dx_f + 0.25f*(pown(dx[0] + (sm[lix+1][liy][liz+1] - sm[lix-1][liy][liz+1])*0.5f*scalex, 2))
                            + 0.25f*(pown(dx[1] + (sm[lix][liy+1][liz+1] - sm[lix][liy-1][liz+1])*0.5f*scaley, 2))) / K );
      Cxd = exp( (dx_b*dx_b + 0.25f*(pown(dx[0] + (sm[lix+1][liy][liz-1] - sm[lix-1][liy][liz-1])*0.5f*scalex, 2))
                            + 0.25f*(pown(dx[1] + (sm[lix][liy+1][liz-1] - sm[lix][liy-1][liz-1])*0.5f*scaley, 2))) / K );
      delta += (Cx*dx_f - Cxd*dx_b);
    }

    buf[gidx] = delta;
  }

}
#endif
