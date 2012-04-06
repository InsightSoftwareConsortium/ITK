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
// Compute index for Global memory
//

// For 1 or 2 dimensional images, we have ysize = 1 and/or zsize = 1
#define MAXDIM 3

// Global memory order (indir/outdir)
//
//  0 (fast x) : x/y/z
//  1 (fast y) : y/z/x
//  2 (fast z) : z/x/y
int GetImageOffset(int order,
                    __constant int size[MAXDIM],
                    int index[MAXDIM])
{
  int idx;

  if(order == 0)
  {
    idx = (index[2]*size[1] + index[1])*size[0] + index[0];
  }
  else if(order == 1)
  {
    idx = (index[0]*size[2] + index[2])*size[1] + index[1];
  }
  else
  {
    idx = (index[1]*size[0] + index[0])*size[2] + index[2];
  }

  return idx;
}

//
// 3D block dimension is blockx*1*1 or 1*blocky*1 or 1*1*blockz
//
// For an image that is stored with the indir-th dimension as the consecutive dimension,
// smooth along the indir-th dimension, and store the result with the outdir-th dimension
// as the consecutive dimension.
__kernel void SmoothingFilterReorder(__global OUTPIXELTYPE *imgIn, __global OUTPIXELTYPE *imgOut,
                                    __constant int *imgSize, int dim,
                                    __constant OUTPIXELTYPE *filter, int filterSize,
                                    int indir, int outdir,
                                    __local volatile OUTPIXELTYPE *sharedFilter,
                                    __local volatile OUTPIXELTYPE *sharedData
                                    )
{
  if(get_global_id(0) >= imgSize[0] ||
     get_global_id(1) >= imgSize[1] ||
     get_global_id(2) >= imgSize[2])
    {
    return;
    }

  int blockSize = get_local_size(indir);
  int threadId  = get_local_id(indir);
  int index[MAXDIM];
  int tempIndex[MAXDIM];

  int component;
  int radius = filterSize / 2;
  OUTPIXELTYPE sum;
  int i;
  int lowerBound = 0;
  int upperBound = max(0, imgSize[indir] - 1);

  for (i = threadId; i < filterSize; i += blockSize)
    {
    sharedFilter[i] = filter[i];
    }

  for (i = 0; i<MAXDIM; i++)
    {
    index[i] = get_global_id(i);
    tempIndex[i] = get_global_id(i);
    }

  for (component = 0; component < dim; component ++ )
    {
    index[indir] -= radius;

    for (i = threadId; i < blockSize+filterSize-1; i += blockSize)
      {
        if (index[indir] < 0 )
        {
          tempIndex[indir] = lowerBound;
        }
        else if (index[indir] > upperBound)
        {
          tempIndex[indir] = upperBound;
        }
        else
        {
          tempIndex[indir] = index[indir];
        }
        sharedData[i] = imgIn[GetImageOffset(indir, imgSize, tempIndex) * dim + component];
        index[indir] += blockSize;
      }
    // wait for loading data to shared memory
    barrier(CLK_LOCAL_MEM_FENCE);

    // Compute the dot product
    sum = 0;
    for (i=0; i<filterSize; i++)
      {
        sum += sharedData[threadId+i] * sharedFilter[i];
      }
    index[indir] = get_global_id(indir);
    imgOut[GetImageOffset(outdir, imgSize, index) * dim + component] = sum;
//     imgOut[GetImageOffset(outdir, imgSize, index) * dim + component] = imgIn[GetImageOffset(indir, imgSize, index)*dim + component];
    } // for component
}

// Smoothe the deformation field without reordering dimensions in the storage
// outdir is no longer used here, but it is kept as an argument for compatibility
__kernel void SmoothingFilter(__global OUTPIXELTYPE *imgIn, __global OUTPIXELTYPE *imgOut,
                              __constant int *imgSize, int dim,
                              __constant OUTPIXELTYPE *filter, int filterSize,
                              int indir, int outdir,
                              __local volatile OUTPIXELTYPE *sharedFilter,
                              __local volatile OUTPIXELTYPE *sharedData
)
{
  if(get_global_id(0) >= imgSize[0] ||
     get_global_id(1) >= imgSize[1] ||
     get_global_id(2) >= imgSize[2])
    {
    return;
    }

  int blockSize = get_local_size(indir);
  int threadId  = get_local_id(indir);
  int index[MAXDIM];
  int tempIndex[MAXDIM];

  int component;
  int radius = filterSize / 2;
  OUTPIXELTYPE sum;
  int i, offset;
  int lowerBound = 0;
  int upperBound = max(0, imgSize[indir] - 1);


  for (i = threadId; i < filterSize; i += blockSize)
    {
    sharedFilter[i] = filter[i];
    }

  for (i = 0; i<MAXDIM; i++)
    {
    index[i] = get_global_id(i);
    tempIndex[i] = get_global_id(i);
    }

  for (component = 0; component < dim; component ++ )
    {
    index[indir] -= radius;

    for (i = threadId; i < blockSize+filterSize-1; i += blockSize)
      {
//       if (index[indir] < 0 || index[indir] >= imgSize[indir])
//         {
//         sharedData[i] = 0;
//         }
//       else
//         {
//         offset = (index[2]*imgSize[1] + index[1])*imgSize[0] + index[0];
//         sharedData[i] = imgIn[offset * dim + component];
//         }
        if (index[indir] < 0 )
        {
          tempIndex[indir] = lowerBound;
        }
        else if (index[indir] > upperBound)
        {
          tempIndex[indir] = upperBound;
        }
        else
        {
          tempIndex[indir] = index[indir];
        }
        offset = (tempIndex[2]*imgSize[1] + tempIndex[1])*imgSize[0] + tempIndex[0];
        sharedData[i] = imgIn[offset * dim + component];

      index[indir] += blockSize;
      }
    // wait for loading data to shared memory
    barrier(CLK_LOCAL_MEM_FENCE);

    // Compute the dot product
    sum = 0;
    for (i=0; i<filterSize; i++)
      {
      sum += sharedData[threadId+i] * sharedFilter[i];
      }
    index[indir] = get_global_id(indir);
    offset = (index[2]*imgSize[1] + index[1])*imgSize[0] + index[0];
    imgOut[offset * dim + component] = sum;
    } // for component
}
