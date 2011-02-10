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
// pixel by pixel addition of 2D images
//
__kernel void ImageAdd(__global const PIXELTYPE* a, __global const PIXELTYPE* b, __global PIXELTYPE* c, unsigned int nElem)
{
    unsigned int width = get_global_size(0);
    unsigned int gix = get_global_id(0);
 unsigned int giy = get_global_id(1);
 unsigned int gidx = giy*width + gix;

    // bound check
    if (gidx < nElem)
    {
  c[gidx] = a[gidx] + b[gidx];
    }
}

//
// pixel by pixel subtraction of 2D images
//
__kernel void ImageSub(__global const PIXELTYPE* a, __global const PIXELTYPE* b, __global PIXELTYPE* c, unsigned int nElem)
{
    unsigned int width = get_global_size(0);
    unsigned int gix = get_global_id(0);
 unsigned int giy = get_global_id(1);

 unsigned int gidx = giy*width + gix;

    // bound check
    if (gidx < nElem)
    {
  c[gidx] = a[gidx] - b[gidx];
    }
}

//
// pixel by pixel multiplication of 2D images
//
__kernel void ImageMult(__global const PIXELTYPE* a, __global const PIXELTYPE* b, __global PIXELTYPE* c, unsigned int nElem)
{
    unsigned int width = get_global_size(0);
    unsigned int gix = get_global_id(0);
 unsigned int giy = get_global_id(1);

 unsigned int gidx = giy*width + gix;

    // bound check
    if (gidx < nElem)
    {
  c[gidx] = a[gidx] * b[gidx];
    }
}

//
// pixel by pixel division of 2D images
//
__kernel void ImageDiv(__global const PIXELTYPE* a, __global const PIXELTYPE* b, __global PIXELTYPE* c, unsigned int nElem)
{
    unsigned int width = get_global_size(0);
    unsigned int gix = get_global_id(0);
 unsigned int giy = get_global_id(1);

 unsigned int gidx = giy*width + gix;

    // bound check
    if (gidx < nElem)
    {
  c[gidx] = a[gidx] / b[gidx];
    }
}
