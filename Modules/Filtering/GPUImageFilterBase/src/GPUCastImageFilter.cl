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
//
// \author Denis P. Shamonin and Marius Staring. Division of Image Processing,
// Department of Radiology, Leiden, The Netherlands
//
// \note This work was funded by the Netherlands Organisation for
// Scientific Research (NWO NRG-2010.02 and NWO 639.021.124).
//
// OpenCL implementation of itk::CastImageFilter

//------------------------------------------------------------------------------
// Apple OpenCL 1.0 support function
bool is_valid_3d( const uint3 index, const uint3 size )
{
  /* NOTE: More than three-level nested conditional statements (e.g.,
  if A && B && C..) invalidates command queue during kernel
  execution on Apple OpenCL 1.0 (such Macbook Pro with NVIDIA 9600M
  GT). Therefore, we flattened conditional statements. */
  if ( index.x >= size.x ) { return false; }
  if ( index.y >= size.y ) { return false; }
  if ( index.z >= size.z ) { return false; }
  return true;
}

//------------------------------------------------------------------------------
OUTPIXELTYPE Functor( const INPIXELTYPE in )
{
  // Cast it and return
  OUTPIXELTYPE out = (OUTPIXELTYPE)in;

  return out;
}

//------------------------------------------------------------------------------
#ifdef DIM_1
__kernel void CastImageFilter( __global const INPIXELTYPE *in,
  __global OUTPIXELTYPE *out,
  uint width )
{
  uint index = get_global_id( 0 );

  if ( index < width )
  {
    out[index] = Functor( in[index] );
  }
}

#endif

//------------------------------------------------------------------------------
#ifdef DIM_2
__kernel void CastImageFilter( __global const INPIXELTYPE *in,
  __global OUTPIXELTYPE *out,
  uint width, uint height )
{
  uint2 index = (uint2)( get_global_id( 0 ), get_global_id( 1 ) );
  uint2 size = (uint2)( width, height );

  if ( index.x < width && index.y < height )
  {
    uint gidx = mad24( size.x, index.y, index.x );
    out[gidx] = Functor( in[gidx] );
  }
}

#endif

//------------------------------------------------------------------------------
#ifdef DIM_3
__kernel void CastImageFilter( __global const INPIXELTYPE *in,
  __global OUTPIXELTYPE *out,
  uint width, uint height, uint depth )
{
  uint3 index = (uint3)( get_global_id( 0 ), get_global_id( 1 ), get_global_id( 2 ) );
  uint3 size = (uint3)( width, height, depth );

  if ( is_valid_3d( index, size ) )
  {
    uint gidx = mad24( size.x, mad24( index.z, size.y, index.y ), index.x );
    out[gidx] = Functor( in[gidx] );
  }
}

#endif
