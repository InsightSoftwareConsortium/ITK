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
#ifndef __itkPyBuffer_hxx
#define __itkPyBuffer_hxx

#include "itkPyBuffer.h"
#include "itkPixelTraits.h"

// Deal with slight incompatibilites between NumPy (the future, hopefully),
// Numeric (old version) and Numarray's Numeric compatibility module (also old).
#ifndef NDARRAY_VERSION
// NDARRAY_VERSION is only defined by NumPy's arrayobject.h
// In non NumPy arrayobject.h files, PyArray_SBYTE is used instead of BYTE.
#define PyArray_BYTE PyArray_SBYTE
#endif

namespace itk
{

template<class TImage>
PyObject *
PyBuffer<TImage>
::GetArrayFromImage( ImageType * image )
{
  if( !image )
    {
    throw std::runtime_error("Input image is null");
    }

  image->Update();

  PixelType * buffer = const_cast < PixelType * > ( image->GetBufferPointer() );

  char * data = (char *)( buffer );

  int dimensions[ ImageDimension ];

  SizeType size = image->GetBufferedRegion().GetSize();

  for(unsigned int d=0; d < ImageDimension; d++ )
    {
    dimensions[ImageDimension - d - 1] = size[d];
    }

  int item_type = GetPyType();  // TODO find a way of doing this through pixel traits
  // figure out an appropriate type

  PyObject * obj = PyArray_FromDimsAndData( ImageDimension, dimensions, item_type, data );

  return obj;
}

template<class TImage>
const typename PyBuffer<TImage>::ImagePointer
PyBuffer<TImage>
::GetImageFromArray( PyObject *obj )
{

    int element_type = GetPyType();  ///PyArray_DOUBLE;  // change this with pixel traits.

    PyArrayObject * parray =
          (PyArrayObject *) PyArray_ContiguousFromObject(
                                                    obj,
                                                    element_type,
                                                    ImageDimension,
                                                    ImageDimension  );

    if( parray == NULL )
      {
      throw std::runtime_error("Contiguous array couldn't be created from input python object");
      }

    const unsigned int imageDimension = parray->nd;

    SizeType size;

    unsigned int numberOfPixels = 1;

    for( unsigned int d=0; d<imageDimension; d++ )
      {
      size[imageDimension - d - 1]         = parray->dimensions[d];
      numberOfPixels *= parray->dimensions[d];
      }

    IndexType start;
    start.Fill( 0 );

    RegionType region;
    region.SetIndex( start );
    region.SetSize( size );

    PointType origin;
    origin.Fill( 0.0 );

    SpacingType spacing;
    spacing.Fill( 1.0 );

    ImporterPointer importer = ImporterType::New();
    importer->SetRegion( region );
    importer->SetOrigin( origin );
    importer->SetSpacing( spacing );

    const bool importImageFilterWillOwnTheBuffer = false;

    PixelType * data = (PixelType *)parray->data;

    importer->SetImportPointer(
                        data,
                        numberOfPixels,
                        importImageFilterWillOwnTheBuffer );

    importer->Update();
    ImagePointer output = importer->GetOutput();
    output->DisconnectPipeline();

    return output;
}

template<class TImage>
typename PyBuffer<TImage>::PyArrayType
PyBuffer<TImage>
::GetPyType(void)
{
  PyArrayType item_type;
  typedef typename PixelTraits< PixelType >::ValueType    ScalarType;
  if(typeid(ScalarType) == typeid(double))
    {
    item_type = PyArray_DOUBLE;
    }
  else if(typeid(ScalarType) == typeid(float))
    {
    item_type = PyArray_FLOAT;
    }
  else if(typeid(ScalarType) == typeid(long))
    {
    item_type = PyArray_LONG;
    }
  else if(typeid(ScalarType) == typeid(unsigned long))
    {
#ifdef NDARRAY_VERSION
    item_type = PyArray_ULONG;
#else
    throw std::runtime_error("Type currently not supported");
#endif
    }
  else if(typeid(ScalarType) == typeid(int))
    {
    item_type = PyArray_INT;
    }
  else if(typeid(ScalarType) == typeid(unsigned int))
    {
    item_type = PyArray_UINT;
    }
  else if(typeid(ScalarType) == typeid(short))
    {
    item_type = PyArray_SHORT;
    }
  else if(typeid(ScalarType) == typeid(unsigned short))
    {
    item_type = PyArray_USHORT;
    }
  else if(typeid(ScalarType) == typeid(signed char))
    {
    item_type = PyArray_BYTE;
    }
  else if(typeid(ScalarType) == typeid(unsigned char))
    {
    item_type = PyArray_UBYTE;
    }
  else
    {
    item_type = PyArray_NOTYPE;
    throw std::runtime_error("Type currently not supported");
    }
  return item_type;
}

} // namespace itk

#endif
