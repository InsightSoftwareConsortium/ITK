/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPyBuffer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPyBuffer_txx
#define _itkPyBuffer_txx

#include "itkPyBuffer.h"

namespace itk
{

template<typename TImage>
PyBuffer<TImage>
::PyBuffer()
{
    this->obj = NULL;
    this->m_Importer = ImporterType::New();
}

template<typename TImage>
PyBuffer<TImage>
::~PyBuffer()
{
    if (this->obj)
    {
        Py_DECREF(this->obj);
    }
    this->obj = NULL;
}

    
template<typename TImage>
PyObject * 
PyBuffer<TImage>
::GetArrayFromImage( const ImageType * image )
{
   if( !image )
     {
     itkExceptionMacro("Input image is null");
     }

   PixelType * buffer = const_cast< PixelType *>( image->GetBufferPointer() );

   char * data = (char *)( buffer );
   
   int dimensions[ ImageDimension ];

   typedef typename ImageType::SizeType     SizeType;

   SizeType size = image->GetBufferedRegion().GetSize();

   for(unsigned int d=0; d < ImageDimension; d++ )
     {
     dimensions[d] = size[d];
     }

   int item_type = 0;  // TODO find a way of doing this through pixel traits
   
   this->obj = PyArray_FromDimsAndData( ImageDimension, dimensions, item_type, data );

   return this->obj;
}



template<typename TImage>
const typename PyBuffer<TImage>::ImageType * 
PyBuffer<TImage>
::GetImageFromArray( PyObject *obj )
{
    if (obj != this->obj)
    {
        if (this->obj)
        {
            // get rid of our reference
            Py_DECREF(this->obj);
        }

        // store the new object
        this->obj = obj;

        if (this->obj)
        {
            // take out reference (so that the calling code doesn't
            // have to keep a binding to the callable around)
            Py_INCREF(this->obj);
        }
    }
    return this->m_Importer->GetOutput();
}



} // namespace itk

#endif

