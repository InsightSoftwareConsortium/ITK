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
#ifndef itkVTKImageExport_hxx
#define itkVTKImageExport_hxx

#include "itkVTKImageExport.h"

#include "itkPixelTraits.h"
#include "itkNumericTraits.h"
#include "itkNumericTraitsArrayPixel.h"
#include "itkNumericTraitsCovariantVectorPixel.h"
#include "itkNumericTraitsDiffusionTensor3DPixel.h"
#include "itkNumericTraitsFixedArrayPixel.h"
#include "itkNumericTraitsPointPixel.h"
#include "itkNumericTraitsRGBPixel.h"
#include "itkNumericTraitsRGBAPixel.h"
#include "itkNumericTraitsTensorPixel.h"
#include "itkNumericTraitsVectorPixel.h"

namespace itk
{
/**
 * The constructor records the name of the pixel's scalar type for the
 * image to be sent to vtkImageImport's ScalarTypeCallback.
 */
template< typename TInputImage >
VTKImageExport< TInputImage >::VTKImageExport()
{
  typedef typename TInputImage::PixelType              PixelType;
  typedef typename PixelTraits< PixelType >::ValueType ScalarType;

  if ( typeid( ScalarType ) == typeid( double ) )
    {
    m_ScalarTypeName = "double";
    }
  else if ( typeid( ScalarType ) == typeid( float ) )
    {
    m_ScalarTypeName = "float";
    }
  else if ( typeid( ScalarType ) == typeid( long ) )
    {
    m_ScalarTypeName = "long";
    }
  else if ( typeid( ScalarType ) == typeid( unsigned long ) )
    {
    m_ScalarTypeName = "unsigned long";
    }
  else if ( typeid( ScalarType ) == typeid( int ) )
    {
    m_ScalarTypeName = "int";
    }
  else if ( typeid( ScalarType ) == typeid( unsigned int ) )
    {
    m_ScalarTypeName = "unsigned int";
    }
  else if ( typeid( ScalarType ) == typeid( short ) )
    {
    m_ScalarTypeName = "short";
    }
  else if ( typeid( ScalarType ) == typeid( unsigned short ) )
    {
    m_ScalarTypeName = "unsigned short";
    }
  else if ( typeid( ScalarType ) == typeid( char ) )
    {
    m_ScalarTypeName = "char";
    }
  else if ( typeid( ScalarType ) == typeid( unsigned char ) )
    {
    m_ScalarTypeName = "unsigned char";
    }
  else if ( typeid( ScalarType ) == typeid( signed char ) )
    {
    m_ScalarTypeName = "signed char";
    }
  else
    {
    itkExceptionMacro(<< "Type currently not supported");
    }
}

template< typename TInputImage >
void VTKImageExport< TInputImage >::PrintSelf(std::ostream & os,
                                              Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/**
 * Set the input image for this filter.
 */
template< typename TInputImage >
void VTKImageExport< TInputImage >::SetInput(const InputImageType *input)
{
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< TInputImage * >( input ) );
}

/**
 * Get the current input image.
 */
template< typename TInputImage >
typename VTKImageExport< TInputImage >::InputImageType *
VTKImageExport< TInputImage >::GetInput(void)
{
  return itkDynamicCastInDebugMode< TInputImage * >( this->ProcessObject::GetInput(0) );
}

/**
 * Implements the WholeExtentCallback.  This returns a pointer to an
 * array of six integers describing the VTK-style extent of the image.
 * This corresponds to the ITK image's LargestPossibleRegion.
 */
template< typename TInputImage >
int *VTKImageExport< TInputImage >::WholeExtentCallback()
{
  InputImagePointer input = this->GetInput();

  if ( !input )
    {
    itkExceptionMacro(<< "Need to set an input");
    }

  InputRegionType region = input->GetLargestPossibleRegion();
  InputSizeType   size = region.GetSize();
  InputIndexType  index = region.GetIndex();

  unsigned int i = 0;
  // Fill in the known portion of the extent.
  for (; i < InputImageDimension; ++i )
    {
    m_WholeExtent[i * 2] = int(index[i]);
    m_WholeExtent[i * 2 + 1] = int(index[i] + size[i]) - 1;
    }
  // Fill in the extent for dimensions up to three.
  for (; i < 3; ++i )
    {
    m_WholeExtent[i * 2] = 0;
    m_WholeExtent[i * 2 + 1] = 0;
    }
  return m_WholeExtent;
}

/**
 * Implements the SpacingCallback.  This returns a pointer to an array
 * of three floating point values describing the spacing of the image.
 */
template< typename TInputImage >
double *VTKImageExport< TInputImage >::SpacingCallback()
{
  InputImagePointer input = this->GetInput();

  if ( !input )
    {
    itkExceptionMacro(<< "Need to set an input");
    }

  const typename TInputImage::SpacingType & spacing = input->GetSpacing();

  unsigned int i = 0;
  // Fill in the known portion of the spacing.
  for (; i < InputImageDimension; ++i )
    {
    m_DataSpacing[i] = static_cast< double >( spacing[i] );
    }
  // Fill up the spacing with defaults up to three dimensions.
  for (; i < 3; ++i )
    {
    m_DataSpacing[i] = 1;
    }
  return m_DataSpacing;
}

/**
 * Implements the SpacingCallback.  This returns a pointer to an array
 * of three floating point values describing the spacing of the image.
 */
template< typename TInputImage >
float *VTKImageExport< TInputImage >::FloatSpacingCallback()
{
  InputImagePointer input = this->GetInput();

  const typename TInputImage::SpacingType & spacing = input->GetSpacing();

  unsigned int i = 0;
  // Fill in the known portion of the spacing.
  for (; i < InputImageDimension; ++i )
    {
    m_FloatDataSpacing[i] = static_cast< float >( spacing[i] );
    }
  // Fill up the spacing with defaults up to three dimensions.
  for (; i < 3; ++i )
    {
    m_FloatDataSpacing[i] = 1;
    }
  return m_FloatDataSpacing;
}

/**
 * Implements the OriginCallback.  This returns a pointer to an array
 * of three floating point values describing the origin of the image.
 */
template< typename TInputImage >
double *VTKImageExport< TInputImage >::OriginCallback()
{
  InputImagePointer input = this->GetInput();

  if ( !input )
    {
    itkExceptionMacro(<< "Need to set an input");
    }

  const typename TInputImage::PointType & origin = input->GetOrigin();

  unsigned int i = 0;
  // Fill in the known portion of the origin.
  for (; i < InputImageDimension; ++i )
    {
    m_DataOrigin[i] = static_cast< double >( origin[i] );
    }
  // Fill up the origin with defaults up to three dimensions.
  for (; i < 3; ++i )
    {
    m_DataOrigin[i] = 0;
    }
  return m_DataOrigin;
}

/**
 * Implements the OriginCallback.  This returns a pointer to an array
 * of three floating point values describing the origin of the image.
 */
template< typename TInputImage >
float *VTKImageExport< TInputImage >::FloatOriginCallback()
{
  InputImagePointer input = this->GetInput();

  const typename TInputImage::PointType & origin = input->GetOrigin();

  unsigned int i = 0;
  // Fill in the known portion of the origin.
  for (; i < InputImageDimension; ++i )
    {
    m_FloatDataOrigin[i] = static_cast< float >( origin[i] );
    }
  // Fill up the origin with defaults up to three dimensions.
  for (; i < 3; ++i )
    {
    m_FloatDataOrigin[i] = 0;
    }
  return m_FloatDataOrigin;
}

/**
 * Implements the ScalarTypeCallback.  This returns the name of the
 * scalar value type of the image.
 */
template< typename TInputImage >
const char *VTKImageExport< TInputImage >::ScalarTypeCallback()
{
  return m_ScalarTypeName.c_str();
}

/**
 * Implements the NumberOfComponentsCallback.  This returns the number of
 * components per pixel for the image.
 *
 * Currently, only one pixel component is supported by this class.
 */
template< typename TInputImage >
int VTKImageExport< TInputImage >::NumberOfComponentsCallback()
{
  typedef typename TInputImage::PixelType PixelType;
  return static_cast< int >( NumericTraits< PixelType >::GetLength() );
}

/**
 * Implements the PropagateUpdateExtentCallback.  This takes the
 * update extent from VTK and translates it into a RequestedRegion for
 * the input image.  This requested region is then propagated through
 * the ITK pipeline.
 */
template< typename TInputImage >
void VTKImageExport< TInputImage >::PropagateUpdateExtentCallback(int *extent)
{
  InputSizeType  size;
  InputIndexType index;

  for ( unsigned int i = 0; i < InputImageDimension; ++i )
    {
    index[i] = extent[i * 2];
    size[i] = ( extent[i * 2 + 1] - extent[i * 2] ) + 1;
    }

  InputRegionType region;
  region.SetSize(size);
  region.SetIndex(index);

  InputImagePointer input = this->GetInput();
  if ( !input )
    {
    itkExceptionMacro(<< "Need to set an input");
    }

  input->SetRequestedRegion(region);
}

/**
 * Implements the DataExtentCallback.  This returns a pointer to an
 * array of six integers describing the VTK-style extent of the
 * buffered portion of the image.  This corresponds to the ITK image's
 * BufferedRegion.
 */
template< typename TInputImage >
int *VTKImageExport< TInputImage >::DataExtentCallback()
{
  InputImagePointer input = this->GetInput();

  if ( !input )
    {
    itkExceptionMacro(<< "Need to set an input");
    }

  InputRegionType region = input->GetBufferedRegion();
  InputSizeType   size = region.GetSize();
  InputIndexType  index = region.GetIndex();

  unsigned int i = 0;
  for (; i < InputImageDimension; ++i )
    {
    m_DataExtent[i * 2] = int(index[i]);
    m_DataExtent[i * 2 + 1] = int(index[i] + size[i]) - 1;
    }
  for (; i < 3; ++i )
    {
    m_DataExtent[i * 2] = 0;
    m_DataExtent[i * 2 + 1] = 0;
    }
  return m_DataExtent;
}

/**
 * Implements the BufferPointerCallback.  This returns a pointer to
 * the image's memory buffer.
 */
template< typename TInputImage >
void *VTKImageExport< TInputImage >::BufferPointerCallback()
{
  InputImagePointer input = this->GetInput();

  if ( !input )
    {
    itkExceptionMacro(<< "Need to set an input");
    }

  return input->GetBufferPointer();
}
} // end namespace itk

#endif
