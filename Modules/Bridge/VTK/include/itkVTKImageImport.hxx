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
#ifndef itkVTKImageImport_hxx
#define itkVTKImageImport_hxx

#include "itkVTKImageImport.h"
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
template< typename TOutputImage >
VTKImageImport< TOutputImage >
::VTKImageImport()
{
  typedef typename TOutputImage::PixelType             PixelType;
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
  else if ( typeid( ScalarType ) == typeid( signed char ) )
    {
    m_ScalarTypeName = "signed char";
    }
  else if ( typeid( ScalarType ) == typeid( unsigned char ) )
    {
    m_ScalarTypeName = "unsigned char";
    }
  else
    {
    itkExceptionMacro(<< "Type currently not supported");
    }
  m_DataExtentCallback = 0;
  m_WholeExtentCallback = 0;
  m_BufferPointerCallback = 0;
  m_UpdateDataCallback = 0;
  m_PipelineModifiedCallback = 0;
  m_NumberOfComponentsCallback = 0;
  m_SpacingCallback = 0;
  m_FloatSpacingCallback = 0;
  m_OriginCallback = 0;
  m_FloatOriginCallback = 0;
  m_UpdateInformationCallback = 0;
  m_ScalarTypeCallback = 0;
  m_DataExtentCallback = 0;
  m_PropagateUpdateExtentCallback = 0;
  m_CallbackUserData = 0;
}

/**
 *
 */
template< typename TOutputImage >
void
VTKImageImport< TOutputImage >
::PropagateRequestedRegion(DataObject *outputPtr)
{
  OutputImageType *output = dynamic_cast< OutputImageType * >( outputPtr );

  if ( !output )
    {
    itkExceptionMacro(<< "Downcast from DataObject to my Image type failed.");
    return;
    }
  Superclass::PropagateRequestedRegion(output);
  if ( m_PropagateUpdateExtentCallback )
    {
    OutputRegionType region = output->GetRequestedRegion();
    OutputSizeType   size = region.GetSize();
    OutputIndexType  index = region.GetIndex();

    int          updateExtent[6];
    unsigned int i = 0;
    for (; i < OutputImageDimension; ++i )
      {
      updateExtent[i * 2] = int(index[i]);
      updateExtent[i * 2 + 1] = int(index[i] + size[i]) - 1;
      }
    for (; i < 3; ++i )
      {
      updateExtent[i * 2] = 0;
      updateExtent[i * 2 + 1] = 0;
      }
    (m_PropagateUpdateExtentCallback)( m_CallbackUserData, updateExtent );
    }
}

template< typename TOutputImage >
void
VTKImageImport< TOutputImage >
::UpdateOutputInformation()
{
  // If set, use the callbacks to propagate pipeline update information.
  if ( m_UpdateInformationCallback )
    {
    (m_UpdateInformationCallback)( m_CallbackUserData );
    }
  if ( m_PipelineModifiedCallback )
    {
    if ( (m_PipelineModifiedCallback)( m_CallbackUserData ) )
      {
      this->Modified();
      }
    }

  Superclass::UpdateOutputInformation();
}

/**
 *
 */
template< typename TOutputImage >
void
VTKImageImport< TOutputImage >
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointer to the output
  OutputImagePointer output = this->GetOutput();

  if ( m_WholeExtentCallback )
    {
    int *           extent = (m_WholeExtentCallback)( m_CallbackUserData );
    OutputIndexType index;
    OutputSizeType  size;

    for ( unsigned int i = 0; i < OutputImageDimension; ++i )
      {
      index[i] = extent[i * 2];
      size[i] = ( extent[i * 2 + 1] - extent[i * 2] ) + 1;
      }

    OutputRegionType region;
    region.SetIndex(index);
    region.SetSize(size);
    output->SetLargestPossibleRegion(region);
    }
  if ( m_SpacingCallback )
    {
    double *inSpacing = (m_SpacingCallback)( m_CallbackUserData );
    typename TOutputImage::SpacingType outSpacing;
    for ( unsigned int i = 0; i < OutputImageDimension; ++i )
      {
      outSpacing[i] = inSpacing[i];
      }
    output->SetSpacing(outSpacing);
    }
  else if ( m_FloatSpacingCallback )
    {
    float *inSpacing = (m_FloatSpacingCallback)( m_CallbackUserData );
    typename TOutputImage::SpacingType outSpacing;
    for ( unsigned int i = 0; i < OutputImageDimension; ++i )
      {
      outSpacing[i] = inSpacing[i];
      }
    output->SetSpacing(outSpacing);
    }
  if ( m_OriginCallback )
    {
    double *inOrigin = (m_OriginCallback)( m_CallbackUserData );
    typename TOutputImage::PointType outOrigin;
    for ( unsigned int i = 0; i < OutputImageDimension; ++i )
      {
      outOrigin[i] = inOrigin[i];
      }
    output->SetOrigin(outOrigin);
    }
  else if ( m_FloatOriginCallback )
    {
    float *inOrigin = (m_FloatOriginCallback)( m_CallbackUserData );
    typename TOutputImage::PointType outOrigin;
    for ( unsigned int i = 0; i < OutputImageDimension; ++i )
      {
      outOrigin[i] = inOrigin[i];
      }
    output->SetOrigin(outOrigin);
    }
  if ( m_NumberOfComponentsCallback )
    {
    const unsigned int components =
      (m_NumberOfComponentsCallback)( m_CallbackUserData );

    typedef typename TOutputImage::PixelType             PixelType;
    const unsigned int estimatedNumberOfComponents = NumericTraits< PixelType >::GetLength();

    if ( components != estimatedNumberOfComponents )
      {
      itkExceptionMacro(<< "Input number of components is " << components
                        << " but should be " << estimatedNumberOfComponents);
      }
    }
  if ( m_ScalarTypeCallback )
    {
    const char *scalarName = (m_ScalarTypeCallback)( m_CallbackUserData );
    if ( scalarName != m_ScalarTypeName )
      {
      itkExceptionMacro( << "Input scalar type is " << scalarName
                         << " but should be " << m_ScalarTypeName.c_str() );
      }
    }
}

/**
 *
 */
template< typename TOutputImage >
void
VTKImageImport< TOutputImage >
::GenerateData()
{
  // Normally, GenerateData() allocates memory.  However, the application
  // provides the memory for this filter via the SetImportPointer() method.
  // Therefore, this filter does not call outputPtr->Allocate().

  if ( m_UpdateDataCallback )
    {
    (m_UpdateDataCallback)( m_CallbackUserData );
    }
  if ( m_DataExtentCallback && m_BufferPointerCallback )
    {
    // get pointer to the output
    OutputImagePointer output = this->GetOutput();

    int *           extent = (m_DataExtentCallback)( m_CallbackUserData );
    OutputIndexType index;
    OutputSizeType  size;

    SizeValueType importSize = 1;
    for ( unsigned int i = 0; i < OutputImageDimension; ++i )
      {
      index[i] = extent[i * 2];
      size[i] = ( extent[i * 2 + 1] - extent[i * 2] ) + 1;
      importSize *= size[i];
      }

    OutputRegionType region;
    region.SetIndex(index);
    region.SetSize(size);
    output->SetBufferedRegion(region);

    void *           data = (m_BufferPointerCallback)( m_CallbackUserData );
    OutputPixelType *importPointer = reinterpret_cast< OutputPixelType * >( data );

    // pass the pointer down to the output container during each
    // Update() since a call to Initialize() causes the container to
    // forget the pointer.  Note that we tell the container NOT to
    // manage memory itself.  We leave the responsibility of
    // deallocating the pixel data to VTK.
    output->GetPixelContainer()
    ->SetImportPointer(importPointer, importSize, false);
    }
}

template< typename TOutputImage >
void
VTKImageImport< TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  if ( m_DataExtentCallback )
    {
    os << "DataExtentCallback: " << m_DataExtentCallback << std::endl;
    }
  if ( m_WholeExtentCallback )
    {
    os << "WholeExtentCallback: " << m_WholeExtentCallback << std::endl;
    }
  if ( m_BufferPointerCallback )
    {
    os << "BufferPointerCallback: " << m_BufferPointerCallback << std::endl;
    }
  if ( m_UpdateDataCallback )
    {
    os << "UpdateDataCallback: " << m_UpdateDataCallback << std::endl;
    }
  if ( m_PipelineModifiedCallback )
    {
    os << "PipelineModifiedCallback: " << m_PipelineModifiedCallback << std::endl;
    }
  if ( m_NumberOfComponentsCallback )
    {
    os << "NumberOfComponentsCallback: " << m_NumberOfComponentsCallback << std::endl;
    }
  if ( m_SpacingCallback )
    {
    os << "SpacingCallback: " << m_SpacingCallback << std::endl;
    }
  if ( m_FloatSpacingCallback )
    {
    os << "FloatSpacingCallback: " << m_FloatSpacingCallback << std::endl;
    }
  if ( m_OriginCallback )
    {
    os << "OriginCallback: " << m_OriginCallback << std::endl;
    }
  if ( m_FloatOriginCallback )
    {
    os << "FloatOriginCallback: " << m_FloatOriginCallback << std::endl;
    }
  if ( m_UpdateInformationCallback )
    {
    os << "UpdateInformationCallback: " << m_UpdateInformationCallback << std::endl;
    }
  if ( m_ScalarTypeCallback )
    {
    os << "ScalarTypeCallback: " << m_ScalarTypeCallback << std::endl;
    }
  if ( m_PropagateUpdateExtentCallback )
    {
    os << "PropagateUpdateExtentCallback: " << m_PropagateUpdateExtentCallback << std::endl;
    }
  if ( m_CallbackUserData )
    {
    os << "CallbackUserData: " << m_CallbackUserData << std::endl;
    }
}
} // namespace itk

#endif
