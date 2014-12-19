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
#ifndef itkImageToRGBVTKImageFilter_hxx
#define itkImageToRGBVTKImageFilter_hxx

#include "vtkVersion.h"

#include "itkImageToRGBVTKImageFilter.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage >
ImageToRGBVTKImageFilter< TInputImage >
::ImageToRGBVTKImageFilter()
{
  m_Output = vtkSmartPointer< vtkImageData >::New();
}

/**
 * Destructor
 */
template< typename TInputImage >
ImageToRGBVTKImageFilter< TInputImage >
::~ImageToRGBVTKImageFilter()
{}

/**
 * Set an itk::Image as input
 */
template< typename TInputImage >
void
ImageToRGBVTKImageFilter< TInputImage >
::SetInput(const InputImageType *inputImage)
{
  m_Input = inputImage;
  this->Modified();
}

/**
 * Get a vtkImage as output
 */
template< typename TInputImage >
vtkSmartPointer< vtkImageData >
ImageToRGBVTKImageFilter< TInputImage >
::GetOutput() const
{
  return m_Output;
}

/**
 * Delegate the Update to the importer
 */
template< typename TInputImage >
void
ImageToRGBVTKImageFilter< TInputImage >
::Update()
{
  int dimension[3];
  dimension[0] = 0;
  dimension[1] = 0;
  dimension[2] = 0;

  double spacing[3];
  spacing[0] = 0.;
  spacing[1] = 0.;
  spacing[2] = 0.;

  InputRegionType region = m_Input->GetLargestPossibleRegion();
  InputSizeType itk_size = region.GetSize();

  InputSpacingType itk_spacing = m_Input->GetSpacing();

  for( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
    {
    dimension[i] = static_cast< int >( itk_size[i] );
    spacing[i] = static_cast< double >( itk_spacing[i] );
    }
  m_Output->SetDimensions( dimension );
  m_Output->SetSpacing( spacing );
  m_Output->SetExtent( 0, dimension[0],
                       0, dimension[1],
                       0, dimension[2] );
#if VTK_MAJOR_VERSION <= 5
  m_Output->SetNumberOfScalarComponents( 3 );

  // at first let's convert it to unsigned char
  m_Output->SetScalarTypeToUnsignedChar();
  m_Output->AllocateScalars();
#else
  m_Output->AllocateScalars(VTK_UNSIGNED_CHAR,3);
#endif
  //TODO: use itk iterators instead
  for( int x = 0; x < dimension[0]; x++ )
    {
    for( int y = 0; y < dimension[1]; y++ )
      {
      if( TInputImage::ImageDimension == 3 )
        {
        for( int z = 0; z < dimension[2]; z++ )
          {
          InputPixelType* vtkpixel =
              static_cast<InputPixelType*>(m_Output->GetScalarPointer(x,y,z));
          InputIndexType index;
          index[0] = x;
          index[1] = y;
          index[2] = z;

          InputPixelType itkpixel = m_Input->GetPixel(index);
          vtkpixel[0] = itkpixel;
          vtkpixel[1] = itkpixel;
          vtkpixel[2] = itkpixel;
          }
        }
      else
        {
        InputPixelType* vtkpixel =
            static_cast<InputPixelType*>(m_Output->GetScalarPointer(x,y,0));
        InputIndexType index;
        index[0] = x;
        index[1] = y;

        InputPixelType itkpixel = m_Input->GetPixel(index);
        vtkpixel[0] = itkpixel;
        vtkpixel[1] = itkpixel;
        vtkpixel[2] = itkpixel;
        }
      }
    }
}

} // end namespace itk

#endif
