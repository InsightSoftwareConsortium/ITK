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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkImageToImageFilter_hxx
#define itkImageToImageFilter_hxx
#include "itkImageToImageFilter.h"
#include "itkInputDataObjectIterator.h"

#include <cmath>

namespace itk
{

template< typename TInputImage, typename TOutputImage >
ImageToImageFilter< TInputImage, TOutputImage >
::ImageToImageFilter() : m_CoordinateTolerance(Self::GetGlobalDefaultCoordinateTolerance()),
                         m_DirectionTolerance(Self::GetGlobalDefaultDirectionTolerance())
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);
}


template< typename TInputImage, typename TOutputImage >
ImageToImageFilter< TInputImage, TOutputImage >
::~ImageToImageFilter()
{}


template< typename TInputImage, typename TOutputImage >
void
ImageToImageFilter< TInputImage, TOutputImage >
::SetInput(const InputImageType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< InputImageType * >( input ) );
}


template< typename TInputImage, typename TOutputImage >
void
ImageToImageFilter< TInputImage, TOutputImage >
::SetInput(unsigned int index, const TInputImage *image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( index,
                                    const_cast< TInputImage * >( image ) );
}


template< typename TInputImage, typename TOutputImage >
const typename ImageToImageFilter< TInputImage, TOutputImage >::InputImageType *
ImageToImageFilter< TInputImage, TOutputImage >
::GetInput(void) const
{
  return itkDynamicCastInDebugMode< const TInputImage * >( this->GetPrimaryInput() );
}


template< typename TInputImage, typename TOutputImage >
const typename ImageToImageFilter< TInputImage, TOutputImage >::InputImageType *
ImageToImageFilter< TInputImage, TOutputImage >
::GetInput(unsigned int idx) const
{
  const TInputImage *in = dynamic_cast< const TInputImage * >
    ( this->ProcessObject::GetInput(idx) );

  if ( in == ITK_NULLPTR && this->ProcessObject::GetInput(idx) != ITK_NULLPTR )
    {
    itkWarningMacro (<< "Unable to convert input number " << idx << " to type " <<  typeid( InputImageType ).name () );
    }
  return in;
}


template< typename TInputImage, typename TOutputImage >
void
ImageToImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  for( InputDataObjectIterator it( this ); !it.IsAtEnd(); it++ )
    {
    // Check whether the input is an image of the appropriate dimension
    typedef ImageBase< InputImageDimension > ImageBaseType;
    ImageBaseType * input = dynamic_cast< ImageBaseType * >( it.GetInput() );
    if ( input )
      {
      // Use the function object RegionCopier to copy the output region
      // to the input.  The default region copier has default implementations
      // to handle the cases where the input and output are the same
      // dimension, the input a higher dimension than the output, and the
      // input a lower dimension than the output.
      typename ImageBaseType::RegionType inputRegion;
      this->CallCopyOutputRegionToInputRegion( inputRegion, this->GetOutput()->GetRequestedRegion() );
      input->SetRequestedRegion(inputRegion);
      }
    }
}


template< typename TInputImage, typename TOutputImage >
void
ImageToImageFilter< TInputImage, TOutputImage >
::CallCopyOutputRegionToInputRegion(InputImageRegionType & destRegion,
                                    const OutputImageRegionType & srcRegion)
{
  OutputToInputRegionCopierType regionCopier;

  regionCopier(destRegion, srcRegion);
}


template< typename TInputImage, typename TOutputImage >
void
ImageToImageFilter< TInputImage, TOutputImage >
::CallCopyInputRegionToOutputRegion(OutputImageRegionType & destRegion,
                                    const InputImageRegionType & srcRegion)
{
  InputToOutputRegionCopierType regionCopier;

  regionCopier(destRegion, srcRegion);
}


template< typename TInputImage, typename TOutputImage >
void
ImageToImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "CoordinateTolerance: "
     << this->m_CoordinateTolerance << std::endl;
  os << indent << "DirectionTolerance: "
     << this->m_DirectionTolerance << std::endl;
}


template< typename TInputImage, typename TOutputImage >
void
ImageToImageFilter< TInputImage, TOutputImage >
::VerifyInputInformation()
{

  typedef ImageBase< InputImageDimension > ImageBaseType;

  ImageBaseType *inputPtr1 = ITK_NULLPTR;
  InputDataObjectIterator it(this);

  for(; !it.IsAtEnd(); ++it )
    {
    // Check whether the output is an image of the appropriate
    // dimension (use ProcessObject's version of the GetInput()
    // method since it returns the input as a pointer to a
    // DataObject as opposed to the subclass version which
    // static_casts the input to an TInputImage).
    inputPtr1 = dynamic_cast< ImageBaseType * >( it.GetInput() );

    if ( inputPtr1 )
      {
      break;
      }
    }


  for (; !it.IsAtEnd(); ++it )
    {
    ImageBaseType *inputPtrN =
      dynamic_cast< ImageBaseType * >( it.GetInput() );

    // Physical space computation only matters if we're using two
    // images, and not an image and a constant.
    if( inputPtrN )
      {
      // check that the image occupy the same physical space, and that
      // each index is at the same physical location

      // tolerance for origin and spacing depends on the size of pixel
      // tolerance for directions a fraction of the unit cube.
      const SpacePrecisionType coordinateTol
        = std::abs(this->m_CoordinateTolerance * inputPtr1->GetSpacing()[0]); // use first dimension spacing

      if ( !inputPtr1->GetOrigin().GetVnlVector().is_equal(inputPtrN->GetOrigin().GetVnlVector(), coordinateTol) ||
           !inputPtr1->GetSpacing().GetVnlVector().is_equal(inputPtrN->GetSpacing().GetVnlVector(), coordinateTol) ||
           !inputPtr1->GetDirection().GetVnlMatrix().as_ref().is_equal(inputPtrN->GetDirection().GetVnlMatrix(), this->m_DirectionTolerance) )
        {
        std::ostringstream originString, spacingString, directionString;
        if ( !inputPtr1->GetOrigin().GetVnlVector().is_equal(inputPtrN->GetOrigin().GetVnlVector(), coordinateTol) )
          {
          originString.setf( std::ios::scientific );
          originString.precision( 7 );
          originString << "InputImage Origin: " << inputPtr1->GetOrigin()
                       << ", InputImage" << it.GetName() << " Origin: " << inputPtrN->GetOrigin() << std::endl;
          originString << "\tTolerance: " << coordinateTol << std::endl;
          }
        if ( !inputPtr1->GetSpacing().GetVnlVector().is_equal(inputPtrN->GetSpacing().GetVnlVector(), coordinateTol) )
          {
          spacingString.setf( std::ios::scientific );
          spacingString.precision( 7 );
          spacingString << "InputImage Spacing: " << inputPtr1->GetSpacing()
                        << ", InputImage" << it.GetName() << " Spacing: " << inputPtrN->GetSpacing() << std::endl;
          spacingString << "\tTolerance: " << coordinateTol << std::endl;
          }
        if ( !inputPtr1->GetDirection().GetVnlMatrix().as_ref().is_equal(inputPtrN->GetDirection().GetVnlMatrix(), this->m_DirectionTolerance) )
          {
          directionString.setf( std::ios::scientific );
          directionString.precision( 7 );
          directionString << "InputImage Direction: " << inputPtr1->GetDirection()
                          << ", InputImage" << it.GetName() << " Direction: " << inputPtrN->GetDirection() << std::endl;
          directionString << "\tTolerance: " << this->m_DirectionTolerance << std::endl;
          }
        itkExceptionMacro(<< "Inputs do not occupy the same physical space! "
                          << std::endl
                          << originString.str() << spacingString.str()
                          << directionString.str() );
        }
      }
    }

}


template< typename TInputImage, typename TOutputImage >
void
ImageToImageFilter< TInputImage, TOutputImage >
::PushBackInput(const InputImageType *input)
{
  // Forward to the protected method in the superclass
  this->ProcessObject::PushBackInput(input);
}


template< typename TInputImage, typename TOutputImage >
void
ImageToImageFilter< TInputImage, TOutputImage >
::PopBackInput()
{
  // Forward to the protected method in the superclass
  this->ProcessObject::PopBackInput();
}


template< typename TInputImage, typename TOutputImage >
void
ImageToImageFilter< TInputImage, TOutputImage >
::PushFrontInput(const InputImageType *input)
{
  // Forward to the protected method in the superclass
  this->ProcessObject::PushFrontInput(input);
}


template< typename TInputImage, typename TOutputImage >
void
ImageToImageFilter< TInputImage, TOutputImage >
::PopFrontInput()
{
  // Forward to the protected method in the superclass
  this->ProcessObject::PopFrontInput();
}

} // end namespace itk

#endif
