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
#ifndef itkChangeLabelImageFilter_hxx
#define itkChangeLabelImageFilter_hxx

#include "itkChangeLabelImageFilter.h"

namespace itk
{
/**
 *
 */
template< typename TInputImage, typename TOutputImage >
ChangeLabelImageFilter< TInputImage, TOutputImage >
::ChangeLabelImageFilter()
{}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
ChangeLabelImageFilter< TInputImage, TOutputImage >
::SetChange(const InputPixelType & original, const OutputPixelType & result)
{
  OutputPixelType current = this->GetFunctor().GetChange(original);

  if ( current != result )
    {
    this->GetFunctor().SetChange(original, result);
    this->Modified();
    }
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
ChangeLabelImageFilter< TInputImage, TOutputImage >
::SetChangeMap(const ChangeMapType & changeMap)
{
  //If the whole map is being set then we assume that a real change is made
  this->GetFunctor().SetChangeMap(changeMap);
  this->Modified();
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
ChangeLabelImageFilter< TInputImage, TOutputImage >
::ClearChangeMap()
{
  //If the whole map is being set then we assume that a real change is made
  this->GetFunctor().ClearChangeMap();
  this->Modified();
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
ChangeLabelImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  //Maybe should iterate the change map and print it here
}
} // end namespace itk

#endif
