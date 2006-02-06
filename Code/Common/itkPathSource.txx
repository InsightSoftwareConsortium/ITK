/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPathSource.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPathSource_txx
#define __itkPathSource_txx

#include "itkPathSource.h"

namespace itk
{

/**
 *
 */
template<class TOutputPath>
PathSource<TOutputPath>
::PathSource()
{
  // Create the output. We use static_cast<> here because we know the default
  // output must be of type TOutputPath
  OutputPathPointer output
    = static_cast<TOutputPath*>(this->MakeOutput(0).GetPointer()); 

  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput( 0, output.GetPointer() );

  // Initialize PathSource member data
}

/**
 *
 */
template<class TOutputPath>
typename PathSource<TOutputPath>::OutputPathType *
PathSource<TOutputPath>
::GetOutput(void)
{
  if (this->GetNumberOfOutputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TOutputPath*>
                     (this->ProcessObject::GetOutput(0));
}

  
/**
 *
 */
template<class TOutputPath>
typename PathSource<TOutputPath>::OutputPathType *
PathSource<TOutputPath>
::GetOutput(unsigned int idx)
{
  return static_cast<TOutputPath*>
                     (this->ProcessObject::GetOutput(idx));
}

/**
 * 
 */
template<class TOutputPath>
void
PathSource<TOutputPath>
::GraftOutput(TOutputPath *graft)
{
  this->GraftNthOutput(0, graft);
}

/**
 * 
 */
template<class TOutputPath>
void
PathSource<TOutputPath>
::GraftNthOutput(unsigned int idx, TOutputPath *graft)
{
  if (idx < this->GetNumberOfOutputs())
    {
    OutputPathType * output = this->GetOutput(idx);

    if (output && graft)
      {
      // Paths do not have a generic pointer to their bulk data
      itkWarningMacro( << "Warning:  GraftNthOutput() is broken" );
      
      // possible VERY WRONG KLUDGE that should enable mini-pipelining:
      // Completely copy the path to graft over the current output path,
      // but RESTORE the original Source ivars to preserve pipeline routing.
      // 
      // ProcessObject *source = output->GetSource();
      // *output = *graft;
      // output->DisconnectSource( graft->GetSource(),
      //                           graft->GetSourceOutputIndex() );
      // output->ConnectSource( source, idx );
      
      
      // grab a handle to the bulk data of the specified data object
      // output->SetPixelContainer( graft->GetPixelContainer() );

      // copy the region ivars of the specified data object
      // output->SetRequestedRegion( graft->GetRequestedRegion() );
      // output->SetLargestPossibleRegion( graft->GetLargestPossibleRegion() );
      // output->SetBufferedRegion( graft->GetBufferedRegion() );

      // copy the meta-information
      //output->CopyInformation( graft );
      }
    }
}

/**
 *
 */
template<class TOutputPath>
typename PathSource<TOutputPath>::DataObjectPointer
PathSource<TOutputPath>
::MakeOutput(unsigned int)
{
  return static_cast<DataObject*>(TOutputPath::New().GetPointer());
}

/**
 *
 */
template<class TOutputPath>
void 
PathSource<TOutputPath>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

#endif
