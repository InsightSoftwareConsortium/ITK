/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBayesianClassifierImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBayesianClassifierImageFilter_txx
#define _itkBayesianClassifierImageFilter_txx

#include "itkBayesianClassifierImageFilter.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{

/**
 *  Constructor
 */
template < class TInputVectorImage, class TLabelsType, 
           class TPosteriorsPrecisionType, class TPriorsPrecisionType >
BayesianClassifierImageFilter<TInputVectorImage, TLabelsType, 
                              TPosteriorsPrecisionType, TPriorsPrecisionType >
::BayesianClassifierImageFilter()
{
   m_UserProvidedPriors = false;
   m_UserProvidedSmoothingFilter = false;
   this->SetNumberOfRequiredOutputs( 2 );
   m_NumberOfSmoothingIterations = 0;
   m_SmoothingFilter = NULL;
   PosteriorsImagePointer p = PosteriorsImageType::New();
   this->SetNthOutput( 1 , p.GetPointer() );
}




/**
 *  Print Self Method
 */
template < class TInputVectorImage, class TLabelsType, 
           class TPosteriorsPrecisionType, class TPriorsPrecisionType >
void 
BayesianClassifierImageFilter<TInputVectorImage, TLabelsType, 
                              TPosteriorsPrecisionType, TPriorsPrecisionType >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "User provided priors =  " << m_UserProvidedPriors << std::endl;
  os << indent << "User provided smooting filter =  " << m_UserProvidedSmoothingFilter << std::endl;
  os << indent << "Smooting filter pointer =  " << m_SmoothingFilter.GetPointer() << std::endl;
  os << indent << "Number of smoothing iterations =  " << m_NumberOfSmoothingIterations << std::endl;

}

/**
 * Generate Data method is where the classification (and smoothing) is performed.
 */
template < class TInputVectorImage, class TLabelsType, 
           class TPosteriorsPrecisionType, class TPriorsPrecisionType >
void 
BayesianClassifierImageFilter<TInputVectorImage, TLabelsType, 
                              TPosteriorsPrecisionType, TPriorsPrecisionType >
::GenerateData()
{
  // Setup input image
  const InputImageType * membershipImage = this->GetInput();

  // Setup general parameters
  const unsigned int numberOfClasses = membershipImage->GetVectorLength();

  if( numberOfClasses == 0 )
    {
    itkExceptionMacro("The number of components in the input Membership image is Zero !");
    return;
    }

  this->AllocateOutputs();

  this->ComputeBayesRule();
  
  if( m_UserProvidedSmoothingFilter )
    {
    this->NormalizeAndSmoothPosteriors();
    }

  this->ClassifyBasedOnPosteriors();

}


template < class TInputVectorImage, class TLabelsType, 
           class TPosteriorsPrecisionType, class TPriorsPrecisionType >
typename BayesianClassifierImageFilter<TInputVectorImage, TLabelsType, 
                              TPosteriorsPrecisionType, TPriorsPrecisionType >
::PosteriorsImageType *                              
BayesianClassifierImageFilter<TInputVectorImage, TLabelsType, 
                              TPosteriorsPrecisionType, TPriorsPrecisionType >
::GetPosteriorImage()
{
  return  dynamic_cast< PosteriorsImageType * >(
                  this->ProcessObject::GetOutput(1) );
}


/**
 * Compute the labeled map with no priors and no smoothing
 */
template < class TInputVectorImage, class TLabelsType, 
           class TPosteriorsPrecisionType, class TPriorsPrecisionType >
void 
BayesianClassifierImageFilter<TInputVectorImage, TLabelsType, 
                              TPosteriorsPrecisionType, TPriorsPrecisionType >
::AllocateOutputs()
{
  const InputImageType * membershipImage = this->GetInput();

  this->GetOutput()->SetRegions( membershipImage->GetBufferedRegion() );
  this->GetOutput()->SetSpacing( membershipImage->GetSpacing() );
  this->GetOutput()->SetOrigin(  membershipImage->GetOrigin() );
  this->GetOutput()->Allocate();

  // The first output is the Image of Labels, 
  // The second output is the image of Posteriors.
  // TODO Make this optional.. RequiredNumberOfOutputs should not always
  // be 2.

  this->GetPosteriorImage()->SetRegions( membershipImage->GetBufferedRegion() );
  this->GetPosteriorImage()->SetSpacing( membershipImage->GetSpacing() );
  this->GetPosteriorImage()->SetOrigin(  membershipImage->GetOrigin() );
  this->GetPosteriorImage()->SetVectorLength( this->GetInput()->GetVectorLength() );
  this->GetPosteriorImage()->Allocate();

}




/**
 * Compute the posteriors using the Bayes rule. If no priors are available,
 * then the posteriors are just a copy of the memberships.  */
template < class TInputVectorImage, class TLabelsType, 
           class TPosteriorsPrecisionType, class TPriorsPrecisionType >
void 
BayesianClassifierImageFilter<TInputVectorImage, TLabelsType, 
                              TPosteriorsPrecisionType, TPriorsPrecisionType >
::ComputeBayesRule()
{
  itkDebugMacro( << "Computing Bayes Rule" );
  const InputImageType * membershipImage = this->GetInput();

  ImageRegionType   imageRegion  = membershipImage->GetBufferedRegion();

  if( m_UserProvidedPriors )
    {
    const PriorsImageType * priorsImage = 
             dynamic_cast< const PriorsImageType * >( this->GetInput(1) ); 

    if( priorsImage == NULL )
      {
      itkExceptionMacro("Second input type does not correspond to expected Priors Image Type");
      }

    PosteriorsImageType * posteriorsImage = 
             dynamic_cast< PosteriorsImageType * >( this->GetPosteriorImage() ); 

    if( posteriorsImage == NULL )
      {
      itkExceptionMacro("Second output type does not correspond to expected Posteriors Image Type");
      }


    InputImageIteratorType       itrMembershipImage(  membershipImage, imageRegion );
    PriorsImageIteratorType      itrPriorsImage(      priorsImage,     imageRegion );
    PosteriorsImageIteratorType  itrPosteriorsImage(  posteriorsImage, imageRegion );

    itrMembershipImage.GoToBegin();
    itrPriorsImage.GoToBegin();

    const unsigned int numberOfClasses = membershipImage->GetVectorLength();
    
    itkDebugMacro( << "Computing Bayes Rule nclasses in membershipImage: " << numberOfClasses );
    
    while( !itrMembershipImage.IsAtEnd() )
      {
      PosteriorsPixelType posteriors;
      const PriorsPixelType      priors      = itrPriorsImage.Get();
      const MembershipPixelType  memberships = itrMembershipImage.Get();
      for( unsigned int i=0; i<numberOfClasses; i++)
         {
         posteriors[i] =
            static_cast< TPosteriorsPrecisionType >( memberships[i] * priors[i] ); 
         }   
      itrPosteriorsImage.Set( posteriors );
      ++itrMembershipImage;
      ++itrPriorsImage;
      ++itrPosteriorsImage;
      }
    }
  else
    {
    PosteriorsImageType * posteriorsImage = 
             dynamic_cast< PosteriorsImageType * >( this->GetPosteriorImage() ); 

    if( posteriorsImage == NULL )
      {
      itkExceptionMacro("Second output type does not correspond to expected Posteriors Image Type");
      }

    InputImageIteratorType      itrMembershipImage( membershipImage, imageRegion );
    PosteriorsImageIteratorType itrPosteriorsImage( posteriorsImage, imageRegion );

    itrMembershipImage.GoToBegin();
    itrPosteriorsImage.GoToBegin();
    
    while( !itrMembershipImage.IsAtEnd() )
      {
      itrPosteriorsImage.Set( itrMembershipImage.Get() ); 
      ++itrMembershipImage;
      ++itrPosteriorsImage;
      }

    }
}


template < class TInputVectorImage, class TLabelsType, 
           class TPosteriorsPrecisionType, class TPriorsPrecisionType >
void 
BayesianClassifierImageFilter<TInputVectorImage, TLabelsType, 
                              TPosteriorsPrecisionType, TPriorsPrecisionType >
::SetSmoothingFilter( SmoothingFilterType * smoothingFilter ) 
{
  this->m_SmoothingFilter = smoothingFilter;
  this->m_UserProvidedSmoothingFilter = true;
  this->Modified();
}


/**
 * Normalize the posteriors and smooth them using an user-provided.
 */
template < class TInputVectorImage, class TLabelsType, 
           class TPosteriorsPrecisionType, class TPriorsPrecisionType >
void 
BayesianClassifierImageFilter<TInputVectorImage, TLabelsType, 
                              TPosteriorsPrecisionType, TPriorsPrecisionType >
::NormalizeAndSmoothPosteriors()
{
  
  PosteriorsImageIteratorType itrPosteriorImage( 
      this->GetPosteriorImage(), this->GetPosteriorImage()->GetBufferedRegion() );

  PosteriorsPixelType p;
  const unsigned int numberOfClasses = this->GetPosteriorImage()->GetVectorLength();
  
  for( unsigned int iter=0; iter< m_NumberOfSmoothingIterations; iter++)
    {
    itrPosteriorImage.GoToBegin();
    while( !itrPosteriorImage.IsAtEnd() )
      {
      p = itrPosteriorImage.Get();
      
      // Normalize P so the probablity across components sums to 1
      TPosteriorsPrecisionType probability=0;
      for( unsigned int i=0; i< numberOfClasses; i++ )
        {
        probability += p[i];
        }
      p /= probability;
      itrPosteriorImage.Set( p );
      ++itrPosteriorImage;
      }
    

    for( unsigned int componentToExtract=0; componentToExtract < numberOfClasses; componentToExtract++)
      {
      // Create an auxillary image to store one component of the vector image. 
      // Smoothing filters typically can't handle multi-component images, so we
      // will extract each component and smooth it.
      typedef itk::Image< TPosteriorsPrecisionType, Dimension > ExtractedComponentImageType;
      typename ExtractedComponentImageType::Pointer extractedComponentImage = 
                                      ExtractedComponentImageType::New();
      extractedComponentImage->CopyInformation( this->GetPosteriorImage());
      extractedComponentImage->SetBufferedRegion( 
                     this->GetPosteriorImage()->GetBufferedRegion() );
      extractedComponentImage->SetRequestedRegion( 
                     this->GetPosteriorImage()->GetRequestedRegion() );
      extractedComponentImage->Allocate();
      typedef itk::ImageRegionIterator< ExtractedComponentImageType > IteratorType;

      itrPosteriorImage.GoToBegin();
      IteratorType it( extractedComponentImage, 
                       extractedComponentImage->GetBufferedRegion() );
      
      it.GoToBegin();
      while( !itrPosteriorImage.IsAtEnd() )
        {
        it.Set(itrPosteriorImage.Get()[componentToExtract]);
        ++it;
        ++itrPosteriorImage;
        }

      m_SmoothingFilter->SetInput( extractedComponentImage );
      m_SmoothingFilter->Modified(); // Force an update
      m_SmoothingFilter->Update();

      itrPosteriorImage.GoToBegin();

      IteratorType sit( m_SmoothingFilter->GetOutput(),
                       m_SmoothingFilter->GetOutput()->GetBufferedRegion() );
      sit.GoToBegin();
      while( !itrPosteriorImage.IsAtEnd() )
        {
        PosteriorsPixelType posteriorPixel = itrPosteriorImage.Get();
        posteriorPixel[componentToExtract] = sit.Get();
        itrPosteriorImage.Set(posteriorPixel);
        ++sit;
        ++itrPosteriorImage;
        }
      }
    }
}


/**
 * Compute the labeled map based on the Maximum rule applied to the posteriors.
 */
template < class TInputVectorImage, class TLabelsType, 
           class TPosteriorsPrecisionType, class TPriorsPrecisionType >
void 
BayesianClassifierImageFilter<TInputVectorImage, TLabelsType, 
                              TPosteriorsPrecisionType, TPriorsPrecisionType >
::ClassifyBasedOnPosteriors()
{
  OutputImagePointer labels = this->GetOutput();

  ImageRegionType   imageRegion  = labels->GetBufferedRegion();

  PosteriorsImageType * posteriorsImage = 
           dynamic_cast< PosteriorsImageType * >( this->GetPosteriorImage() ); 

  if( posteriorsImage == NULL )
    {
    itkExceptionMacro("Second output type does not correspond to expected Posteriors Image Type");
    }

  OutputImageIteratorType      itrLabelsImage(     labels,         imageRegion );
  PosteriorsImageIteratorType  itrPosteriorsImage( posteriorsImage,imageRegion );

  DecisionRulePointer decisionRule = DecisionRuleType::New();

  itrLabelsImage.GoToBegin();
  itrPosteriorsImage.GoToBegin();

  while ( !itrLabelsImage.IsAtEnd() )
    {
    itrLabelsImage.Set( static_cast< TLabelsType >(
          decisionRule->Evaluate( itrPosteriorsImage.Get())) );
    ++itrLabelsImage;
    ++itrPosteriorsImage;
    }
}



} // end namespace itk

#endif
