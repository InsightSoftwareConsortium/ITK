/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMarchingExtensionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFastMarchingExtensionImageFilter_txx
#define _itkFastMarchingExtensionImageFilter_txx

#include "itkFastMarchingExtensionImageFilter.h"

namespace itk
{

/*
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension,
          class TSpeedImage>
FastMarchingExtensionImageFilter<TLevelSet,TAuxValue,VAuxDimension,TSpeedImage>
::FastMarchingExtensionImageFilter()
{

  m_AuxAliveValues = NULL;
  m_AuxTrialValues = NULL;

  this->ProcessObject::SetNumberOfRequiredOutputs( 1 + AuxDimension );

  AuxImagePointer ptr;
  for ( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    ptr = AuxImageType::New();
    this->ProcessObject::SetNthOutput( k+1, ptr.GetPointer() );
    }
}


/*
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension,
          class TSpeedImage>
void
FastMarchingExtensionImageFilter<TLevelSet,TAuxValue,VAuxDimension,TSpeedImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Aux alive values: ";
  os << m_AuxAliveValues.GetPointer() << std::endl;
  os << indent << "Aux trail values: ";
  os << m_AuxTrialValues.GetPointer() << std::endl;
}


/*
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension,
          class TSpeedImage>
typename FastMarchingExtensionImageFilter<TLevelSet,TAuxValue,VAuxDimension,TSpeedImage>
::AuxImageType *
FastMarchingExtensionImageFilter<TLevelSet,TAuxValue,VAuxDimension,TSpeedImage>
::GetAuxiliaryImage( unsigned int idx )
{

  if ( idx >= AuxDimension || this->GetNumberOfOutputs() < idx + 2 )
    {
    return NULL;
    }

  return static_cast<AuxImageType *>( this->ProcessObject::GetOutput(idx + 1) );

}


/*
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension,
          class TSpeedImage>
void
FastMarchingExtensionImageFilter<TLevelSet,TAuxValue,VAuxDimension,TSpeedImage>
::GenerateOutputInformation()
{

  // call the superclass implementation of this function
  this->Superclass::GenerateOutputInformation();

  // set the size of all the auxiliary outputs
  // to be the same as the primary output
  typename Superclass::LevelSetPointer primaryOutput = this->GetOutput();
  for ( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    AuxImageType * ptr = this->GetAuxiliaryImage(k);
    ptr->SetLargestPossibleRegion( 
      primaryOutput->GetLargestPossibleRegion() );
    }

}


/*
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension,
          class TSpeedImage>
void
FastMarchingExtensionImageFilter<TLevelSet,TAuxValue,VAuxDimension,TSpeedImage>
::EnlargeOutputRequestedRegion(
  DataObject * itkNotUsed(output) )
{

  // This filter requires all of the output images in the buffer.
  for ( unsigned int j = 0; j < this->GetNumberOfOutputs(); j++ )
    {
    if ( this->GetOutput(j) )
      {
      this->GetOutput(j)->SetRequestedRegionToLargestPossibleRegion();
      }
    }
}


/*
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension,
          class TSpeedImage>
void
FastMarchingExtensionImageFilter<TLevelSet,TAuxValue,VAuxDimension,TSpeedImage>
::Initialize( LevelSetImageType * output )
{

  this->Superclass::Initialize( output );

  const typename LevelSetImageType::SizeType size = this->GetOutputSize();

  AuxImagePointer auxImages[AuxDimension];
  
  // allocate memory for the auxiliary outputs
  for ( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    AuxImageType * ptr = this->GetAuxiliaryImage( k );
    ptr->SetBufferedRegion( ptr->GetRequestedRegion() );
    ptr->Allocate();
    auxImages[k] = ptr;
    }

  if ( this->GetAlivePoints() && !m_AuxAliveValues )
    {
    itkExceptionMacro(<<"in Initialize(): Null pointer for AuxAliveValues" );
    }  

  if ( m_AuxAliveValues &&
       m_AuxAliveValues->Size() != (this->GetAlivePoints())->Size() )
    {
    itkExceptionMacro(<<"in Initialize(): AuxAliveValues is the wrong size" );
    }

  if ( this->GetTrialPoints() && !m_AuxTrialValues )
    {
    itkExceptionMacro(<<"in Initialize(): Null pointer for AuxTrialValues" );
    } 

  if ( m_AuxTrialValues &&
       m_AuxTrialValues->Size() != (this->GetTrialPoints())->Size() )
    {
    itkExceptionMacro(<<"in Initialize(): AuxTrialValues is the wrong size" );
    } 
  
  // set all alive points to alive
  typename Superclass::NodeType node;

  AuxValueVectorType auxVec;

  if ( m_AuxAliveValues )
    { 
    typename AuxValueContainer::ConstIterator auxIter = m_AuxAliveValues->Begin();
    typename NodeContainer::ConstIterator pointsIter = (this->GetAlivePoints())->Begin();
    typename NodeContainer::ConstIterator pointsEnd = (this->GetAlivePoints())->End();

    for ( ; pointsIter != pointsEnd; ++pointsIter, ++auxIter )
      {
      node = pointsIter.Value();
      auxVec = auxIter.Value();

      // check if node index is within the output level set
      bool inRange = true;
      for ( unsigned int j = 0; j < SetDimension; j++ )
        {
        if ( node.GetIndex()[j] > (signed long) size[j] )
          {
          inRange = false;
          break;
          }
        }
      if ( !inRange ) continue;
    
      for ( unsigned int k = 0; k < VAuxDimension; k++ )
        {
        auxImages[k]->SetPixel( node.GetIndex(), auxVec[k] );
        }
    
      } // end container loop
    } // if AuxAliveValues set

  if ( m_AuxTrialValues )
    { 
    typename AuxValueContainer::ConstIterator auxIter = m_AuxTrialValues->Begin();
    typename NodeContainer::ConstIterator pointsIter = (this->GetTrialPoints())->Begin();
    typename NodeContainer::ConstIterator pointsEnd = (this->GetTrialPoints())->End();

    for ( ; pointsIter != pointsEnd; ++pointsIter, ++auxIter )
      {
      node = pointsIter.Value();
      auxVec = auxIter.Value();

      // check if node index is within the output level set
      bool inRange = true;
      for ( unsigned int j = 0; j < SetDimension; j++ )
        {
        if ( node.GetIndex()[j] > (signed long) size[j] )
          {
          inRange = false;
          break;
          }
        }
      if ( !inRange ) continue;

      for ( unsigned int k = 0; k < VAuxDimension; k++ )
        {
        auxImages[k]->SetPixel( node.GetIndex(), auxVec[k] );
        }

      } // end container loop

    } // if AuxTrialValues set
      
}


/*
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension, 
          class TSpeedImage>
double
FastMarchingExtensionImageFilter<TLevelSet,TAuxValue,VAuxDimension,TSpeedImage>
::UpdateValue(
  const IndexType& index,
  const SpeedImageType * speed,
  LevelSetImageType * output )
{

  // A extension value at node is choosen such that 
  // grad(F) dot_product grad(Phi) = 0
  // where F is the extended speed function and Phi is 
  // the level set function.
  //
  // The extension value can approximated as a weighted
  // sum of the values from nodes used in the calculation
  // of the distance by the superclass.
  //
  // For more detail see Chapter 11 of
  // "Level Set Methods and Fast Marching Methods", J.A. Sethian,
  // Cambridge Press, Second edition, 1999.

  double solution = this->Superclass::UpdateValue( index, speed, output );

  typename Superclass::NodeType node;

  if ( solution < this->GetLargeValue() )
    {
    // update auxiliary values
    for ( unsigned int k = 0; k < VAuxDimension; k++ )
      {
      double numer = 0.0;
      double denom = 0. ;
      AuxValueType auxVal;

      for( unsigned int j = 0; j < SetDimension; j++ )
        {
        node = this->GetNodeUsedInCalculation(j);

        if( solution < node.GetValue() )
          {
          break;
          }

        auxVal = this->GetAuxiliaryImage(k)->GetPixel( node.GetIndex() );
        numer +=  auxVal  *  ( solution - node.GetValue() );
        denom += solution - node.GetValue();

        }

      if( denom > 0 )
        {
        auxVal = static_cast<AuxValueType>( numer / denom );
        }
      else 
        {
        auxVal = NumericTraits<AuxValueType>::Zero;
        }
        
      this->GetAuxiliaryImage(k)->SetPixel( index, auxVal );
      }
    }

  return solution;

}

} // namespace itk


#endif
