/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMarchExtension.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/

namespace itk
{

/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension>
FastMarchExtension<TLevelSet,TAuxValue,VAuxDimension>
::FastMarchExtension()
{

  m_AuxAliveValues = NULL;
  m_AuxTrialValues = NULL;

  AuxImagePointer ptr;
  for( int k = 0; k < VAuxDimension; k++ )
    {
    ptr = AuxImageType::New();
    m_AuxImage[k] = ptr;
    this->ProcessObject::AddOutput( ptr.GetPointer() );
    }

  m_DebugOn = false;

}


/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension>
void
FastMarchExtension<TLevelSet,TAuxValue,VAuxDimension>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Fast Marching Extension" << std::endl;
}


/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension>
void
FastMarchExtension<TLevelSet,TAuxValue,VAuxDimension>
::GenerateOutputInformation()
{

  // call the superclass implementation of this function
  this->Superclass::GenerateOutputInformation();

  // set the size of all the auxiliary outputs
  // to be the same as the primary output
  LevelSetPointer primaryOutput = this->GetOutput(0);
  for( int k = 0; k < VAuxDimension; k++ )
    {
    m_AuxImage[k]->SetLargestPossibleRegion( 
      primaryOutput->GetLargestPossibleRegion() );
    }

}


/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension>
void
FastMarchExtension<TLevelSet,TAuxValue,VAuxDimension>
::EnlargeOutputRequestedRegion(
DataObject *output )
{

  // call the superclass implementation of this function
  this->Superclass::EnlargeOutputRequestedRegion(output);

  // set the requested region for all auxiliary outputs
  // to be the same as the primary output
  LevelSetPointer primaryOutput = this->GetOutput(0);
  for( int k = 0; k < VAuxDimension; k++ )
    {
    m_AuxImage[k]->SetRequestedRegion(
      primaryOutput->GetRequestedRegion() );
    }
}


/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension>
void
FastMarchExtension<TLevelSet,TAuxValue,VAuxDimension>
::Initialize()
{

  this->Superclass::Initialize();

  LevelSetPointer output = this->GetOutput();
  const typename LevelSetImageType::SizeType size = this->GetOutputSize();

  // allocate memory for the auxiliary outputs
  for( int k = 0; k < VAuxDimension; k++ )
    {
    m_AuxImage[k]->SetBufferedRegion( 
      m_AuxImage[k]->GetRequestedRegion() );
    m_AuxImage[k]->Allocate();
    }

  if( this->GetAlivePoints() && !m_AuxAliveValues )
    {
    throw ExceptionObject();
    }  

  if( m_AuxAliveValues &&
      m_AuxAliveValues->Size() != (this->GetAlivePoints())->Size() )
    {
    throw ExceptionObject();
    }

  if( this->GetTrialPoints() && !m_AuxTrialValues )
    {
    throw ExceptionObject();
    } 

  if( m_AuxTrialValues &&
      m_AuxTrialValues->Size() != (this->GetTrialPoints())->Size() )
    {
    throw ExceptionObject();
    } 
  
  // set all alive points to alive
  typename NodeContainer::ConstIterator pointsIter;
  typename NodeContainer::ConstIterator pointsEnd;
  NodeType node;

  typename AuxValueContainer::ConstIterator auxIter;

  AuxValueVectorType auxVec;

  if( m_AuxAliveValues )
    { 
    auxIter = m_AuxAliveValues->Begin();

    pointsIter = (this->GetAlivePoints())->Begin();
    pointsEnd = (this->GetAlivePoints())->End();

    for( ; pointsIter != pointsEnd; ++pointsIter, ++auxIter )
      {
      node = pointsIter.Value();
      auxVec = auxIter.Value();

      // check if node index is within the output level set
      bool inRange = true;
      for( int j = 0; j < SetDimension; j++ )
        {
        if( node.index[j] > size[j] )
          {
          inRange = false;
          break;
          }
        }
      if( !inRange ) continue;
    
      for( int k = 0; k < VAuxDimension; k++ )
      {
        m_AuxImage[k]->SetPixel( node.index, auxVec[k] );
      }
    
      } // end container loop
    } // if AuxAliveValues set

  if( m_AuxTrialValues )
    { 
    auxIter = m_AuxTrialValues->Begin();
    pointsIter = (this->GetTrialPoints())->Begin();
    pointsEnd = (this->GetTrialPoints())->End();

    for( ; pointsIter != pointsEnd; ++pointsIter, ++auxIter )
      {
      node = pointsIter.Value();
      auxVec = auxIter.Value();

      // check if node index is within the output level set
      bool inRange = true;
      for( int j = 0; j < SetDimension; j++ )
        {
        if( node.index[j] > size[j] )
          {
          inRange = false;
          break;
          }
        }
      if( !inRange ) continue;

      for( int k = 0; k < VAuxDimension; k++ )
      {
        m_AuxImage[k]->SetPixel( node.index, auxVec[k] );
      }

      } // end container loop

    } // if AuxTrialValues set
      
}


/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension>
double
FastMarchExtension<TLevelSet,TAuxValue,VAuxDimension>
::UpdateValue(
IndexType& index )
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

  double solution = this->Superclass::UpdateValue( index );

  NodeType node;

  if( solution < this->GetLargeValue() )
  {
    // update auxiliary values
    for( int k = 0; k < VAuxDimension; k++ )
    {
      double numer = 0.0;
      double denom = 0. ;
      AuxValueType auxVal;

      for( int j = 0; j < SetDimension; j++ )
      {
        node = this->GetNodeUsedInCalculation(j);

        if( solution < node.value )
        {
          break;
        }

        auxVal = m_AuxImage[k]->GetPixel( node.index );
        numer +=  ScalarTraits<AuxValueType>::GetScalar( auxVal ) * 
          ( solution - node.value );
        denom += solution - node.value;

      }

      if( denom > 0 )
      {
        ScalarTraits<AuxValueType>::SetScalar( auxVal, numer / denom );
      }
      else 
      {
        ScalarTraits<AuxValueType>::SetScalar( auxVal, 0.0 );
      }
        
      m_AuxImage[k]->SetPixel( index, auxVal );

    }

  }

  return solution;

}

} // namespace itk

