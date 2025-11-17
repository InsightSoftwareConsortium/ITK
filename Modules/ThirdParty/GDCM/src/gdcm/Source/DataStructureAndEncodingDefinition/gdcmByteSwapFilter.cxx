/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmByteSwapFilter.h"

#include "gdcmElement.h"
#include "gdcmByteValue.h"
#include "gdcmSequenceOfFragments.h"
#include "gdcmSequenceOfItems.h"
#include "gdcmSwapper.h"

namespace gdcm
{

//-----------------------------------------------------------------------------
//ByteSwapFilter::ByteSwapFilter()
//{
//}
//-----------------------------------------------------------------------------

bool ByteSwapFilter::ByteSwap()
{
  for(
    DataSet::ConstIterator it = DS.Begin();
    it != DS.End(); ++it)
    {
    const DataElement &de = *it;
    VR const & vr = de.GetVR();
    //gdcm_assert( vr & VR::VRASCII || vr & VR::VRBINARY );
    ByteValue *bv = const_cast<ByteValue*>(de.GetByteValue());
    gdcm::SmartPointer<gdcm::SequenceOfItems> si = de.GetValueAsSQ();
    if( de.IsEmpty() )
      {
      }
    else if( bv && !si )
      {
      gdcm_assert( !si );
      // ASCII do not need byte swap
      if( vr & VR::VRBINARY /*&& de.GetTag().IsPrivate()*/ )
        {
        //gdcm_assert( de.GetTag().IsPrivate() );
        switch(vr)
          {
        case VR::AT:
          gdcm_assert( 0 && "Should not happen" );
          break;
        case VR::FL:
          // FIXME: Technically FL should not be byte-swapped...
          //std::cerr << "ByteSwap FL:" << de.GetTag() << std::endl;
          SwapperDoOp::SwapArray((uint32_t*)bv->GetVoidPointer(), bv->GetLength() / sizeof(uint32_t) );
          break;
        case VR::FD:
          gdcm_assert( 0 && "Should not happen" );
          break;
        case VR::OB:
          // I think we are fine, unless this is one of those OB_OW thingy
          break;
        case VR::OF:
          gdcm_assert( 0 && "Should not happen" );
          break;
        case VR::OW:
          gdcm_assert( 0 && "Should not happen" );
          break;
        case VR::SL:
          SwapperDoOp::SwapArray((uint32_t*)bv->GetVoidPointer(), bv->GetLength() / sizeof(uint32_t) );
          break;
        case VR::SQ:
          gdcm_assert( 0 && "Should not happen" );
          break;
        case VR::SS:
          SwapperDoOp::SwapArray((uint16_t*)bv->GetVoidPointer(), bv->GetLength() / sizeof(uint16_t) );
          break;
        case VR::UL:
          SwapperDoOp::SwapArray((uint32_t*)bv->GetVoidPointer(), bv->GetLength() / sizeof(uint32_t) );
          break;
        case VR::UN:
          gdcm_assert( 0 && "Should not happen" );
          break;
        case VR::US:
          SwapperDoOp::SwapArray((uint16_t*)bv->GetVoidPointer(), bv->GetLength() / sizeof(uint16_t) );
          break;
        case VR::UT:
          gdcm_assert( 0 && "Should not happen" );
          break;
        default:
          gdcm_assert( 0 && "Should not happen" );
          }
        }
      }
    //else if( const SequenceOfItems *si = de.GetSequenceOfItems() )
    else if( si )
      {
      //if( de.GetTag().IsPrivate() )
        {
        //std::cerr << "ByteSwap SQ:" << de.GetTag() << std::endl;
        SequenceOfItems::ConstIterator it2 = si->Begin();
        for( ; it2 != si->End(); ++it2)
          {
          const Item &item = *it2;
          DataSet &ds = const_cast<DataSet&>(item.GetNestedDataSet()); // FIXME
          ByteSwapFilter bsf(ds);
          bsf.ByteSwap();
          }
        }
      }
    else if( const SequenceOfFragments *sf = de.GetSequenceOfFragments() )
      {
      (void)sf;
      gdcm_assert( 0 && "Should not happen" );
      }
    else
      {
      gdcm_assert( 0 && "error" );
      }

    }
  if( ByteSwapTag )
    {
    DataSet copy;
    DataSet::ConstIterator it = DS.Begin();
    for( ; it != DS.End(); ++it)
      {
      DataElement de = *it;
      const Tag& tag = de.GetTag();
      de.SetTag(
        Tag( SwapperDoOp::Swap( tag.GetGroup() ), SwapperDoOp::Swap( tag.GetElement() ) ) );
      copy.Insert( de );
      }
    DS = copy;
    }

  return true;
}

}
