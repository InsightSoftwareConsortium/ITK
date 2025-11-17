/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmExplicitDataElement.h"
#include "gdcmSequenceOfItems.h"
#include "gdcmSequenceOfFragments.h"

namespace gdcm_ns
{

//-----------------------------------------------------------------------------

VL ExplicitDataElement::GetLength() const
{
  if( ValueLengthField.IsUndefined() )
    {
    gdcm_assert( ValueField->GetLength().IsUndefined() );
    Value *p = ValueField;
    // If this is a SQ we need to compute it's proper length
    SequenceOfItems *sq = dynamic_cast<SequenceOfItems*>(p);
    // TODO can factor the code:
    if( sq )
      {
      const VL sqlen = sq->ComputeLength<ExplicitDataElement>();
      gdcm_assert( sqlen % 2 == 0 );
      return TagField.GetLength() + VRField.GetLength() +
        ValueLengthField.GetLength() + sqlen;
      }
    SequenceOfFragments *sf = dynamic_cast<SequenceOfFragments*>(p);
    if( sf )
      {
      gdcm_assert( VRField & VR::OB_OW ); // VR::INVALID is not possible AFAIK...
      const VL sflen = sf->ComputeLength();
      gdcm_assert( sflen % 2 == 0 );
      return TagField.GetLength() + VRField.GetLength()
        + ValueLengthField.GetLength() + sflen;
      }
    gdcm_assert(0);
    return 0;
    }
  else
    {
    // Each time VR::GetLength() is 2 then Value Length is coded in 2
    //                              4 then Value Length is coded in 4
    gdcm_assert( !ValueField || ValueField->GetLength() == ValueLengthField );
    const bool vr16bitsimpossible = (VRField & VR::VL16) && (ValueLengthField > (uint32_t)VL::GetVL16Max());

    if( vr16bitsimpossible || VRField == VR::INVALID )
      return TagField.GetLength() + 2*VR::GetLength(VR::UN) + ValueLengthField;
    return TagField.GetLength() + 2*VRField.GetLength() + ValueLengthField;
    }
}


} // end namespace gdcm_ns
