/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmEquipmentManufacturer.h"

#include "gdcmAttribute.h"
#include "gdcmSystem.h"

namespace gdcm
{

struct Mapping
{
  EquipmentManufacturer::Type type;
  size_t nstrings;
  const char* const *strings;
};

static const char* const fuji[] = {"FUJI", "FUJI PHOTO FILM Co., ltd.","FUJIFILM Corporation","FUJI PHOTO FILM CO. LTD."};
static const char* const gems[] = {"GE MEDICAL SYSTEMS", "GE_MEDICAL_SYSTEMS", "GE Healthcare", "G.E. Medical Systems","GE Vingmed Ultrasound","\"GE Healthcare\""/*sigh*/};
static const char* const hitachi[] = {"Hitachi Medical Corporation","ALOKA CO., LTD."};
static const char* const kodak[] = {"Kodak"};
static const char* const pms[] = { "Philips Medical Systems", "Philips Healthcare", "Philips Medical Systems, Inc.","Philips","Picker International, Inc." };
static const char* const siemens[] = { "SIEMENS", "Siemens HealthCare GmbH", "Siemens Health Services","Acuson" };
static const char* const marconi[] = { "Marconi Medical Systems, Inc." };
static const char* const toshiba[] = { "TOSHIBA_MEC", "Toshiba" };

#define ARRAY_SIZE( X ) \
  sizeof(X) / sizeof(*X)

#define MAPPING(X, Y) \
  { X, ARRAY_SIZE(Y), Y }

static const Mapping mappings[] = {
  MAPPING( EquipmentManufacturer::FUJI, fuji ),
  MAPPING( EquipmentManufacturer::GEMS, gems ),
  MAPPING( EquipmentManufacturer::HITACHI, hitachi ),
  MAPPING( EquipmentManufacturer::KODAK, kodak ),
  MAPPING( EquipmentManufacturer::PMS, pms ),
  MAPPING( EquipmentManufacturer::SIEMENS, siemens ),
  MAPPING( EquipmentManufacturer::MARCONI, marconi ),
  MAPPING( EquipmentManufacturer::TOSHIBA, toshiba )
};

EquipmentManufacturer::Type EquipmentManufacturer::Compute( DataSet const & ds )
{
  // proper anonymizer should not touch Manufacturer attribute value:
  // http://dicom.nema.org/medical/dicom/current/output/chtml/part15/chapter_E.html#table_E.1-1
  gdcm::Attribute<0x0008,0x0070> manu = { "" }; // Manufacturer
  manu.SetFromDataSet( ds );
  const std::string manufacturer = manu.GetValue().Trim();
  for( const Mapping * mapping = mappings; mapping != mappings + ARRAY_SIZE(mappings); ++mapping )
  {
    for( size_t i = 0; i < mapping->nstrings; ++i )
    {
      // case insensitive to handle: "GE MEDICAL SYSTEMS" vs "GE Medical Systems"
      if( System::StrCaseCmp( mapping->strings[i], manufacturer.c_str() ) == 0 )
        return mapping->type;
    }
  }

  // try against with well known private tag:
  gdcm::Tag gems_iden_01(0x0009,0x0010);
  if( ds.FindDataElement( gems_iden_01 ) )
  {
    const gdcm::DataElement & de = ds.GetDataElement( gems_iden_01 );
    gdcm::Element<VR::LO, VM::VM1> priv_creator;
    priv_creator.SetFromDataElement( de );
    if( priv_creator.GetValue() == "GEMS_IDEN_01" ) return GEMS;
  }

  gdcm::PrivateTag siemens_manu(0x0021,0x0022,"SIEMENS MR SDS 01");
  if( ds.FindDataElement( siemens_manu ) )
  {
    const gdcm::DataElement & de = ds.GetDataElement( siemens_manu );
    gdcm::Element<VR::SH, VM::VM1> value;
    value.SetFromDataElement( de );
    if( value.GetValue().Trim() == "SIEMENS" ) return SIEMENS;
  }

  gdcm::Tag elscint1(0x00e1,0x0010);
  if( ds.FindDataElement( elscint1 ) )
  {
    const gdcm::DataElement & de = ds.GetDataElement( elscint1 );
    gdcm::Element<VR::LO, VM::VM1> priv_creator;
    priv_creator.SetFromDataElement( de );
    if( priv_creator.GetValue() == "ELSCINT1" ) return GEMS;
  }

  return UNKNOWN;
}

} // end namespace gdcm
