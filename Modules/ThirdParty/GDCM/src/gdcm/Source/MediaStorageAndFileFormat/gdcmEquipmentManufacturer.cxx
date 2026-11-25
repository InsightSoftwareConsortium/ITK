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

namespace gdcm {

// FIXME: fuji and hitachi are the same now
static const char* TypeStrings[] = {"UNKNOWN", "AGFA",    "FUJI",    "GEMS",
                                    "HITACHI", "KODAK",   "MARCONI", "PMS",
                                    "SAMSUNG", "SIEMENS", "TOSHIBA", "UIH"};

const char* EquipmentManufacturer::TypeToString(Type type) {
  return TypeStrings[type];
}

struct Mapping {
  EquipmentManufacturer::Type type;
  size_t nstrings;
  const char* const* strings;
};

static const char* const agfa[] = {"Agfa"};
static const char* const fuji[] = {"FUJI",
                                   "FUJI PHOTO FILM Co., ltd.",
                                   "FUJIFILM Healthcare Corporation",
                                   "FUJIFILM SonoSite",
                                   "FUJIFILM Corporation",
                                   "FUJI PHOTO FILM CO. LTD."};
static const char* const gems[] = {"GE MEDICAL SYSTEMS",
                                   "GE_MEDICAL_SYSTEMS",
                                   "GE Healthcare",
                                   "G.E. Medical Systems",
                                   "GE Healthcare IT Cardiology",
                                   "GE Healthcare Austria GmbH & Co OG",
                                   "GE Healthcare Ultrasound",
                                   "GE MEDICAL SYSTEMS, NUCLEAR",
                                   "GEMS Ultrasound",
                                   "GE OEC Medical Systems GmbH",
                                   "GE Vingmed Ultrasound",
                                   "\"GE Healthcare\"" /*sigh*/};
static const char* const hitachi[] = {"Hitachi Medical Corporation",
                                      "Hitachi, Ltd.", "ALOKA CO., LTD."};
static const char* const kodak[] = {"Kodak"};
static const char* const pms[] = {
    "Philips Medical Systems", "Philips Healthcare",
    "Philips Medical Systems, Inc.", "Philips", "Picker International, Inc."};
static const char* const siemens[] = {"Siemens Healthineers",
                                      "SIEMENS",
                                      "SIEMENS NM",
                                      "Siemens HealthCare GmbH",
                                      "Siemens Health Services",
                                      "Acuson"};
static const char* const marconi[] = {"Marconi Medical Systems, Inc."};
static const char* const samsung[] = {"SAMSUNG MEDISON CO., LTD.",
                                      "SAMSUNG MEDISON CO.,LTD." /*sigh*/};
static const char* const toshiba[] = {"TOSHIBA_MEC", "CANON_MEC",
                                      "TOSHIBA_MEC_US",
                                      "Toshiba"};  // must include canon
static const char* const uih[] = {"UIH"};          // United Imaging Healthcare

#define ARRAY_SIZE(X) (sizeof(X) / sizeof(*X))

#define MAPPING(X, Y) \
  { X, ARRAY_SIZE(Y), Y }

static const Mapping mappings[] = {
    MAPPING(EquipmentManufacturer::AGFA, agfa),
    MAPPING(EquipmentManufacturer::FUJI, fuji),
    MAPPING(EquipmentManufacturer::GEMS, gems),
    MAPPING(EquipmentManufacturer::HITACHI, hitachi),
    MAPPING(EquipmentManufacturer::KODAK, kodak),
    MAPPING(EquipmentManufacturer::PMS, pms),
    MAPPING(EquipmentManufacturer::SIEMENS, siemens),
    MAPPING(EquipmentManufacturer::MARCONI, marconi),
    MAPPING(EquipmentManufacturer::SAMSUNG, samsung),
    MAPPING(EquipmentManufacturer::TOSHIBA, toshiba),
    MAPPING(EquipmentManufacturer::UIH, uih)};

// long story short, private creator could be moved around, what we are trying
// to achieve here is true modality check, so generally they should not have
// been moved in the process.
static bool IsPrivateCreatorFound(DataSet const& ds, Tag const& private_tag,
                                  std::string const& creator_value) {
  if (ds.FindDataElement(private_tag)) {
    const DataElement& de = ds.GetDataElement(private_tag);
    Element<VR::LO, VM::VM1> priv_creator;
    priv_creator.SetFromDataElement(de);
    if (priv_creator.GetValue().Trim() == creator_value) return true;
  }
  return false;
}

template <long long TVR, int TVM>
static std::string GetPrivateTagValueOrEmpty(DataSet const& ds,
                                             PrivateTag const& pt) {
  if (ds.FindDataElement(pt)) {
    const DataElement& de = ds.GetDataElement(pt);
    Element<TVR, TVM> value = {""};
    value.SetFromDataElement(de);
    return value.GetValue().Trim();
  }
  return "";
}

EquipmentManufacturer::Type EquipmentManufacturer::GuessFromPrivateAttributes(
    DataSet const& ds) {
  // try against with well known private tag:
  // watch out for private creator such as ELSCINT1 which can be found in
  // GEMS/PEMS and maybe even SIEMENS !
  // Try to prefer those listed at:
  // https://dicom.nema.org/medical/dicom/current/output/chtml/part15/sect_E.3.10.html#table_E.3.10-1
  if (ds.FindDataElement(PrivateTag(0x0019, 0x0023, "GEMS_ACQU_01")) ||
      ds.FindDataElement(PrivateTag(0x0043, 0x0039, "GEMS_PARM_01")) ||
      ds.FindDataElement(PrivateTag(0x0045, 0x0001, "GEMS_HELIOS_01")) ||
      ds.FindDataElement(PrivateTag(0x0025, 0x001b, "GEMS_SERS_01"))
      /* extra */
      || ds.FindDataElement(
             PrivateTag(0x6003, 0x0010, "GEMS_Ultrasound_ImageGroup_001")) ||
      ds.FindDataElement(PrivateTag(0x0019, 0x0007, "DLX_SERIE_01")) ||
      ds.FindDataElement(PrivateTag(0x0009, 0x0001, "GEMS_GENIE_1")) ||
      ds.FindDataElement(PrivateTag(0x0011, 0x0003, "GEMS_GDXE_FALCON_04")))
    return GEMS;

#if 0
  if (IsPrivateCreatorFound(ds, Tag(0x0025, 0x0010), "GEMS_IDEN_01") ||
      IsPrivateCreatorFound(ds, Tag(0x0009, 0x0010), "GEMS_IDEN_01") ||
      IsPrivateCreatorFound(ds, Tag(0x0009, 0x0010), "GEMS_GENIE_1") ||
      IsPrivateCreatorFound(ds, Tag(0x0009, 0x0010), "GEMS_PETD_01"))
    return GEMS;
#endif

  // Philips:
  if (ds.FindDataElement(
          PrivateTag(0x2005, 0x000d, "Philips MR Imaging DD 001")) ||
      ds.FindDataElement(
          PrivateTag(0x2005, 0x000e, "Philips MR Imaging DD 001")) ||
      ds.FindDataElement(
          PrivateTag(0x2001, 0x0003, "Philips Imaging DD 001")) ||
      ds.FindDataElement(PrivateTag(0x2001, 0x005f, "Philips Imaging DD 001")))
    return PMS;
#if 0
  if (IsPrivateCreatorFound(ds, Tag(0x2005, 0x0014),
                            "Philips MR Imaging DD 005"))
    return PMS;
#endif
  if (IsPrivateCreatorFound(ds, Tag(0x0019, 0x0010), "PHILIPS MR/PART") &&
      IsPrivateCreatorFound(ds, Tag(0x0021, 0x0010), "PHILIPS MR/PART"))
    return PMS;
  if (IsPrivateCreatorFound(ds, Tag(0x0009, 0x0010), "SPI-P Release 1") &&
      IsPrivateCreatorFound(ds, Tag(0x0019, 0x0010), "SPI-P Release 1"))
    return PMS;

  // Siemens:
  if (ds.FindDataElement(PrivateTag(0x0029, 0x0010, "SIEMENS CSA HEADER")) ||
      ds.FindDataElement(PrivateTag(0x0029, 0x0020, "SIEMENS CSA HEADER")) ||
      ds.FindDataElement(PrivateTag(0x0029, 0x0010, "SIEMENS MEDCOM OOG")) ||
      ds.FindDataElement(
          PrivateTag(0x7fdf, 0x0000, "ACUSON:1.2.840.113680.1.0:7ffe")) ||
      ds.FindDataElement(PrivateTag(0x0019, 0x0012, "SIEMENS CM VA0  ACQU")) ||
      ds.FindDataElement(PrivateTag(0x0009, 0x0010, "SIEMENS CT VA0  IDE")))
    return SIEMENS;
#if 0
  if (GetPrivateTagValueOrEmpty<VR::SH, VM::VM1>(
          ds, PrivateTag(0x0021, 0x0022, "SIEMENS MR SDS 01")) == "SIEMENS")
    return SIEMENS;
  // gdcm-MR-SIEMENS-16-2.acr
  if (GetPrivateTagValueOrEmpty<VR::LO, VM::VM1>(
          ds, PrivateTag(0x0019, 0x0012, "SIEMENS CM VA0  ACQU")) == "SIEMENS")
    return SIEMENS;
#endif

  // toshiba:
  if (ds.FindDataElement(PrivateTag(0x7005, 0x0008, "TOSHIBA_MEC_CT3")) ||
      ds.FindDataElement(PrivateTag(0x700d, 0x0008, "TOSHIBA_MEC_MR3")) ||
      ds.FindDataElement(PrivateTag(0x0029, 0x0001, "PMTF INFORMATION DATA")) ||
      ds.FindDataElement(PrivateTag(0x0029, 0x0001, "CANON_MEC_MR3")) ||
      ds.FindDataElement(PrivateTag(0x0029, 0x0001, "TOSHIBA_MEC_MR3")))
    return TOSHIBA;
  // fuji
  if (ds.FindDataElement(PrivateTag(0x0021, 0x0010, "FDMS 1.0"))) return FUJI;
  // hitachi
  if (ds.FindDataElement(PrivateTag(0x0009, 0x0000, "HMC - CT - ID")) ||
      ds.FindDataElement(PrivateTag(0x0009, 0x0003, "MMCPrivate")) ||
      ds.FindDataElement(PrivateTag(0x0009, 0x0050, "MMCPrivate")) ||
      ds.FindDataElement(PrivateTag(0x0019, 0x000e, "MMCPrivate")) ||
      ds.FindDataElement(PrivateTag(0x0019, 0x0021, "MMCPrivate")) ||
      ds.FindDataElement(PrivateTag(0x0029, 0x002f, "MMCPrivate")) ||
      ds.FindDataElement(PrivateTag(0x0029, 0x00d7, "MMCPrivate")))
    return HITACHI;
  // UIH
  if (ds.FindDataElement(PrivateTag(0x0065, 0x000a, "Image Private Header")))
    return UIH;

  return UNKNOWN;
}

EquipmentManufacturer::Type EquipmentManufacturer::Compute(DataSet const& ds) {
  EquipmentManufacturer::Type ret = GuessFromPrivateAttributes(ds);

  // proper anonymizer should not touch Manufacturer attribute value:
  // http://dicom.nema.org/medical/dicom/current/output/chtml/part15/chapter_E.html#table_E.1-1
  Attribute<0x0008, 0x0070> manu = {""};  // Manufacturer
  std::string manufacturer;
  if (ds.FindDataElement(manu.GetTag())) {
    manu.SetFromDataSet(ds);
    manufacturer = manu.GetValue().Trim();
    // TODO: contributing equipement ?
  } 
  if( manufacturer.empty() )
  {
    // MFSPLIT export seems to remove the attribute completely:
    // or in some case make it empty
    manufacturer = GetPrivateTagValueOrEmpty<VR::SH, VM::VM1>(
        ds, PrivateTag(0x0021, 0x0022, "SIEMENS MR SDS 01"));
  }
  if (!manufacturer.empty()) {
    for (const Mapping* mapping = mappings;
         mapping != mappings + ARRAY_SIZE(mappings); ++mapping) {
      for (size_t i = 0; i < mapping->nstrings; ++i) {
        // case insensitive to handle: "GE MEDICAL SYSTEMS" vs "GE Medical
        // Systems"
        if (System::StrCaseCmp(mapping->strings[i], manufacturer.c_str()) ==
            0) {
          if (ret != UNKNOWN && ret != mapping->type) {
            gdcmErrorMacro(" Impossible happen: " << ret << " vs "
                                                  << mapping->type);
            return UNKNOWN;
          }
          return mapping->type;
        }
      }
    }
  }

  gdcmWarningMacro("Unknown Manufacturer [" << manufacturer
                                            << "] trying guess.");
  return ret;
}

}  // end namespace gdcm
