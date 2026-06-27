#-- # Grading Level Criteria Report
#-- EVALUATION DATE: 2026-06-26
#-- EVALUATORS: [hjmjohnson]
#--
#-- ## Compliance Level 1 star (Pre-alpha features under development and code of unknown quality)
#--   - [X] Code complies on at least 1 platform
#--
#-- Level 1 is the lowest the toolkit can express: itk_fetch_module treats a
#-- compliance level of 0 as unset and promotes it to 1. Retired from the ITK
#-- main tree; retained only to detect unknown downstream users. Scheduled for
#-- removal in ITK 7. See ITK#6511.

itk_fetch_module(
  FEM
  "Finite Element Method modules (ITKFEM, ITKFEMRegistration), retired from the ITK main tree. UNMAINTAINED, scheduled for removal in ITK 7. See InsightSoftwareConsortium/ITK#6511."
  MODULE_COMPLIANCE_LEVEL 1
  GIT_REPOSITORY https://github.com/InsightSoftwareConsortium/ITKFEM.git
  GIT_TAG e460bc7fb2988a29aa366e1ae33790dca70413c1
)

if(Module_FEM)
  message(
    WARNING
    "The FEM remote module (ITKFEM / ITKFEMRegistration) is UNMAINTAINED and "
    "scheduled for REMOVAL in ITK 7. It was a grant-funded ITKv4 research "
    "deliverable with no funded upkeep and no known downstream users. If your "
    "project depends on FEM, please comment on "
    "https://github.com/InsightSoftwareConsortium/ITK/issues/6511 so it can be "
    "preserved."
  )
endif()
