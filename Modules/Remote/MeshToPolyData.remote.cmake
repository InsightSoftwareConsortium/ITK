#-- # Grading Level Criteria Report
#-- EVALUATION DATE: 2022-03-08
#-- EVALUATORS: [Matt McCormick
#--
#-- ## Compliance level 5 star (AKA ITK main modules, or remote modules that could become core modules)
#--   - [ ] Widespread community dependance
#--   - [X] Above 90% code coverage
#--   - [X] CI dashboards and testing monitored rigorously
#--   - [X] Key API features are exposed in wrapping interface
#--   - [ ] All requirements of Levels 4,3,2,1
#--
#-- ## Compliance Level 4 star (Very high-quality code, perhaps small community dependance)
#--   - [X] Meets all ITK code style standards
#--   - [X] No external requirements beyond those needed by ITK proper
#--   - [X] Builds and passes tests on all supported platforms within 1 month of each core tagged release
#--            - [X] Windows Shared Library Build with Visual Studio
#--            - [X] Mac with clang compiller
#--            - [X] Linux with gcc compiler
#--   - [x] Active developer community dedicated to maintaining code-base
#--   - [X] 75% code coverage demonstrated for testing suite
#--   - [X] Continuous integration testing performed
#--   - [X] All requirements of Levels 3,2,1
#--
#-- ## Compliance Level 3 star (Quality beta code)
#--   - [X] API | executable interface is considered mostly stable and feature complete
#--   - [X] 10% C0-code coverage demonstrated for testing suite
#--   - [X] Some tests exist and pass on at least some platform
#--   - [X] All requirements of Levels 2,1
#--
#-- ## Compliance Level 2 star (Alpha code feature API development or niche community/execution environment dependance )
#--   - [X] Compiles for at least 1 niche set of execution envirionments, and perhaps others
#--         (may depend on specific external tools like a java environment, or specific external libraries to work )
#--   - [X] All requirements of Levels 1
#--
#-- ## Compliance Level 1 star (Pre-alpha features under development and code of unknown quality)
#--   - [X] Code complies on at least 1 platform
#--
#-- ## Compliance Level 0 star ( Code/Feature of known poor-quality or deprecated status )
#--   - [ ] Code reviewed and explicitly identified as not recommended for use
#--
#-- ### Please document here any justification for the criteria above
#       Code style enforced by clang-format on 2020-02-19, and clang-tidy modernizations completed

# Contact: Matt McCormick <matt.mccormick@kitware.com>
itk_fetch_module(MeshToPolyData
"Convert an ITK Mesh to a data structure compatible with vtkPolyData."
  MODULE_COMPLIANCE_LEVEL 3
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKMeshToPolyData.git
  GIT_TAG f2140a34bec87aecae2f64937d4c1172d4c1763d
  )
