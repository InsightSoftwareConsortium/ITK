#-- # Grading Level Criteria Report
#-- EVALUATION DATE: 2022-11-09
#-- EVALUATORS: [<<NO EVALUATOR>>,<<NO EVALUATOR>>]
#--
#-- ## Compliance level 5 star (AKA ITK main modules, or remote modules that could become core modules)
#--   - [ ] Widespread community dependance
#--   - [ ] Above 90% code coverage
#--   - [ ] CI dashboards and testing monitored rigorously
#--   - [ ] Key API features are exposed in wrapping interface
#--   - [ ] All requirements of Levels 4,3,2,1
#--
#-- ## Compliance Level 4 star (Very high-quality code, perhaps small community dependance)
#--   - [ ] Meets all ITK code style standards
#--   - [ ] No external requirements beyond those needed by ITK proper
#--   - [ ] Builds and passes tests on all supported platforms within 1 month of each core tagged release
#--            - [ ] Windows Shared Library Build with Visual Studio
#--            - [ ] Mac with clang compiller
#--            - [ ] Linux with gcc compiler
#--   - [ ] Active developer community dedicated to maintaining code-base
#--   - [ ] 75% code coverage demonstrated for testing suite
#--   - [ ] Continuous integration testing performed
#--   - [ ] All requirements of Levels 3,2,1
#--
#-- ## Compliance Level 3 star (Quality beta code)
#--   - [ ] API | executable interface is considered mostly stable and feature complete
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

itk_fetch_module(FPFH
"An ITK-based implementation of FPFH (Fast Point Feature Histogram) used for point cloud feature calculation.
The feature points could be used obtain salient points while performing registration using RANSAC remote module.

The class PointFeature is the main driver that takes a PointSet as argument.
Please refer to the documentation upstream for a detailed description and sample usage:
https://github.com/InsightSoftwareConsortium/ITKFPFH
"
  MODULE_COMPLIANCE_LEVEL 2
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKFPFH.git
  GIT_TAG e1f323fa173a83b23ad53ac491de80337e9b7f29
)
