#-- # Grading Level Criteria Report
#-- EVALUATION DATE: 2020-03-01
#-- EVALUATORS: [<<NO EVALUATOR>>,<<NO EVALUATOR>>]
#--
#-- ## Compliance level 5 star (AKA ITK main modules, or remote modules that could become core modules)
#--   - [ ] Widespread community dependance
#--   - [ ] Above 90% code coverage
#--   - [ ] CI dashboards and testing monitored rigorously
#--   - [ ] All requirements of Levels 4,3,2,1
#--
#-- ## Compliance Level 4 star (Very high-quality code, perhaps small community dependance)
#--   - [ ] Meets all ITK code style standards
#--   - [ ] No external requirements beyond those needed by ITK proper
#--   - [ ] Builds and passes tests on all supported platforms within 1 month of each core tagged release
#--   - [ ] Active developer community dedicated to maintaining code-base
#--   - [ ] 75% code coverage demonstrated for testing suite
#--   - [ ] Continuous integration testing performed
#--   - [ ] All requirements of Levels 3,2,1
#--
#-- ## Compliance Level 3 star (Quality beta code)
#--   - [ ] API | executable interface is considered mostly stable and feature complete
#--   - [ ] Some tests exist and pass on at least some platform
#--   - [ ] All requirements of Levels 2,1
#--
#-- ## Compliance Level 2 star (Alpha code feature API development or niche community/exectution environment dependance )
#--   - [ ] Compiles for at least 1 niche set of execution envirionments, and perhaps others
#--         (may depend on specific external tools like a java environment, or specific external libraries to work )
#--   - [ ] All requirements of Levels 1
#--
#-- ## Compliance Level 1 star (Pre-alpha features under development and code of unkown quality)
#--   - [ ] Code complies on at least 1 platform
#--
#-- ## Compliance Level 0 star ( Code/Feature of known poor-quality or deprecated status )
#--   - [X] Code reviewed and explicitly identified as not recommended for use
#--
#-- ### Please document here any justification for the criteria above
#       This outdated code is not recommended for use. It is not optimized or rigourously tested
#       This module has not recieved updates or testing for many many years
#       Core developers should *NOT* expend energy or resources keeping this codebase consistent with ITK

if(NOT ITK_LEGACY_REMOVE AND ITKV4_COMPATIBILITY AND Module_ITKDeprecated)

itk_fetch_module(NeuralNetworks
"This deprecated remote module contains classes and support classes
for the calculation of artificial neural networks.

This can be used, for instance, for image classification.

This historical set of features is likely not appropriate for modern neural
network implementations due to performance issues."

  MODULE_COMPLIANCE_LEVEL 0
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKNeuralNetworks.git
  GIT_TAG c293e56699d6d102dcde567baf1cf5b704819c17
  )

  if(NOT ITK_LEGACY_SILENT AND Module_NeuralNetworks)
    message(WARNING "NeuralNetworks remote module is deprecated.
    Development of this module has ended, and it will be removed
    in future ITK versions.")
  endif()
endif()
