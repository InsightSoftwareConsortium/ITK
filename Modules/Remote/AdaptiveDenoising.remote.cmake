#-- # Grading Level Criteria Report
#-- EVALUATION DATE:
#-- EVALUATORS:
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
#--   - [ ] 10% C0-code coverage demonstrated for testing suite
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
#--   - [ ] Code reviewed and explicitly identified as not recommended for use
#--
#-- ### Please document here any justification for the criteria above
#       Code style enforced by clang-format on 2020-02-19, and clang-tidy modernizations completed

itk_fetch_module(AdaptiveDenoising
  "A spatially adaptive denoising image filter using non-local means.
  A patch-based framework for new ITK functionality: Joint fusion, denoising,
  and non-local super-resolution.
  Tustison N., Avants B., Wang H., Xie L., Coupe P., Yushkevich P., Manjon J.
  Insight Journal.
  http://insight-journal.org/browse/publication/982.
  Two Luis Miguel fans walk into a bar in Nagoya ---> (yada, yada, yada)
  ---> an ITK-implementation of a popular patch-based denoising filter.
  Tustison N., Manjon J.V.
  Insight Journal.
  http://insight-journal.org/browse/publication/979.
  "
  MODULE_COMPLIANCE_LEVEL
  GIT_REPOSITORY ${git_protocol}://github.com/ntustison/ITKAdaptiveDenoising.git
  GIT_TAG ef309a1f050a2f7b1102f5024cf4997cd284266b
)
