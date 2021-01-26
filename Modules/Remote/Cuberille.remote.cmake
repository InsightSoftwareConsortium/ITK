#-- # Grading Level Criteria Report
#-- EVALUATION DATE: 2020-03-24
#-- EVALUATORS: [Dženan Zukić, Davis Vigneault]
#--
#-- ## Compliance level 5 star (AKA ITK main modules, or remote modules that could become core modules)
#--   - [ ] Widespread community dependance
#--   - [X] Above 90% code coverage
#--   - [ ] CI dashboards and testing monitored rigorously
#--   - [X] Key API features are exposed in wrapping interface
#--   - [ ] All requirements of Levels 4,3,2,1
#--
#-- ## Compliance Level 4 star (Very high-quality code, perhaps small community dependance)
#--   - [X] Meets all ITK code style standards
#--   - [X] No external requirements beyond those needed by ITK proper
#--   - [ ] Builds and passes tests on all supported platforms within 1 month of each core tagged release
#--            - [ ] Windows Shared Library Build with Visual Studio
#--            - [ ] Mac with clang compiller
#--            - [ ] Linux with gcc compiler
#--   - [X] Active developer community dedicated to maintaining code-base
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
#-- ## Compliance Level 2 star (Alpha code feature API development or niche community/exectution environment dependance )
#--   - [X] Compiles for at least 1 niche set of execution envirionments, and perhaps others
#--         (may depend on specific external tools like a java environment, or specific external libraries to work )
#--   - [X] All requirements of Levels 1
#--
#-- ## Compliance Level 1 star (Pre-alpha features under development and code of unkown quality)
#--   - [X] Code complies on at least 1 platform
#--
#-- ## Compliance Level 0 star ( Code/Feature of known poor-quality or deprecated status )
#--   - [ ] Code reviewed and explicitly identified as not recommended for use
#--
#-- ### Please document here any justification for the criteria above
#       Code style enforced by clang-format on 2020-02-19, and clang-tidy modernizations completed
#       2020-05-25 Continuous integration and clang format linting now performed by Github Actions
#       2020-05-25 Line coverage is 99.8%, function coverage is 98.0%


# Contact: Matt McCormick <matt.mccormick@kitware.com>
itk_fetch_module(Cuberille
"This module implements cuberille implicit surface
polygonization for ITK. This method operates by diving the surface into a
number of small cubes called cuberilles. Each cuberille is centered at a
pixel lying on the iso-surface and then quadrilaterals are generated for each
face. The original approach is improved by projecting the vertices of each
cuberille onto the implicit surface, smoothing the typical block-like
resultant mesh.

A more detailed description can be found in the Insight Journal article:

  Mueller, D. \"Cuberille Implicit Surface Polygonization for ITK\"
  https://www.insight-journal.org/browse/publication/740
  July-December, 2010.

And the related:

  Mueller, D. \"Fast Marching Minimal Path Extraction in ITK\"
  https://www.insight-journal.org/browse/publication/213
  January-June, 2008.
"
  MODULE_COMPLIANCE_LEVEL 3
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKCuberille.git
  GIT_TAG aaa9fb1332f1b5025e5bcf365967e04f8a636e5b
  )
