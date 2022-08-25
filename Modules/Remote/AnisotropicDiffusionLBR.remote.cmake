#-- # Grading Level Criteria Report
#-- EVALUATION DATE: 2020-03-01
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
#--   - [ ] 10% C0-code coverage demonstrated for testing suite
#--   - [ ] Some tests exist and pass on at least some platform
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

itk_fetch_module(AnisotropicDiffusionLBR
  "Anisotropic Non-Linear Diffusion is a powerful image processing technique,
  which allows to simultaneously remove the noise and enhance sharp features
  in two or three dimensional images. Anisotropic Diffusion is understood here
  in the sense of Weickert, meaning that diffusion tensors are anisotropic and
  reflect the local orientation of image features. This is in contrast with
  the non-linear diffusion filter of Perona and Malik, which only involves
  scalar diffusion coefficients, in other words isotropic diffusion tensors.

  In this module, an anisotropic non-linear diffusion technique based on a recent
  adaptive scheme making the diffusion stable and requiring limited numerical resources
  is available.

  From the Insight Journal Article:

    \"Anisotropic Diffusion in ITK\"
    Mirebeau J., Fehrenbach J., Risser L., Tobji S.
    The Insight Journal. 2014 January-December.
    https://insight-journal.org/browse/publication/953
  "
  MODULE_COMPLIANCE_LEVEL 2
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKAnisotropicDiffusionLBR.git
  GIT_TAG d46aab0e0c8f02b89eaa0420e61970559177683f
  )
