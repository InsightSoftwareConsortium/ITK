# Introduction

This directory is a place-holder for all remote modules distributed outside
ITK's main repository. Remote modules share the same directory structure as
modules in the main repository. They will be picked up by the configuration
system and entered into the build after they are downloaded into this
directory.

Modules can easily be downloaded and made accessible to the community by
listing the module in the current directory. For more information on the
policy and procedures for adding a new module, please visit the
[Contributing with a Remote Module](https://itk.org/ITKSoftwareGuide/html/Book1/ITKSoftwareGuide-Book1ch9.html#x55-1640009.7)  in the [ITK Software Guide].

For more information on adding a new module to the list of new modules,
please visit the [ITKModuleTemplate](https://github.com/InsightSoftwareConsortium/ITKModuleTemplate) repository.

Note that in each `<remote module name>.remote.cmake` file, the first argument
of the function `itk_fetch_module()` is the name of the remote module, and it
has to be consistent with the module name defined in the correponding
`<remote module name>.remote.cmake` file.

To better distinguish the remote modules from the internal ITK modules, the names
of the remote modules should **not** contain the "ITK" string prefix in them.


[ITK Software Guide]: https://itk.org/ItkSoftwareGuide.pdf


# Compliance Level guide

Given that remote modules have a variety of maturity levels, a rating system
is available to assist with communicating the quality and maturity level
of each remote module.

## Background

One of the primary goals of creating remote modules was to ease the burden of
core developers in maintaining the core ITK code base.  There are currently (2020-02-21)
44 remote modules distributed with ITK.

## Goals of the Compliance Level gradings

 * Provide a grading/ranking system for the remote modules to better convey the compliance level for which the module passes.
 * Provide cmake filtering to hide modules of lower-class quality
 * Provide ITK core developers a test-bed in Compliance levels 5,4,3 for ensuring backward compatibility testing, external tool support, and identifying migration guide support
 * Provide ITK core developers with indications of which remote modules (2,1,0) that have not reached a level of maturity which demands high levels of efforts to ensure that they continue to work with the latest ITK developments (Perhaps they have surpassed their useful lifespan, or been replaced with other mechanisms).

## Documenting the Compliance Level of a remote module
Use the following template to document the compliance level of the modules.
```
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
#--   - [X] Meets all ITK code style standards
#--   - [X] No external requirements beyond those needed by ITK proper
#--   - [X] Builds and passes tests on all supported platforms within 1 month of each core tagged release
#--            - [ ] Windows Shared Library Build with Visual Studio
#--            - [X] Mac with clang compiller
#--            - [X] Linux with gcc compiler
#--   - [X] Active developer community dedicated to maintaining code-base
#--   - [X] 75% code coverage demonstrated for testing suite
#--   - [ ] Continuous integration testing performed
#--   - [ ] All requirements of Levels 3,2,1
#--
#-- ## Compliance Level 3 star (Quality beta code)
#--   - [X] API | executable interface is considered mostly stable and feature complete
#--   - [ ] 10% C0-code coverage demonstrated for testing suite
#--   - [X] Some tests exist and pass on at least some platform
#--   - [ ] All requirements of Levels 2,1
#--
#-- ## Compliance Level 2 star (Alpha code feature API development or niche community/exectution environment dependance )
#--   - [X] Compiles for at least 1 niche set of execution envirionments, and perhaps others
#--         (may depend on specific external tools like a java environment, or specific external libraries to work )
#--   - [ ] All requirements of Levels 1
#--
#-- ## Compliance Level 1 star (Pre-alpha features under development and code of unkown quality)
#--   - [X] Code complies on at least 1 platform
#--
#-- ## Compliance Level 0 star ( Code/Feature of known poor-quality or deprecated status )
#--   - [ ] Code reviewed and explicitly identified as not recommended for use
#--
#-- ### Please document here any justification for the criteria above
#       Code style enforced by clang-format on 2020-02-19, and clang-tidy modernizations completed.
#       On 2020-02-15 manual evaluation of code coverage for running the supplied test resulted in 77% code coverage
#       Tests manually confirmed to pass for Windows VS 19, Mac 10.14, and Ubuntu Linux gcc 7 compilers
#       CI integration is not yet working as of 2020-02-19, so level 4 compliance not yet achived.

itk_fetch_module(MyFavRemoteModule
  "The best image processing algorithm in ITK. From Insight Journal article with handle: https://hdl.handle.net/1234/567"
  MODULE_COMPLIANCE_LEVEL 3
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/MyFavRemoteModule.git
  GIT_TAG 9988d866433896368bd3049c396b974433b22ccd
  )
```
