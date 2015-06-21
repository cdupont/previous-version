Previous Version
================

Previous Version is a simple Haskell tool that allow to retrieve several versions of the same module from git.
File and module names are annotated using there version tag. For example "MyModule.hs" becomes "MyModuleV1.hs".

Use case
========

Previous Version is used for "continuous data integration".


Implementation
==============

Use Template Haskell to retrieve previous versions?



- extract a certain number of version of a file from GIT and put them in target/autogen
- rename them to tag them with their version number
- alias the last one with "PreviousVersion"
 
