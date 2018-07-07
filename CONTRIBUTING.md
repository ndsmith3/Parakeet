# Contributing

## Feature Additions

If there is a potential feature to be added to Parakeet, complete the following steps:

 * Fork the project
 * Create a branch for changes in personal repository
 * Commit feature changes in the feature branch
 * Add unit tests for the new feature
 * Ensure **ALL** tests pass
 * Run `scalafmt` in the project's root directory
  * Ensure that the `.scalafmt.conf` found in the project's root directory is being used
 * Update CHANGELOG.md with a 'release note' style log
 * Create pull request against `master`

Your pull request will be reviewed by a code maintainer (currently only @ndsmith3). Failure to complete **any** of the steps will result in a denial until the issue has been corrected by the feature creator.

## Bug Fixes

If you find a bug in Parakeet, please report it as an issue. If you would like to fix said bug, or another bug previously logged, complete the following steps:

 * Create a branch for the fix off of `master`
 * Commit changes to said branch
 * Add unit tests should your fix warrant them
 * Ensure **ALL** tests pass
 * Run `scalafmt` in the project's root directory
  * Ensure that the `.scalafmt.conf` found in the project's root directory is being used
 * Update CHANGELOG.md with a 'release note' style log
 * Create pull request against `master`

Your pull request will be reviewed by a code maintainer (currently only @ndsmith3). Failure to complete **any** of the steps will result in a denial until the issue has been corrected by the bug fixer. The bug fixer doesn't need to worry about the logged issue, as it will be handled by the code maintainers.
