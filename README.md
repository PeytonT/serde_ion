# serde_ion
Rust implementation of serialization and deserialization of the Ion data format.

This crate is an incomplete work in progress. 

http://amzn.github.io/ion-docs/

http://amzn.github.io/ion-docs/docs/binary.html

## Testing

This repository contains a git submodule called ion-tests (located at tests/ion-tests) containing Ion's [compatibility test suite repository](https://github.com/amzn/ion-tests).

The submodule is required to run the test suite. It can be initialized from within the repository with the following commands.

```
$ git submodule init
$ git submodule update
```
