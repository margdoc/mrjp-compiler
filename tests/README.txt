# Tests

This directory containts tests in format `[test_name].lat`. If file `[test_name].output` exists, code inside `[test_name].lat` should compile succesfully (and outputs "OK" message on stdout) and after executing stdout should have the same output as `[test_name].output` file. If such a file doesn't exist, code shouldn't compile and return meaningfull error message.

## Tests directories

### official
Tests provided by lecturer.

### mrjp-tests
These tests were written by some students from previous editions of this course (some cases may differ, since task specification can be slightly changed each year).

### margdoc
These tests are written by me. It contains cases not covered by the other two tests sets.

## Usage
```./test.sh <file> [-qu] [--official|--students|--margdoc|--all]```
Where
* `<file>` - path to the compiler binary
* `-q` - run in quiet mode
* `-u` - run uncertain tests (along with other tests)
* `--official` - run only official's tests
* `--students` - run only students's tests
* `--margdoc` - run only margdoc's tests
* `--all` - run all tests (default)
