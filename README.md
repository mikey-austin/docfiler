# Docfiler

Save GPG encrypted files to a filesystem.

## Running the tests

The tests all emit TAP (https://testanything.org) so any TAP test harness will work. To use the
perl test harness make sure that the Test::Harness perl module is installed. It can then be
run with the following command:

    $ prove -e 'guile2.2 -L .' test/*.scm
    test/fs_test.scm ... ok   
    test/tap_test.scm .. ok   
    All tests successful.
    Files=2, Tests=6,  0 wallclock secs ( 0.06 usr  0.01 sys +  0.08 cusr  0.02 csys =  0.17 CPU)
    Result: PASS

In the future this project will use autotools and will use it's tap harness to run tests as the
recommended way.
