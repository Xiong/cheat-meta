package Bundle::Cheat::Test;

BEGIN {
    die 'OK';
}

## END MODULE
return 1;   # Abort execution; fool syntax highlighting for following code.
die 'Failed to abort. This line should never execute; please contact author.';

#============================================================================#
#=========# TEST MODULES
                                # [<-44 cols to end                78 cols ->]
                                # #2345678901234567890123456789012345678901234

use Test::Simple tests => 6;    # Basic utilities for writing tests
    ok( $bool, $name );             # ok if $bool is true
    ok( $foo eq $bar, $name );      # ok if $foo eq $bar
## Test::Simple

use Test::More tests => 6;      # Standard framework for writing test scripts
    ok  ( $bool, $name );               # ok if $bool is true
    is  ( $got, $want, $name );         # ok if $got eq $want
    isnt( $got, $want, $name );         # ok if $got ne $want
    like( $got, qr/./, $name );         # ok if $got =~ /regex/
  unlike( $got, qr/./, $name );         # ok if $got !~ /regex/
  cmp_ok( $got, '==', $want, $name );    # ok if $got == $want
  my $object = new_ok( $class => \@args );  # calls $class->new(@args)
  can_ok( $object, @methods );              # ...or: can_ok($module...
  isa_ok( $object, $class, $object_name);   # safe if $object is undef
    subtest $name => \&code;            # version 0.94
    pass( $name);                   # unconditional ok
    fail( $name);                   # unconditional failure
    BEGIN { use_ok($module, @imports); }    # ok if find, load, import
    require_ok($file);                      # ok if find and load
    is_deeply( $got, $want, $name );    # walks deeply but doesn't check bless
use Test::More;                     # declare number of tests later
    plan tests => $calculated;      # calculate plan at run time
    done_testing($counter);         # or after testing
    diag(@message);             # will print but won't mess up test harness
    note(@message);             # will only print if verbose output is asked
    ok($bool) or diag(@message);    # passage or failure propagates
    my @dump = explain( @refs );            # uses Data::Dumper
    my @dump = explain( \@array, \%hash );  #   to dump list of references
    BAIL_OUT( $reason );        # abort this and all following test scripts
## Test::More

use Test::Deep;                 # Extremely flexible deep comparison
    cmp_deeply( $got, $want, $name );   # ok if $got eq $want deeply
    # Special comparision functions for each value; may be nested
    my $cmp = {                     # check each $got->{key}        # $gv...
        key     => ignore(),            # ok regardless of $gv
        key     => 'literal',           # ok if $gv eq 'literal'
        key     => re('regex'),         # ok if $gv =~ /regex/
        key     => bag(@want),          # ignore ordering of elements
        key     => set(@want),          # ignore ord.of and duplicate elements
        key     => superbagof(@want),   # $gv contains at least this bag
        key     => subbagof  (@want),   # $gv contains at most  this bag
        key     => supersetof(@want),   # $gv contains at least this in order
        key     => subsetof  (@want),   # $gv contains at most  this in order
        key     => all(@want),          # ok if $gv eq all @want (and)
        key     => any(@want),          # ok if $gv eq any @want (or)
        key     => array_each($cmp2),   # check each @{$gv} against $cmp2
        key     => str ($want),         # stringify $gv eq $want
        key     => num ($want, $tolc),  # numify    $gv == $want +/- $tolc
        key     => bool($want),         # ok if ( !!$gv == !!$want )
        key     => code($cref),         # $c = sub( $gv = shift; return $ok );
        key     => isa($class),         # ok if $gv->UNIVERSAL::isa($class)
        key     => methods(             # invoke methods of $gv
            method => $want,            # ok if $gv->method()      eq $want
          [ method, @args ] => $want,   # ok if $gv->method(@args) eq $want
        ),
    };
    cmp_deeply( $got, $cmp,  $name ); # ok if $got special $cmp deeply
## Test::Deep

use Test::Trap;                 # Trap exit codes, exceptions, output, etc.
# $trap object is exported into your namespace and contains everything.
# Methods can be combined in a large variety of ways; see Test::Trap POD.
use Test::Trap  qw( :raw :die :exit 
                        :flow 
                    :stdout             :stderr 
                    :stdout(perlio)     :stderr(perlio)
                    :stdout(tempfile)   :stderr(tempfile)
                    :stdout(method)     :stderr(method)
                    :stdout( m1, m2, m3 )
                    :warn
                        :default
                    :on_fail(method)
                    :void       :scalar     :list
                    :output(systemsafe)     :output(method)
);
use Test::Trap(     # order of layers in the use-array is significant
        ':raw',                 # traps normal return and stops trapping
        ':die',                 # traps fatal exceptions
        ':exit',                # traps attempts to exit() perl
    ':flow',                # shortcut for :raw:die:exit
        ':stdout',              # trap STDOUT
        ':stderr',              # trap STDERR
            ':stdout(perlio)',      # trap using PerlIO::scalar
            ':stdout(tempfile)',    # trap using File::Temp to a tempfile
            ':stdout(method)',      # trap using some user method
            ':stdout(m1,m2,m3)',    # provide a list of fallback methods
        ':warn',                # trap warnings and tee them to STDERR
    ':default',             # shortcut for :raw:die:exit:stdout:stderr:warn
        ':on_fail(method)',     # user method to callback on fatals
    ':void',                # return in scalar context
    ':scalar',              # return in list   context
    ':list',                # return in void   context
        ':output(systemsafe)',  # traps children including system calls
        ':output(method)',      # fallback processing with user method
);
    # If you want the normal return value from code under test, 
    #       use Test::Trap qw( :scalar );
    #       use Test::Trap qw( :list );
    #   or  provide a context yourself with:
    my $rv  = trap{  };     # return in scalar context
    my @rvs = trap{  };     # return in list   context
    trap{                   # return in void   context
        # Your code under test here
    };
    $trap->diag_all;                    # Dumps the $trap object, TAP safe
    # Accessor methods  # ACC
    my $got = $trap->leaveby;           # 'return', 'die', or 'exit'. 
    my $got = $trap->die;               # exception thrown if any 
    my $got = $trap->exit;              # exit code caught if attempted
    my $got = $trap->stdout;            # STDOUT in one string
    my $got = $trap->stderr;            # STDERR in one string
    my $got = $trap->return;            # arrayref of normal return values
    my $got = $trap->return($index);    # pass $index as method argument
    my $got = $trap->return(@indices);  # it slices, it dices
    my $got = $trap->warn  ($index);    # warnings as an array
    # Test methods  (for any ACC)
    #   e.g.:       $trap->return_ok();   $trap->stdout_like();
    #   $ix or @ixs required if ACC is array, otherwise omit
    use Test::More tests => 9;                      # ~~ Test::More::*
    $trap->ACC_ok       ( $ix, $name );             #              ok()
    $trap->ACC_nok      ( $ix, $name );             #       !      ok()
    $trap->ACC_is       ( $ix, $want,  $name );     #              is()
    $trap->ACC_isnt     ( $ix, $want,  $name );     #       !      is()
    $trap->ACC_like     ( $ix, qr/./,  $name );     #            like()
    $trap->ACC_unlike   ( $ix, qr/./,  $name );     #       !    like()
    $trap->ACC_isa_ok   ( $ix, $class, $name );     #             isa()
    $trap->ACC_is_deeply( $ix, $want,  $name );     #       is_deeply()
    # Examples of above: 
    $trap->return_ok( 0, 'got something' ); # even if :scalar, return => []
    $trap->return_is( 1, 9,  'returns an array and the second element is 9' );
    $trap->stdout_like( qr/hell|damn/, 'tried to print a word to screen'); 
    $trap->die_unlike ( qr/hell|damn/, 'died like a lady');  # fail if !die
    $trap->return_isa_ok ( 0, 'Acme::Teddy' 'returned object');
    # Convenience methods with better diagnostics than their equivalents...
    $trap->did_die;         # $trap->leaveby_is('die');
    $trap->did_exit;        # $trap->leaveby_is('exit');
    $trap->did_return;      # $trap->leaveby_is('return');
    $trap->quiet;           # ok( !$trap->stdout && !$trap-stderr);
## Test::Trap

#TODO: use Test::Exception tests => 6; # Test exception based code




#============================================================================#
__END__

=head1 NAME

Bundle::Cheat::Test - Usage for Test::* modules

=head1 SYNOPSIS

    $ cat Bundle/Cheat/Test.pm

=head1 DESCRIPTION

I<The test itself is a cheat, isn't it?> 
I<I mean you program it to be unwinnable.> 
--James Tiberius Kirk

This is a collection of "cheat sheets": highly compressed, abbreviated 
documentation for various modules. Each module within the bundle covers a 
top-level namespace or a set of otherwise closely-related modules. 

For each module, a paragraph is given, generally: 

    Some::Module            # Short description
        qw( various exportable symbols if any );
        routine( $placeholder, @arguments );
        $context    = function( @arguments);
        $object->method();

You should be able to copy and paste this into your own code, 
delete what you don't need, and be on your way. 

=head1 CHEATS

=over

=item * L<Test::Simple>

=item * L<Test::More>

=item * L<Test::Deep>

=item * L<Test::Trap>

=back

=head1 SEE ALSO

=over

=item * L<perlcheat>

=item * L<Bundle::Cheat>

=back

=head1 FUNCTIONS/METHODS

None. This module does nothing at all. 
You shouldn't even try to C<use()> it;
if you try, it will C<die()>.

If it didn't C<die()> immediately, in an early C<BEGIN> block, 
then it would try to C<use()> every single module it cheats. 
So we don't do that. 

=head1 INTERFACE 

Open the module (the *.pm file itself) in your editor. 
Copy out whatever you like. 

=head1 DIAGNOSTICS

=over

=item Failed to abort. This line should never execute; please contact author.

This module contains executable code so that your editor will use
its syntax highlighting feature, if any. But it would be madness to execute it
all. You should never, ever see this error. 

=back

=head1 CONFIGURATION AND ENVIRONMENT

Bundle::Cheat::Test requires no configuration files or environment variables.

=head1 DEPENDENCIES

No dependencies. 

=head1 INCOMPATIBILITIES

None.

=head1 BUGS AND LIMITATIONS

No cheat sheet will teach you anything. It's only a reminder. You B<must> 
consult each module's own full documentation I<at least> before using it. 
I hope. 

This module does not contain magic to squirt code into your module. 
Copy and paste. 

=head1 THANKS

=over

=item *

To about 8500 authors who have uploaded about 85,000 modules to the CPAN. 

=back

=head1 AUTHOR

Xiong Changnian  C<< <xiong@cpan.org> >>

=head1 LICENSE

Copyright (C) 2010 Xiong Changnian C<< <xiong@cpan.org> >>

This library and its contents are released under Artistic License 2.0:

L<http://www.opensource.org/licenses/artistic-license-2.0.php>

=cut
