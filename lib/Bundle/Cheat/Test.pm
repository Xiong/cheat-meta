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

use Test::More tests => 6;      # Standard framework for writing test scripts
    ok  ( $bool, $name );               # ok if $bool is true
    is  ( $got, $want, $name );         # ok if $got eq $want
    isnt( $got, $want, $name );         # ok if $got ne $want
    like( $got, qr/./, $name );         # ok if $got =~ qr/regex/
  unlike( $got, qr/./, $name );         # ok if $got !~ qr/regex/
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

=head1 MODULES

=over

=item Test::Simple

=item Test::More

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
